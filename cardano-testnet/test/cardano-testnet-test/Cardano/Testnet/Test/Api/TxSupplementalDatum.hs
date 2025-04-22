{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Testnet.Test.Api.TxSupplementalDatum
  ( hprop_tx_supp_datum
  )
where

import           Cardano.Api
import qualified Cardano.Api.Ledger as L
import qualified Cardano.Api.Network as Net
import qualified Cardano.Api.Network as Net.Tx
import           Cardano.Api.Shelley

import           Cardano.Testnet

import           Prelude

import           Control.Monad
import           Data.Bifunctor (second)
import           Data.Default.Class
import qualified Data.Map.Strict as M
import           Data.Proxy
import           Data.Set (Set)
import           GHC.Exts (IsList (..))
import           GHC.Stack
import           Lens.Micro

import           Testnet.Components.Query
import           Testnet.Property.Util (integrationRetryWorkspace)
import           Testnet.Types

import           Hedgehog
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.TestWatchdog as H

hprop_tx_supp_datum :: Property
hprop_tx_supp_datum = integrationRetryWorkspace 2 "api-tx-supp-dat" $ \tempAbsBasePath' -> H.runWithDefaultWatchdog_ $ do
  conf@Conf{tempAbsPath} <- mkConf tempAbsBasePath'
  let tempAbsPath' = unTmpAbsPath tempAbsPath

  let ceo = ConwayEraOnwardsConway
      beo = convert ceo
      sbe = convert ceo
      eraProxy = proxyToAsType Proxy
      options = def{cardanoNodeEra = AnyShelleyBasedEra sbe}

  tr@TestnetRuntime
    { configurationFile
    , testnetNodes = node0 : _
    , wallets = wallet0@(PaymentKeyInfo _ addrTxt0) : wallet1 : _
    } <-
    cardanoTestnetDefault options def conf

  systemStart <- H.noteShowM $ getStartTime tempAbsPath' tr
  epochStateView <- getEpochStateView configurationFile (nodeSocketPath node0)
  connectionInfo <- node0ConnectionInfo tr
  pparams <- getProtocolParams epochStateView ceo

  -- prepare tx inputs and output address
  H.noteShow_ addrTxt0
  addr0 <- H.nothingFail $ deserialiseAddress (AsAddressInEra eraProxy) addrTxt0

  let (PaymentKeyInfo _ addrTxt1) = wallet1
  H.noteShow_ addrTxt1
  addr1 <- H.nothingFail $ deserialiseAddress (AsAddressInEra eraProxy) addrTxt1

  -- read key witnesses
  [wit0, wit1] :: [ShelleyWitnessSigningKey] <-
    forM [wallet0, wallet1] $ \wallet ->
      H.leftFailM . H.evalIO $
        readFileTextEnvelopeAnyOf
          [FromSomeType (proxyToAsType Proxy) WitnessGenesisUTxOKey]
          (signingKey $ paymentKeyInfoPair wallet)

  -- query node for era history
  epochInfo <-
    (H.leftFail <=< H.leftFailM) . H.evalIO $
      executeLocalStateQueryExpr connectionInfo Net.VolatileTip $
        fmap toLedgerEpochInfo <$> queryEraHistory

  let scriptData1 = unsafeHashableScriptData $ ScriptDataBytes "CAFEBABE"
      scriptData2 = unsafeHashableScriptData $ ScriptDataBytes "DEADBEEF"
      scriptData3 = unsafeHashableScriptData $ ScriptDataBytes "FEEDCOFFEE"
  -- 4e548d257ab5309e4d029426a502e5609f7b0dbd1ac61f696f8373bd2b147e23
  H.noteShow_ $ hashScriptDataBytes scriptData1
  -- 24f56ef6459a29416df2e89d8df944e29591220283f198d39f7873917b8fa7c1
  H.noteShow_ $ hashScriptDataBytes scriptData2
  -- 5e47eaf4f0a604fcc939076f74ce7ed59d1503738973522e4d9cb99db703dcb8
  H.noteShow_ $ hashScriptDataBytes scriptData3
  let txDatum1 =
        TxOutDatumHash
          (convert beo)
          (hashScriptDataBytes scriptData1)
      txDatum2 = TxOutDatumInline beo scriptData2
      txDatum3 = TxOutSupplementalDatum (convert beo) scriptData3

  -- Build a first transaction with txout supplemental data
  tx1Utxo <- do
    txIn <- findLargestUtxoForPaymentKey epochStateView sbe wallet0

    -- prepare txout
    let txOutValue = lovelaceToTxOutValue sbe 100_000_000
        txOuts =
          [ TxOut addr1 txOutValue txDatum1 ReferenceScriptNone
          , TxOut addr1 txOutValue txDatum2 ReferenceScriptNone
          , TxOut addr1 txOutValue txDatum3 ReferenceScriptNone
          ]

        -- build a transaction
        content =
          defaultTxBodyContent sbe
            & setTxIns [(txIn, pure $ KeyWitness KeyWitnessForSpending)]
            & setTxOuts txOuts
            & setTxProtocolParams (pure $ pure pparams)

    utxo <- UTxO <$> findAllUtxos epochStateView sbe

    BalancedTxBody _ txBody@(ShelleyTxBody _ lbody _ (TxBodyScriptData _ (L.TxDats' datums) _) _ _) _ fee <-
      H.leftFail $
        makeTransactionBodyAutoBalance
          sbe
          systemStart
          epochInfo
          pparams
          mempty
          mempty
          mempty
          utxo
          content
          addr0
          Nothing -- keys override
    H.noteShow_ fee

    H.noteShowPretty_ lbody

    let bodyScriptData = fromList . map fromAlonzoData $ M.elems datums :: Set HashableScriptData
    -- TODO: only inline datum gets included here, but should be all of them
    -- TODO what's the actual purpose of TxSupplementalDatum - can we remove it?
    -- TODO adding all datums breaks script integrity hash, might have to manually compute it?
    -- https://github.com/tweag/cooked-validators/blob/9cb80810d982c9eccd3f7710a996d20f944a95ec/src/Cooked/MockChain/GenerateTx/Body.hs#L127
    --
    -- TODO getDataHashBabbageTxOut excludes inline datums - WHY IT HAPPENS ONLY HERE BUT NOT WHEN CALLING CLI?

    -- TODO add scriptData1 when datum can be provided to transaction building
    -- [ scriptData2
    --   , scriptData3
    --   ]
    --   === bodyScriptData

    let tx = signShelleyTransaction sbe txBody [wit0]
    txId <- H.noteShow . getTxId $ getTxBody tx

    H.noteShowPretty_ tx

    submitTx sbe connectionInfo tx

    -- wait till transaction gets included in the block
    _ <- waitForBlocks epochStateView 1

    -- test if it's in UTxO set
    utxo1 <- findAllUtxos epochStateView sbe
    txUtxo <- H.noteShowPretty $ M.filterWithKey (\(TxIn txId' _) _ -> txId == txId') utxo1
    (length txOuts + 1) === length txUtxo

    let chainTxOuts =
          reverse
            . drop 1
            . reverse
            . map snd
            . toList
            $ M.filterWithKey (\(TxIn txId' _) _ -> txId == txId') utxo1

    (toCtxUTxOTxOut <$> txOuts) === chainTxOuts

    pure txUtxo

  do
    [(txIn1, _)] <- pure $ filter (\(_, TxOut _ _ datum _) -> datum == txDatum1) $ toList tx1Utxo
    -- H.noteShowPretty_ tx1Utxo
    [(txIn2, _)] <- pure $ filter (\(_, TxOut _ _ datum _) -> datum == txDatum2) $ toList tx1Utxo

    let scriptData4 = unsafeHashableScriptData $ ScriptDataBytes "C0FFEE"
        txDatum = TxOutDatumInline beo scriptData4
        txOutValue = lovelaceToTxOutValue sbe 99_999_500
        txOut = TxOut addr0 txOutValue txDatum ReferenceScriptNone

    let content =
          defaultTxBodyContent sbe
            & setTxIns [(txIn1, pure $ KeyWitness KeyWitnessForSpending)]
            & setTxInsReference (TxInsReference beo [txIn2])
            & setTxFee (TxFeeExplicit sbe 500)
            & setTxOuts [txOut]

    txBody@(ShelleyTxBody _ _ _ (TxBodyScriptData _ (L.TxDats' datums) _) _ _) <-
      H.leftFail $ createTransactionBody sbe content
    let bodyScriptData = fromList . map fromAlonzoData $ M.elems datums :: Set HashableScriptData
    [scriptData1, scriptData2, scriptData3] === bodyScriptData

    let tx = signShelleyTransaction sbe txBody [wit1]
    -- H.noteShowPretty_ tx
    txId <- H.noteShow . getTxId $ getTxBody tx

    submitTx sbe connectionInfo tx

    -- wait till transaction gets included in the block
    _ <- waitForBlocks epochStateView 1

    -- test if it's in UTxO set
    utxo1 <- findAllUtxos epochStateView sbe
    let txUtxo = M.filterWithKey (\(TxIn txId' _) _ -> txId == txId') utxo1
    [toCtxUTxOTxOut txOut] === M.elems txUtxo

  H.failure

submitTx
  :: MonadTest m
  => MonadIO m
  => HasCallStack
  => ShelleyBasedEra era
  -> LocalNodeConnectInfo
  -> Tx era
  -> m ()
submitTx sbe connectionInfo tx =
  withFrozenCallStack $
    H.evalIO (submitTxToNodeLocal connectionInfo (TxInMode sbe tx)) >>= \case
      Net.Tx.SubmitFail reason -> H.noteShowPretty_ reason >> H.failure
      Net.Tx.SubmitSuccess -> H.success
