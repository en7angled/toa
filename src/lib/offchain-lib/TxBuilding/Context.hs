-- | Atlas provider context and shared tx-building runners for TOA.
--
-- TOA has no singleton-deployed validators (the parametric @ToaV1@ validator
-- is inlined per spend tx via 'TxBuilding.Toa.Skeletons.spendFromToaInlined'),
-- so we drop DBS's @DeployedScriptsContext@ \/ @TxBuildingContext@ pair and
-- pass 'ProviderCtx' directly. See the architecture rules for the carve-out:
-- @.cursor\/rules\/architecture\/offchain-rules.mdc@.
module TxBuilding.Context
  ( ProviderCtx (..),
    getNetworkId,
    runQuery,
    runTx',
    collateralToRunParam,
    submitTx,
  )
where

import GeniusYield.GYConfig (GYCoreConfig, cfgNetworkId)
import GeniusYield.TxBuilder
  ( GYTxBuilderMonadIO,
    GYTxQueryMonadIO,
    GYTxSkeleton,
    buildTxBody,
    runGYTxBuilderMonadIO,
    runGYTxQueryMonadIO,
  )
import GeniusYield.Types

-- | Atlas provider configuration + resolved providers, threaded through every
-- handler that needs a chain query, tx build, or submission.
data ProviderCtx = ProviderCtx
  { ctxCoreCfg :: !GYCoreConfig,
    ctxProviders :: !GYProviders
  }

getNetworkId :: ProviderCtx -> GYNetworkId
getNetworkId = cfgNetworkId . ctxCoreCfg

-- | Run a read-only Atlas query against the provider.
runQuery :: ProviderCtx -> GYTxQueryMonadIO a -> IO a
runQuery ctx = runGYTxQueryMonadIO (getNetworkId ctx) (ctxProviders ctx)

-- | Convert optional CBOR-wrapped collateral to the parameter expected by
-- 'runGYTxBuilderMonadIO'. @Bool = True@ keeps Atlas's 5-ADA-only check on
-- the reserved collateral UTxO (DBS convention).
collateralToRunParam :: Maybe GYTxOutRefCbor -> Maybe (GYTxOutRef, Bool)
collateralToRunParam = fmap (\c -> (getTxOutRefHex c, True))

-- | Build a tx body from a 'PlutusV3' skeleton, using the caller-supplied
-- wallet addresses for fee balancing and collateral selection.
runTx' ::
  ProviderCtx ->
  -- | User's used addresses.
  [GYAddress] ->
  -- | User's change address.
  GYAddress ->
  -- | Browser wallet's reserved collateral (if set).
  Maybe GYTxOutRefCbor ->
  GYTxBuilderMonadIO (GYTxSkeleton 'PlutusV3) ->
  IO GYTxBody
runTx' ctx addrs addr collateral skeleton =
  runGYTxBuilderMonadIO
    (getNetworkId ctx)
    (ctxProviders ctx)
    addrs
    addr
    (collateralToRunParam collateral)
    (skeleton >>= buildTxBody)

-- | Submit a signed transaction via the provider, returning its id.
submitTx :: ProviderCtx -> GYTx -> IO GYTxId
submitTx ctx = gySubmitTx (ctxProviders ctx)
