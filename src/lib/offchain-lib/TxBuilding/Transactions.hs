-- | Top of the off-chain pipeline: 'Interaction' → hex-encoded unsigned tx CBOR.
--
-- Wraps 'TxBuilding.Interactions.interactionToTxSkeleton' with the live
-- 'ProviderCtx'-driven runner 'TxBuilding.Context.runTx''. HTTP handlers
-- call this; CLI tools (if added later) can do the same.
module TxBuilding.Transactions
  ( interactionToHexEncodedCBOR
  ) where

import GeniusYield.Types (txToHex, unsignedTx)
import TxBuilding.Context (ProviderCtx, getNetworkId, runTx')
import TxBuilding.Interactions (Interaction (..), UserAddresses (..), interactionToTxSkeleton)

interactionToHexEncodedCBOR :: ProviderCtx -> Interaction -> IO String
interactionToHexEncodedCBOR ctx i@Interaction {userAddresses = UserAddresses {..}} =
  txToHex . unsignedTx
    <$> runTx' ctx usedAddresses changeAddress reservedCollateral (interactionToTxSkeleton (getNetworkId ctx) i)
