-- | TOA v1 address derivation.
--
-- Given a 'TOAParamsV1', produce the Cardano enterprise script address
-- (no stake credential) per the CIP @Address derivation algorithm@.
-- Header byte is @0x71@ on mainnet, @0x70@ on any testnet.
module TxBuilding.Toa.Address
  ( toaAddress
  ) where

import GeniusYield.Types (GYAddress, GYNetworkId, addressFromScriptHash)
import Onchain.Protocol.Types (TOAParamsV1)
import TxBuilding.Toa.Validator (toaV1ScriptHash)

-- | Derive the TOA enterprise address on the given network.
toaAddress :: GYNetworkId -> TOAParamsV1 -> GYAddress
toaAddress nid p = addressFromScriptHash nid (toaV1ScriptHash p)
