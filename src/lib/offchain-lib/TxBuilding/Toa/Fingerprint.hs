-- | CIP-14 asset fingerprint.
--
-- Bech32-encodes @blake2b_160(policy_id ‖ asset_name)@ with HRP @"asset"@.
-- The result is the canonical \"asset1…\" form used in Cardano explorers
-- and wallets, and is byte-stable across implementations.
--
-- This was previously inlined in @src/exe/toa-gen-vectors/Main.hs@. Lifted
-- here so the HTTP API and the vector generator share a single source.
module TxBuilding.Toa.Fingerprint
  ( cip14Fingerprint
  ) where

import Codec.Binary.Bech32 qualified as Bech32
import Crypto.Hash (Blake2b_160, Digest, hash)
import Data.ByteArray qualified as BA
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Text (Text)
import GeniusYield.Types
  ( GYMintingPolicyId
  , GYTokenName
  , mintingPolicyIdToApi
  , tokenNameToApi
  )
import Utils (rawBytes)

-- | CIP-14 fingerprint of an asset class.
cip14Fingerprint :: GYMintingPolicyId -> GYTokenName -> Text
cip14Fingerprint policyId assetName =
  let payload = policyIdBytes policyId <> assetNameBytes assetName
      d = hash payload :: Digest Blake2b_160
      digest = BS.pack (BA.unpack d)
      hrp = either (error . show) id (Bech32.humanReadablePartFromText "asset")
   in Bech32.encodeLenient hrp (Bech32.dataPartFromBytes digest)

policyIdBytes :: GYMintingPolicyId -> ByteString
policyIdBytes = rawBytes . mintingPolicyIdToApi

assetNameBytes :: GYTokenName -> ByteString
assetNameBytes = rawBytes . tokenNameToApi
