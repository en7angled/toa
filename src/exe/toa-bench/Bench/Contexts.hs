{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Synthetic data-backed V3 'ScriptContext' fixtures used by toa-bench.
--
-- Each scenario builds a fully-populated 'V3.ScriptContext' covering the
-- minimal set of fields the TOA v1 validator inspects:
--
--   * @txInfoInputs@ — the inputs whose value gets summed for T1
--   * @txInfoOutputs@ — outputs summed for T2
--   * @txInfoMint@   — checked for T0
--   * @txInfoReferenceInputs@ — populated for the reference-input attack scenario
--
-- All other 'V3.TxInfo' fields are filled with empty / always-valid defaults.
-- The validator does not read them; their shape is constrained only by what
-- 'PlutusTx.unsafeFromBuiltinData' needs to decode 'V3.ScriptContext' without
-- erroring.
--
-- The scenarios mirror the acceptance-criteria categories at
-- the TOA CIP §"Acceptance criteria". Each one is constructed once at module load and
-- pre-serialised to 'PlutusTx.Data', so the evaluator's per-run cost reflects
-- only the validator's execution, not context construction.
module Bench.Contexts
  ( scenarios
  ) where

import Bench.Eval (Expectation (..), Scenario (..))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Text qualified as T
import Onchain.Protocol.Types (TOAParamsV1 (..))
import PlutusLedgerApi.Data.V3
import PlutusLedgerApi.V1.Data.Interval qualified as Interval
import PlutusLedgerApi.V1.Data.Value (CurrencySymbol (..), Value)
import PlutusLedgerApi.V1.Data.Value qualified as Value
import PlutusLedgerApi.V1.Value qualified as SopValue
import PlutusLedgerApi.V3.Data.MintValue (MintValue, emptyMintValue)
import PlutusLedgerApi.V3.Data.MintValue qualified as MintValue
import PlutusTx.Builtins.Internal qualified as Builtins
import PlutusTx qualified as PTx
import PlutusTx.Builtins (toBuiltin)
import PlutusTx.Data.AssocMap qualified as AssocMap
import PlutusTx.Data.List (List)
import PlutusTx.Data.List qualified as DList

-------------------------------------------------------------------------------
-- Scenario list
-------------------------------------------------------------------------------

-- | Eleven scenarios, covering the validator's positive and negative paths
-- plus value-shape stress cases.
scenarios :: [Scenario]
scenarios =
  [ scenario "S01_positive_baseline"        Pass            s01
  , scenario "S02_no_nft_input"             (FailWith "T1") s02
  , scenario "S03_two_nft_inputs"           (FailWith "T1") s03
  , scenario "S04_two_nft_outputs"          (FailWith "T2") s04
  , scenario "S05_mint_nft"                 (FailWith "T0") s05
  , scenario "S06_burn_nft"                 (FailWith "T0") s06
  , scenario "S07_reference_input_only"     (FailWith "T1") s07
  , scenario "S08_multi_toa_inputs_one_nft" Pass            s08
  , scenario "V01_many_policies"            Pass            v01
  , scenario "V02_many_asset_names"         Pass            v02
  , scenario "V03_junk_inputs_untouched"    Pass            v03
  ]
  where
    scenario name expect ctx =
      Scenario
        { scName        = T.pack name
        , scExpect      = expect
        , scParams      = nftParams
        , scContextData = PTx.toData ctx
        }

-------------------------------------------------------------------------------
-- Fixed parameters
-------------------------------------------------------------------------------

-- Hand-built 28-byte CurrencySymbol from a single repeated byte. Avoids
-- relying on hex literal handling for IsString instances.
--
-- We keep two parallel newtype wrappers around the same 'BuiltinByteString':
-- one for 'TOAParamsV1' (which uses the SoP 'PlutusLedgerApi.V1.Value' types)
-- and one for the data-backed 'Value' / 'TxOut' fields. The wire 'Data'
-- representation is identical (both are @B bytes@), so the validator sees a
-- consistent 'AssetClass' regardless of which Haskell wrapper we used to
-- build the context.
nftCsBytes :: BS.ByteString
nftCsBytes = BS.replicate 28 0x11

nftTnBytes :: BS.ByteString
nftTnBytes = BS.pack [0x54, 0x4f, 0x41]  -- "TOA"

mkCS :: ByteString -> CurrencySymbol
mkCS = CurrencySymbol . toBuiltin

nftCs :: CurrencySymbol
nftCs = mkCS nftCsBytes

nftTn :: TokenName
nftTn = TokenName (toBuiltin nftTnBytes)

-- SoP-typed versions used by 'TOAParamsV1'.
nftCsSop :: SopValue.CurrencySymbol
nftCsSop = SopValue.CurrencySymbol (toBuiltin nftCsBytes)

nftTnSop :: SopValue.TokenName
nftTnSop = SopValue.TokenName (toBuiltin nftTnBytes)

nftParams :: TOAParamsV1
nftParams = TOAParamsV1 1 nftCsSop nftTnSop

-- | The 1-unit controlling-NFT value.
nftValue :: Value
nftValue = Value.singleton nftCs nftTn 1

-- | Distinct dummy TxOutRefs.
ownRef, otherRef, thirdRef :: TxOutRef
ownRef   = mkRef 0xa0
otherRef = mkRef 0xb0
thirdRef = mkRef 0xc0

mkRef :: Integer -> TxOutRef
mkRef i =
  TxOutRef
    (TxId (toBuiltin (BS.replicate 32 (fromIntegral (i `mod` 256)))))
    0

-- | Index-flavoured refs for multi-input scenarios.
mkIdxRef :: Integer -> TxOutRef
mkIdxRef i = TxOutRef (TxId (toBuiltin (BS.replicate 32 0xdd))) i

-- | Address of the TOA itself. Payment credential is a synthetic script hash;
-- the validator does not enforce the spending UTxO's address.
toaAddress :: Address
toaAddress =
  Address
    (ScriptCredential (ScriptHash (toBuiltin (BS.replicate 28 0xde))))
    Nothing

-- | A generic wallet address used for the carry-through output.
walletAddress :: Address
walletAddress =
  Address
    (PubKeyCredential (PubKeyHash (toBuiltin (BS.replicate 28 0x01))))
    Nothing

-- | ADA-only value of the given lovelace quantity.
adaOnly :: Integer -> Value
adaOnly = Value.singleton (CurrencySymbol (toBuiltin BS.empty)) (TokenName (toBuiltin BS.empty))

-------------------------------------------------------------------------------
-- TxOut / TxInInfo helpers
-------------------------------------------------------------------------------

walletOut :: Value -> TxOut
walletOut v = TxOut walletAddress v NoOutputDatum Nothing

toaOutInlineUnit :: Value -> TxOut
toaOutInlineUnit v = TxOut toaAddress v (OutputDatum (Datum (PTx.toBuiltinData ()))) Nothing

spendIn :: TxOutRef -> TxOut -> TxInInfo
spendIn = TxInInfo

-------------------------------------------------------------------------------
-- TxInfo defaults
-------------------------------------------------------------------------------

emptyTxInfoSkeleton ::
  List TxInInfo ->
  List TxInInfo ->
  List TxOut ->
  MintValue ->
  TxInfo
emptyTxInfoSkeleton ins refIns outs mint =
  TxInfo
    { txInfoInputs                = ins
    , txInfoReferenceInputs       = refIns
    , txInfoOutputs               = outs
    , txInfoFee                   = Lovelace 1_000_000
    , txInfoMint                  = mint
    , txInfoTxCerts               = DList.fromSOP []
    , txInfoWdrl                  = AssocMap.empty
    , txInfoValidRange            = Interval.always :: POSIXTimeRange
    , txInfoSignatories           = DList.fromSOP []
    , txInfoRedeemers             = AssocMap.empty
    , txInfoData                  = AssocMap.empty
    , txInfoId                    = TxId (toBuiltin (BS.replicate 32 0xee))
    , txInfoVotes                 = AssocMap.empty
    , txInfoProposalProcedures    = DList.fromSOP []
    , txInfoCurrentTreasuryAmount = Nothing
    , txInfoTreasuryDonation      = Nothing
    }

mkContext :: TxOutRef -> TxInfo -> ScriptContext
mkContext spendRef info =
  ScriptContext
    info
    (Redeemer (PTx.toBuiltinData ()))
    (SpendingScript spendRef Nothing)

-------------------------------------------------------------------------------
-- Mint helpers
-------------------------------------------------------------------------------

mintSingleton :: CurrencySymbol -> TokenName -> Integer -> MintValue
mintSingleton cs tn q =
  MintValue.UnsafeMintValue (AssocMap.singleton cs (AssocMap.singleton tn q))

-------------------------------------------------------------------------------
-- Scenarios
-------------------------------------------------------------------------------

s01 :: ScriptContext
s01 = mkContext ownRef info
  where
    nftIn = spendIn otherRef (walletOut (nftValue <> adaOnly 2_000_000))
    toaIn = spendIn ownRef   (toaOutInlineUnit (adaOnly 5_000_000))
    out   = walletOut (nftValue <> adaOnly 7_000_000)
    info  =
      emptyTxInfoSkeleton
        (DList.fromSOP [nftIn, toaIn])
        (DList.fromSOP [])
        (DList.fromSOP [out])
        emptyMintValue

s02 :: ScriptContext
s02 = mkContext ownRef info
  where
    toaIn = spendIn ownRef (toaOutInlineUnit (adaOnly 5_000_000))
    out   = walletOut (adaOnly 4_500_000)
    info  =
      emptyTxInfoSkeleton
        (DList.fromSOP [toaIn])
        (DList.fromSOP [])
        (DList.fromSOP [out])
        emptyMintValue

s03 :: ScriptContext
s03 = mkContext ownRef info
  where
    nftIn1 = spendIn otherRef (walletOut (nftValue <> adaOnly 2_000_000))
    nftIn2 = spendIn thirdRef (walletOut (nftValue <> adaOnly 2_000_000))
    toaIn  = spendIn ownRef   (toaOutInlineUnit (adaOnly 5_000_000))
    out    = walletOut (Value.singleton nftCs nftTn 2 <> adaOnly 7_000_000)
    info   =
      emptyTxInfoSkeleton
        (DList.fromSOP [nftIn1, nftIn2, toaIn])
        (DList.fromSOP [])
        (DList.fromSOP [out])
        emptyMintValue

s04 :: ScriptContext
s04 = mkContext ownRef info
  where
    nftIn = spendIn otherRef (walletOut (nftValue <> adaOnly 2_000_000))
    toaIn = spendIn ownRef   (toaOutInlineUnit (adaOnly 5_000_000))
    out1  = walletOut (nftValue <> adaOnly 2_000_000)
    out2  = walletOut (nftValue <> adaOnly 2_000_000)
    info  =
      emptyTxInfoSkeleton
        (DList.fromSOP [nftIn, toaIn])
        (DList.fromSOP [])
        (DList.fromSOP [out1, out2])
        emptyMintValue

s05 :: ScriptContext
s05 = mkContext ownRef info
  where
    nftIn = spendIn otherRef (walletOut (nftValue <> adaOnly 2_000_000))
    toaIn = spendIn ownRef   (toaOutInlineUnit (adaOnly 5_000_000))
    out   = walletOut (Value.singleton nftCs nftTn 2 <> adaOnly 7_000_000)
    info  =
      emptyTxInfoSkeleton
        (DList.fromSOP [nftIn, toaIn])
        (DList.fromSOP [])
        (DList.fromSOP [out])
        (mintSingleton nftCs nftTn 1)

s06 :: ScriptContext
s06 = mkContext ownRef info
  where
    nftIn = spendIn otherRef (walletOut (nftValue <> adaOnly 2_000_000))
    toaIn = spendIn ownRef   (toaOutInlineUnit (adaOnly 5_000_000))
    out   = walletOut (adaOnly 7_000_000)
    info  =
      emptyTxInfoSkeleton
        (DList.fromSOP [nftIn, toaIn])
        (DList.fromSOP [])
        (DList.fromSOP [out])
        (mintSingleton nftCs nftTn (-1))

s07 :: ScriptContext
s07 = mkContext ownRef info
  where
    nftRefIn = spendIn otherRef (walletOut (nftValue <> adaOnly 2_000_000))
    toaIn    = spendIn ownRef   (toaOutInlineUnit (adaOnly 5_000_000))
    out      = walletOut (adaOnly 4_500_000)
    info     =
      emptyTxInfoSkeleton
        (DList.fromSOP [toaIn])
        (DList.fromSOP [nftRefIn])
        (DList.fromSOP [out])
        emptyMintValue

s08 :: ScriptContext
s08 = mkContext ownRef info
  where
    nftIn  = spendIn otherRef (walletOut (nftValue <> adaOnly 2_000_000))
    toaIns =
      [ spendIn (mkIdxRef i) (toaOutInlineUnit (adaOnly 5_000_000))
      | i <- [0 .. 4]
      ]
    out  = walletOut (nftValue <> adaOnly 25_000_000)
    info =
      emptyTxInfoSkeleton
        (DList.fromSOP (nftIn : toaIns))
        (DList.fromSOP [])
        (DList.fromSOP [out])
        emptyMintValue

-------------------------------------------------------------------------------
-- Value-shape stress
-------------------------------------------------------------------------------

manyPoliciesValue :: Value
manyPoliciesValue =
  foldr (<>) (adaOnly 2_000_000)
    [ Value.singleton (mkCS (BS.replicate 28 (fromIntegral (i + 0x20)))) extraTn 1
    | i <- [0 .. 19] :: [Integer]
    ]
  where
    extraTn = TokenName (toBuiltin (BS.pack [0x74, 0x6f, 0x6b]))  -- "tok"

manyNamesValue :: Value
manyNamesValue =
  foldr (<>) (nftValue <> adaOnly 2_000_000)
    [ Value.singleton nftCs (TokenName (toBuiltin (BS.replicate 4 (fromIntegral (i + 0x40))))) 1
    | i <- [0 .. 19] :: [Integer]
    ]

v01 :: ScriptContext
v01 = mkContext ownRef info
  where
    nftIn = spendIn otherRef (walletOut (nftValue <> adaOnly 2_000_000))
    toaIn = spendIn ownRef   (toaOutInlineUnit manyPoliciesValue)
    out   = walletOut (manyPoliciesValue <> nftValue)
    info  =
      emptyTxInfoSkeleton
        (DList.fromSOP [nftIn, toaIn])
        (DList.fromSOP [])
        (DList.fromSOP [out])
        emptyMintValue

v02 :: ScriptContext
v02 = mkContext ownRef info
  where
    nftIn = spendIn otherRef (walletOut manyNamesValue)
    toaIn = spendIn ownRef   (toaOutInlineUnit (adaOnly 5_000_000))
    out   = walletOut (manyNamesValue <> adaOnly 5_000_000)
    info  =
      emptyTxInfoSkeleton
        (DList.fromSOP [nftIn, toaIn])
        (DList.fromSOP [])
        (DList.fromSOP [out])
        emptyMintValue

v03 :: ScriptContext
v03 = mkContext ownRef info
  where
    nftIn   = spendIn otherRef (walletOut (nftValue <> adaOnly 2_000_000))
    toaIn   = spendIn ownRef   (toaOutInlineUnit (adaOnly 5_000_000))
    junkIns =
      [ spendIn (mkIdxRef (i + 100)) (walletOut (adaOnly 2_000_000))
      | i <- [0 .. 9]
      ]
    out  = walletOut (nftValue <> adaOnly 25_000_000)
    info =
      emptyTxInfoSkeleton
        (DList.fromSOP (nftIn : toaIn : junkIns))
        (DList.fromSOP [])
        (DList.fromSOP [out])
        emptyMintValue
