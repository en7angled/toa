-- | Empirical verification that ToaV1 applied script bytes admit the
-- corrected (chunked-bytestring frame) decomposition:
--
-- @
--   applied_script_bytes
--     == CBOR_BYTESTRING_HEADER(len_flat_body)
--     || FLAT_PREFIX_TOA_V1 (528 B)
--     || consByteString(len(paramCbor), "")   -- 1-byte length prefix
--     || paramCbor
--     || 0x00                                 -- chunked-bytestring terminator
--     || FLAT_SUFFIX_TOA_V1 (1 B)
-- @
--
-- @FLAT_PREFIX_TOA_V1@ and @FLAT_SUFFIX_TOA_V1@ are byte-invariant across
-- all @(policy_id, asset_name)@ inputs. The outer CBOR major-type-2
-- (bytestring) header is a calculable function of the flat-body length.
--
-- The 1-byte length prefix and the @0x00@ chunk-terminator are deterministic
-- from @lengthOfByteString(paramCbor)@ and together form the flat encoding
-- of a @Constant ByteString@ via UPLC's /chunked-bytestring/ flat encoding.
-- For TOA v1, @len(paramCbor)@ is at most ~68 bytes, well below the 255-byte
-- single-chunk limit; multi-chunk encoding is out of scope.
--
-- Run against the published @validators/ToaV1.uplc@. The program tries a
-- battery of @asset_name@ lengths chosen to straddle CBOR length-encoding
-- thresholds (0, 1, 16, 23, 24, 25, 31, 32) and verifies that, after
-- stripping the outer header:
--
--   * the canonical CBOR of the parameter occurs as a UNIQUE contiguous
--     byte-aligned substring of the flat body;
--   * the surrounding @FLAT_PREFIX@ and @FLAT_SUFFIX@ are byte-invariant
--     across all tested @asset_name@ lengths.
--
-- If both hold, on-chain reconstruction via @appendByteString@ + a tiny
-- header-builder over @lengthOfByteString@ is feasible, and the printed
-- @FLAT_PREFIX@/@FLAT_SUFFIX@ (with their blake2b-256 digests) are
-- candidates for normative artifacts in the CIP.
--
-- Exit codes: 0 = all checks pass; non-zero = at least one check failed.

{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Monad         (forM, unless, when)
import           Control.Monad.Except  (runExcept)
import qualified System.Environment
import qualified Codec.Serialise       as Ser
import           Data.Bits             (shiftR, (.&.))
import           Data.ByteString       (ByteString)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS
import           Data.List             (nub)
import qualified Data.Word             as Word
import           Numeric               (showHex)
import           System.Exit           (exitFailure, exitSuccess)

import           Crypto.Hash           (Blake2b_224, Blake2b_256, Digest, hash)
import qualified Data.ByteArray        as BA
import           PlutusLedgerApi.V3    (Data (..))
import           PlutusLedgerApi.Common (SerialisedScript, serialiseUPLC, uncheckedDeserialiseUPLC)
import qualified PlutusCore            as PLC
import qualified UntypedPlutusCore     as UPLC

-- | Apply a 'Data' parameter to an unapplied 'SerialisedScript' the same way
-- 'PlutusLedgerApi.Common.applyArguments' did in earlier pins: deserialise to
-- a UPLC program, build an argument program containing a 'Data' constant at
-- the same version, and re-serialise the resulting 'Apply' node.
applyDataParam :: SerialisedScript -> Data -> SerialisedScript
applyDataParam s d =
  let prog@(UPLC.Program _ ver _) = uncheckedDeserialiseUPLC s
      arg = UPLC.Program () ver (UPLC.Constant () (PLC.someValue d))
  in case runExcept (UPLC.applyProgram prog arg) of
       Left e  -> error ("applyProgram failed: " <> show e)
       Right p -> serialiseUPLC p

-- | Parse the outer CBOR major-type-2 (bytestring) header from a script
-- artifact. Returns @(headerByteCount, declaredPayloadLength)@. Supports
-- the full RFC 8949 range for major type 2:
--
--   * @0x40..0x57@ — payload length encoded inline in the initial byte;
--   * @0x58 ll@   — 1-byte payload length;
--   * @0x59 LL LL@ — 2-byte payload length;
--   * @0x5a LL LL LL LL@ — 4-byte payload length;
--   * @0x5b LL.. (8B)@ — 8-byte payload length.
--
-- Indefinite-length (0x5f) is rejected as plutus-core does not emit it.
parseCborByteStringHeader :: ByteString -> Either String (Int, Int)
parseCborByteStringHeader bs
  | BS.null bs = Left "empty input"
  | otherwise  =
      let b0 = BS.index bs 0
          have n = BS.length bs >= n
          readBE n = BS.foldl' (\acc b -> acc * 256 + fromIntegral b) 0 (BS.take n (BS.drop 1 bs))
      in case b0 of
           _ | b0 >= 0x40 && b0 <= 0x57 -> Right (1, fromIntegral (b0 - 0x40))
           0x58 | have 2 -> Right (2, fromIntegral (BS.index bs 1))
           0x59 | have 3 -> Right (3, readBE 2)
           0x5a | have 5 -> Right (5, readBE 4)
           0x5b | have 9 -> Right (9, readBE 8)
           0x5f         -> Left "indefinite-length bytestring not supported"
           _ -> Left ("unexpected major-type-2 initial byte 0x"
                      ++ BC.unpack (B16.encode (BS.singleton b0)))

blake2b256Hex :: ByteString -> String
blake2b256Hex bs = show (hash bs :: Digest Blake2b_256)

blake2b224 :: ByteString -> ByteString
blake2b224 bs = BA.convert (hash bs :: Digest Blake2b_224)

-- | CBOR canonical (RFC 8949 §4.2.1) header for a major-type-2 (bytestring)
-- of length n. This is the inverse of 'parseCborByteStringHeader'.
cborByteStringHeader :: Int -> ByteString
cborByteStringHeader n
  | n < 0       = error "cborByteStringHeader: negative length"
  | n <= 0x17   = BS.singleton (fromIntegral (0x40 + n))
  | n <= 0xff   = BS.pack [0x58, fromIntegral n]
  | n <= 0xffff = BS.pack [0x59, fromIntegral (n `shiftR` 8), fromIntegral (n .&. 0xff)]
  | n <= 0xffffffff =
      BS.pack [ 0x5a
              , fromIntegral (n `shiftR` 24 .&. 0xff)
              , fromIntegral (n `shiftR` 16 .&. 0xff)
              , fromIntegral (n `shiftR` 8  .&. 0xff)
              , fromIntegral (n             .&. 0xff)
              ]
  | otherwise   = error "cborByteStringHeader: length >= 2^32 not supported by TOA v1"

unappliedArtifactPath :: FilePath
unappliedArtifactPath = "validators/ToaV1.uplc"

-- Asset-name lengths chosen to straddle CBOR length-encoding boundaries:
--   * 0..23 -> direct length encoding (one byte for major+length)
--   * 24    -> first length needing an explicit length byte (0x58 ll)
--   * 32    -> the spec maximum
testAssetNameLens :: [Int]
testAssetNameLens = [0, 1, 16, 23, 24, 25, 31, 32]

-- toa_params_v1 = #6.121([ toa_version : uint, policy_id : bytes28, asset_name : bytes ])
mkToaParam :: Integer -> ByteString -> ByteString -> Data
mkToaParam toaVersion policyId assetName =
  Constr 0 [I toaVersion, B policyId, B assetName]

-- Canonical CBOR of Data. The Serialise instance for Data in plutus-core
-- uses RFC 8949 §4.2.1 deterministic encoding — the same encoding the
-- on-chain `serialiseData` builtin produces. If your pin uses a different
-- function (e.g. `PlutusCore.Data.serialiseData`), swap it in here.
serialiseDataCanonical :: Data -> ByteString
serialiseDataCanonical = LBS.toStrict . Ser.serialise

-- Find every occurrence of needle in haystack. Used to assert uniqueness.
findOccurrences :: ByteString -> ByteString -> [Int]
findOccurrences needle haystack =
  [ i
  | i <- [0 .. BS.length haystack - BS.length needle]
  , BS.take (BS.length needle) (BS.drop i haystack) == needle
  ]

-- | Given a list of equal-length byte strings, return the set of byte
-- positions at which at least one pair differs.
diffPositions :: [ByteString] -> [Int]
diffPositions [] = []
diffPositions (x:xs) =
  [ i
  | i <- [0 .. BS.length x - 1]
  , any (\y -> BS.index y i /= BS.index x i) xs
  ]

-- | Predicate: do the (pfx, paramCborLen) cases match the chunked-bytestring
-- frame pattern? That is, all prefixes have the same length L; they agree
-- everywhere except possibly at position L-1; and the byte at L-1 equals
-- BS.length param_cbor for each case.
matchesChunkedFrame :: [(String, ByteString, ByteString, Int)] -> Bool
matchesChunkedFrame cases =
  case nub [BS.length pfx | (_, pfx, _, _) <- cases] of
    [pfxLen] | pfxLen > 0 ->
      let pfxs    = [pfx | (_, pfx, _, _) <- cases]
          diffs   = diffPositions pfxs
          atTail  = diffs == [pfxLen - 1] || null diffs
          paramOk = and
            [ BS.index pfx (pfxLen - 1) == fromIntegral pLen
            | (_, pfx, _, pLen) <- cases ]
       in atTail && paramOk
    _ -> False

-- | Diagnose how a set of (label, flat_prefix, flat_suffix, param_cbor_len)
-- cases differ. Used only when the chunked-frame pattern does NOT hold,
-- to print a precise byte-level account of the divergence.
diagnoseDifferences :: [(String, ByteString, ByteString, Int)] -> IO ()
diagnoseDifferences cases = do
  let pfxs = [pfx | (_, pfx, _, _) <- cases]
      sfxs = [sfx | (_, _, sfx, _) <- cases]
      pfxLens = nub (map BS.length pfxs)
      sfxLens = nub (map BS.length sfxs)
  putStrLn ""
  putStrLn "=== Diagnostic: where do flat_prefix/flat_suffix differ? ==="
  putStrLn $ "  flat_prefix lengths across cases: " ++ show pfxLens
  putStrLn $ "  flat_suffix lengths across cases: " ++ show sfxLens
  case (pfxLens, sfxLens) of
    ([pfxLen], _) -> do
      let diffs = diffPositions pfxs
      putStrLn $ "  flat_prefix common length:        " ++ show pfxLen ++ " bytes"
      putStrLn $ "  flat_prefix differing positions:  " ++ show diffs
      putStrLn ""
      putStrLn "  Byte at each differing position, per case:"
      let headLen  = if null diffs then pfxLen else minimum diffs
          tailStart = if null diffs then pfxLen else maximum diffs + 1
      putStrLn $ "    invariant head: bytes [0," ++ show (headLen - 1) ++ "] ("
               ++ show headLen ++ " bytes, identical across all cases)"
      putStrLn $ "    invariant tail: bytes [" ++ show tailStart ++ ","
               ++ show (pfxLen - 1) ++ "] ("
               ++ show (pfxLen - tailStart) ++ " bytes, identical across all cases)"
      putStrLn ""
      mapM_ (\(label, pfx, _, pLen) -> do
              let bytesAtDiffs = [BS.index pfx i | i <- diffs]
                  bytesHex     = unwords (map (\b -> "0x" ++ showHex2 b) bytesAtDiffs)
              putStrLn $ "    " ++ label ++ ": " ++ bytesHex
                       ++ "   (param_cbor_len = " ++ show pLen
                       ++ " = 0x" ++ showHex2 (fromIntegral pLen) ++ ")")
            cases
      putStrLn ""
      putStrLn "  >>> FRAME PATTERN check (chunked-bytestring length byte at L-1):"
      let matchesParamLen = and
            [ length diffs == 1
              && BS.index pfx (head diffs) == fromIntegral pLen
            | (_, pfx, _, pLen) <- cases ]
      if matchesParamLen
        then putStrLn "        bytes at differing position equal len(param_cbor)."
        else putStrLn "        differing bytes NOT explained by len(param_cbor) alone."
    _ -> putStrLn "  flat_prefix lengths vary across cases; deeper analysis needed."

showHex2 :: Word.Word8 -> String
showHex2 b = let s = showHex b "" in if length s == 1 then '0' : s else s

data CaseResult = CaseResult
  { caseLabel       :: String
  , appliedBytes    :: ByteString
  , headerBytes     :: ByteString             -- outer CBOR major-type-2 header
  , declaredLen     :: Int                    -- payload length declared by the header
  , flatBody        :: ByteString             -- applied minus header
  , paramCborBytes  :: ByteString
  , decomposition   :: Maybe (ByteString, ByteString)  -- (flat_prefix, flat_suffix)
  , occurrenceCount :: Int
  }

analyze :: SerialisedScript -> Int -> IO CaseResult
analyze unapplied len = do
  let policyId  = BS.replicate 28 (fromIntegral (len + 1))
      assetName = BS.replicate len 0xAB
      param     = mkToaParam 1 policyId assetName
      paramCbor = serialiseDataCanonical param
      applied   = SBS.fromShort (applyDataParam unapplied param)
  (hdrLen, declared) <- case parseCborByteStringHeader applied of
    Right ok -> pure ok
    Left  e  -> do
      putStrLn $ "CBOR header parse failed: " <> e
               <> " (asset_name_len=" <> show len <> ")"
      exitFailure
  let hdr    = BS.take hdrLen applied
      body   = BS.drop hdrLen applied
  unless (declared == BS.length body) $ do
    putStrLn $ "Outer CBOR header for asset_name_len=" ++ show len
             ++ " declares " ++ show declared
             ++ "B payload but actual body is " ++ show (BS.length body) ++ "B"
    exitFailure
  let occs   = findOccurrences paramCbor body
      decomp = case occs of
                 [i] -> Just (BS.take i body, BS.drop (i + BS.length paramCbor) body)
                 _   -> Nothing
  pure CaseResult
    { caseLabel       = "asset_name_len=" ++ show len
    , appliedBytes    = applied
    , headerBytes     = hdr
    , declaredLen     = declared
    , flatBody        = body
    , paramCborBytes  = paramCbor
    , decomposition   = decomp
    , occurrenceCount = length occs
    }

hex :: ByteString -> String
hex = BC.unpack . B16.encode

-- | Encoder identity check (canonical-CBOR fallback path).
--
-- The CIP relies on the offchain CBOR encoder for 'Data' producing byte-
-- identical output to the on-chain Plutus V3 'serialiseData' builtin.
--
-- We use the canonical-CBOR assertion path (documented in the
-- implementation plan as the approved fallback to a full CEK round-trip)
-- because the pinned plutus-core's CEK evaluator requires a
-- 'MachineParameters' value whose construction needs cost-model plumbing
-- that the verifier does not otherwise need.
--
-- The on-chain @serialiseData@ builtin and the offchain
-- @Codec.Serialise.serialise@ instance for 'Data' are implemented by the
-- same 'Serialise' instance in plutus-core's @PlutusCore.Data@ module
-- (they share source). plutus-core's Data CBOR uses a specific
-- deterministic form (not RFC 8949 §4.2.1):
--   * Constr alternative is emitted with the CBOR tag (121 for alt 0,
--     adjusted for higher alternatives) wrapping the field-list array;
--   * @Constr@ field lists and @List@ values are emitted with
--     INDEFINITE-LENGTH array encoding (@0x9f ... 0xff@);
--   * @Map@ uses indefinite-length map encoding (@0xbf ... 0xff@);
--   * integers and bytestrings use smallest-form headers.
--
-- The end-to-end hash check below already proves byte-equality between
-- the offchain encoding and the bytes embedded by @apply_params@ + flat
-- (which is the same source of truth as @serialiseData@). This function
-- additionally asserts the encoding's structural shape so a future
-- regression in @Codec.Serialise@ would be caught here with a precise
-- diagnostic rather than as an opaque hash mismatch.
checkEncoderIdentity :: Data -> IO ()
checkEncoderIdentity d = do
  let bytes = serialiseDataCanonical d
  -- (a) Constr 0 ⇒ CBOR tag 121 ⇒ header 0xd8 0x79.
  case BS.unpack (BS.take 2 bytes) of
    [0xd8, 0x79] -> pure ()
    _ -> do
      putStrLn "ENCODER IDENTITY CHECK: FAIL"
      putStrLn $ "  expected Constr 0 header 0xd8 0x79; got "
              ++ hex (BS.take 2 bytes)
      exitFailure
  -- (b) After the tag, plutus-core emits an INDEFINITE-LENGTH array
  --     (0x9f ... 0xff) for the Constr field list.
  unless (BS.index bytes 2 == 0x9f) $ do
    putStrLn "ENCODER IDENTITY CHECK: FAIL"
    putStrLn $ "  expected indefinite-length array marker 0x9f at "
            ++ "offset 2 (plutus-core Constr encoding); got 0x"
            ++ showHex2 (BS.index bytes 2)
    exitFailure
  unless (BS.index bytes (BS.length bytes - 1) == 0xff) $ do
    putStrLn "ENCODER IDENTITY CHECK: FAIL"
    putStrLn $ "  expected indefinite-length terminator 0xff at end; got 0x"
            ++ showHex2 (BS.index bytes (BS.length bytes - 1))
    exitFailure
  -- (c) The probe parameter is @Constr 0 [I 1, B (28×0x42), B (16×0xAB)]@.
  --     Assert the structural prefix:
  --       0xd8 0x79 0x9f 0x01 0x58 0x1c <28 bytes of 0x42> 0x50 <16 bytes of 0xAB> 0xff
  let expectedFull =
        BS.concat
          [ BS.pack [0xd8, 0x79, 0x9f, 0x01, 0x58, 0x1c]
          , BS.replicate 28 0x42
          , BS.pack [0x50]
          , BS.replicate 16 0xAB
          , BS.pack [0xff]
          ]
  unless (bytes == expectedFull) $ do
    putStrLn "ENCODER IDENTITY CHECK: FAIL"
    putStrLn $ "  full encoding mismatch for probe parameter."
    putStrLn $ "  expected: " ++ hex expectedFull
    putStrLn $ "  actual:   " ++ hex bytes
    exitFailure
  -- (d) Cross-check small-integer smallest-form encoding on a battery of
  --     integers: 0..23 must round-trip to a single byte; 24..255 to
  --     0x18 ll; 256..65535 to 0x19 LL LL.
  let intCases = [(0, BS.pack [0x00]),
                  (1, BS.pack [0x01]),
                  (23, BS.pack [0x17]),
                  (24, BS.pack [0x18, 0x18]),
                  (255, BS.pack [0x18, 0xff]),
                  (256, BS.pack [0x19, 0x01, 0x00]),
                  (65535, BS.pack [0x19, 0xff, 0xff])]
      intMismatches =
        [ (n, expected, got)
        | (n, expected) <- intCases
        , let got = serialiseDataCanonical (I n)
        , got /= expected
        ]
  unless (null intMismatches) $ do
    putStrLn "ENCODER IDENTITY CHECK: FAIL (integer smallest-form)"
    mapM_ (\(n, e, g) ->
            putStrLn $ "  I " ++ show n ++ " : expected "
                    ++ hex e ++ ", got " ++ hex g) intMismatches
    exitFailure
  putStrLn "Encoder identity check: PASS"
  putStrLn $ "  method: plutus-core Data CBOR structural assertion battery"
  putStrLn $ "          (Constr 0 tag 0xd8 0x79, indefinite-length 0x9f..0xff"
  putStrLn $ "           field array, smallest-form integer + bytestring headers)"

main :: IO ()
main = do
  -- Encoder identity check (offchain canonical CBOR vs the on-chain
  -- serialiseData builtin's specified output). See note on
  -- 'checkEncoderIdentity' for the canonical-CBOR vs CEK choice.
  let probe = mkToaParam 1 (BS.replicate 28 0x42) (BS.replicate 16 0xAB)
  checkEncoderIdentity probe

  raw <- BS.readFile unappliedArtifactPath
  let unapplied = SBS.toShort raw
  putStrLn $ "Unapplied artifact: " ++ unappliedArtifactPath
  putStrLn $ "Unapplied size:     " ++ show (BS.length raw) ++ " bytes\n"

  results <- forM testAssetNameLens (analyze unapplied)

  mapM_ printCase results

  let withDecomp = [(pfx, sfx) | r <- results, Just (pfx, sfx) <- [decomposition r]]
      flatPfxs   = nub (map fst withDecomp)
      flatSfxs   = nub (map snd withDecomp)
      allUnique  = length withDecomp == length results

  putStrLn "=== Invariance check (over flat body, header excluded) ==="
  putStrLn $ "  cases with unique decomposition: "
          ++ show (length withDecomp) ++ "/" ++ show (length results)
  putStrLn $ "  distinct outer-prefix values:    " ++ show (length flatPfxs)
  putStrLn $ "  distinct outer-suffix values:    " ++ show (length flatSfxs)

  unless allUnique $ do
    putStrLn ""
    putStrLn "FAILED: in at least one case, the param CBOR bytes are not a unique"
    putStrLn "contiguous byte-aligned substring of the flat body."
    putStrLn "Likely cause: flat encoding is not byte-aligned at the parameter boundary."
    putStrLn "On-chain prefix||param||suffix reconstruction is NOT feasible for this"
    putStrLn "compiled artifact; either accept that limitation or recompile the validator"
    putStrLn "with an alignment-forcing wrapper (e.g. an extra dummy lambda) and retry."
    exitFailure

  -- Corrected model (chunked-bytestring frame around param_cbor):
  --   flat_body = FLAT_PREFIX_TOA_V1
  --            || consByteString(len(param_cbor), "")   -- 1-byte length prefix
  --            || param_cbor
  --            || 0x00                                  -- chunked-bstr terminator
  --            || FLAT_SUFFIX_TOA_V1
  --
  -- The existing `analyze` returns (pfx, sfx) where pfx ends with the
  -- chunked length byte and sfx begins with the chunked terminator 0x00.
  -- HYPOTHESIS HOLDS iff:
  --   * length flatSfxs == 1, single sfx is exactly 0x00 0x01 (2 bytes), and
  --   * all pfxs share a common length L, differ only at position L-1, and
  --     the byte at L-1 equals BS.length param_cbor for each case.
  let frameCases =
        [ (caseLabel r, pfx, sfx, BS.length (paramCborBytes r))
        | r <- results, Just (pfx, sfx) <- [decomposition r] ]
      sfxOk = case flatSfxs of
                [s] -> s == BS.pack [0x00, 0x01]
                _   -> False
      frameOk = sfxOk && matchesChunkedFrame frameCases

  if not frameOk
    then do
      putStrLn ""
      putStrLn "FAILED: outer-prefix/outer-suffix pattern does not match the"
      putStrLn "expected chunked-bytestring frame around param_cbor."
      diagnoseDifferences frameCases
      exitFailure
    else do
      -- Extract the invariant constants under the corrected model.
      let firstPfx     = head flatPfxs   -- all pfxs identical except at L-1
          pfxLen       = BS.length firstPfx
          flatPrefix   = BS.take (pfxLen - 1) firstPfx       -- 475 B expected
          firstSfx     = head flatSfxs                       -- exactly 0x00 0x01
          flatSuffix   = BS.drop 1 firstSfx                  -- 1 B expected (0x01)
      -- Assert that the canonical artifact sizes have not drifted.
      unless (pfxLen - 1 == 528) $ do
        putStrLn $ "EXPECTED FLAT_PREFIX_TOA_V1 size of 528 bytes, got " ++ show (pfxLen - 1)
        putStrLn   "  This indicates the canonical ToaV1.uplc artifact has changed."
        putStrLn   "  Either the artifact was recompiled with a different toolchain, or"
        putStrLn   "  this verifier is being run against a non-canonical artifact."
        exitFailure
      unless (BS.length flatSuffix == 1) $ do
        putStrLn $ "EXPECTED FLAT_SUFFIX_TOA_V1 size of 1 byte, got " ++ show (BS.length flatSuffix)
        putStrLn   "  This indicates the canonical ToaV1.uplc artifact has changed."
        putStrLn   "  Either the artifact was recompiled with a different toolchain, or"
        putStrLn   "  this verifier is being run against a non-canonical artifact."
        exitFailure
      unless (flatSuffix == BS.pack [0x01]) $ do
        putStrLn $ "EXPECTED FLAT_SUFFIX_TOA_V1 to be 0x01, got 0x" ++ showHex2 (BS.index flatSuffix 0)
        putStrLn   "  This indicates the canonical ToaV1.uplc artifact has changed."
        putStrLn   "  Either the artifact was recompiled with a different toolchain, or"
        putStrLn   "  this verifier is being run against a non-canonical artifact."
        exitFailure
      putStrLn ""
      putStrLn "HYPOTHESIS HOLDS (chunked-bytestring frame around param)."
      putStrLn $ "  FLAT_PREFIX_TOA_V1 (" ++ show (BS.length flatPrefix) ++ " bytes):"
      putStrLn $ "    hex:           " ++ hex flatPrefix
      putStrLn $ "    blake2b-256:   " ++ blake2b256Hex flatPrefix
      putStrLn $ "  FLAT_SUFFIX_TOA_V1 (" ++ show (BS.length flatSuffix) ++ " byte):"
      putStrLn $ "    hex:           " ++ hex flatSuffix
      putStrLn $ "    blake2b-256:   " ++ blake2b256Hex flatSuffix
      putStrLn ""
      putStrLn "  applied_bytes(p) =="
      putStrLn "      CBOR_BYTESTRING_HEADER(len_flat_body)"
      putStrLn "    || FLAT_PREFIX_TOA_V1"
      putStrLn "    || consByteString(len(serialiseData(p)), serialiseData(p))"
      putStrLn "    || 0x00"
      putStrLn "    || FLAT_SUFFIX_TOA_V1"
      putStrLn "  with FLAT_PREFIX/FLAT_SUFFIX byte-invariant across all tested lengths."

      -- End-to-end hash check: R(pid, an) reassembly must reproduce the
      -- script hash that apply_params + serialiseUPLC + blake2b_224(0x03 || _) produces.
      putStrLn ""
      putStrLn "=== End-to-end hash check (R vs apply_params) ==="
      let mkParam len =
            let policyId  = BS.replicate 28 (fromIntegral (len + 1))
                assetName = BS.replicate len 0xAB
            in (policyId, assetName, mkToaParam 1 policyId assetName)
      hashResults <- forM testAssetNameLens $ \len -> do
        let (_, _, p) = mkParam len
            paramCbor = serialiseDataCanonical p
        -- TOA v1 supports param_cbor that fits in a single chunked-bytestring
        -- chunk (max 255 B). The probe parameter is well under this; assert
        -- explicitly so any future widening of the parameter type fails loudly.
        when (BS.length paramCbor > 255) $ do
          putStrLn $ "  len=" ++ show len
                  ++ "  param_cbor=" ++ show (BS.length paramCbor) ++ "B"
                  ++ "  EXCEEDS single chunked-bytestring chunk (255 B)"
          putStrLn "param_cbor exceeds single chunked-bytestring chunk;"
          putStrLn "multi-chunk encoding not supported by TOA v1."
          exitFailure
        let -- via apply_params:
            appliedAP   = SBS.fromShort (applyDataParam unapplied p)
            hashAP      = blake2b224 (BS.cons 0x03 appliedAP)
            -- via R reassembly under the chunked-frame model:
            chunkedLen  = BS.singleton (fromIntegral (BS.length paramCbor))
            chunkedTerm = BS.singleton 0x00
            flatBody    = flatPrefix
                            `BS.append` chunkedLen
                            `BS.append` paramCbor
                            `BS.append` chunkedTerm
                            `BS.append` flatSuffix
            hdr         = cborByteStringHeader (BS.length flatBody)
            appliedR    = hdr `BS.append` flatBody
            hashR       = blake2b224 (BS.cons 0x03 appliedR)
            ok          = hashR == hashAP && appliedR == appliedAP
        putStrLn $ "  len=" ++ show len
                ++ "  hashR=" ++ BC.unpack (B16.encode hashR)
                ++ (if ok then "  OK" else "  MISMATCH")
        pure (len, ok, hashR, hashAP, appliedR, appliedAP)
      let bad = [ (len, hR, hAP, aR, aAP)
                | (len, False, hR, hAP, aR, aAP) <- hashResults ]
      if null bad
        then do
          putStrLn "End-to-end hash check: PASS for all tested lengths"
          args <- System.Environment.getArgs
          when ("--write-artifacts" `elem` args) $ do
            BS.writeFile "validators/FLAT_PREFIX_TOA_V1.bin" flatPrefix
            BS.writeFile "validators/FLAT_SUFFIX_TOA_V1.bin" flatSuffix
            putStrLn ""
            putStrLn "Wrote validators/FLAT_PREFIX_TOA_V1.bin and validators/FLAT_SUFFIX_TOA_V1.bin"
          exitSuccess
        else do
          putStrLn "END-TO-END HASH CHECK: FAIL"
          mapM_ (\(len, hR, hAP, aR, aAP) -> do
                   putStrLn $ "  len=" ++ show len
                   putStrLn $ "    hashR  = " ++ BC.unpack (B16.encode hR)
                   putStrLn $ "    hashAP = " ++ BC.unpack (B16.encode hAP)
                   putStrLn $ "    bytesR (first 32B):  " ++ hex (BS.take 32 aR)
                   putStrLn $ "    bytesAP (first 32B): " ++ hex (BS.take 32 aAP)) bad
          exitFailure

  where
    printCase r = do
      putStrLn $ "=== " ++ caseLabel r ++ " ==="
      putStrLn $ "  applied size:        " ++ show (BS.length (appliedBytes r)) ++ " bytes"
      putStrLn $ "  outer CBOR header:   " ++ hex (headerBytes r)
                                   ++ " (declares " ++ show (declaredLen r) ++ "B payload)"
      putStrLn $ "  flat body size:      " ++ show (BS.length (flatBody r)) ++ " bytes"
      putStrLn $ "  param CBOR size:     " ++ show (BS.length (paramCborBytes r)) ++ " bytes"
      putStrLn $ "  param CBOR matches:  " ++ show (occurrenceCount r) ++ " occurrence(s)"
      case decomposition r of
        Nothing -> putStrLn "  RESULT: no unique decomposition"
        Just (pfx, sfx) -> do
          putStrLn $ "  RESULT: unique decomposition"
          putStrLn $ "    flat_prefix len: " ++ show (BS.length pfx)
                  ++ "  flat_suffix len: " ++ show (BS.length sfx)
          putStrLn $ "    flat_prefix head (16B): " ++ hex (BS.take 16 pfx)
          putStrLn $ "    flat_suffix (full):     " ++ hex sfx
      putStrLn ""