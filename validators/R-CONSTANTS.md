# R Canonical Byte Constants — TOA v1

These two byte sequences are the **normative inputs** to the canonical TOA v1
address-derivation procedure (function R). They are extracted from
`validators/ToaV1.uplc` by `toa-verify-reconstruction` and cross-checked
against the byte-level decomposition of the applied script. Any implementation
of R MUST use exactly these bytes — see the TOA CIP §"Address Derivation".

## Decomposition Recipe

R reconstructs the applied script bytes as:

```
applied_script_bytes
  == CBOR_BYTESTRING_HEADER(len_flat_body)
  || FLAT_PREFIX_TOA_V1                          (528 bytes, invariant)
  || consByteString(len(paramCbor), "")          (1 byte, computed from paramCbor)
  || paramCbor                                   (canonical PlutusData CBOR)
  || 0x00                                        (chunked-bytestring terminator)
  || FLAT_SUFFIX_TOA_V1                          (1 byte, invariant)
```

The 1-byte length prefix and 0x00 terminator form the UPLC chunked-bytestring
flat encoding of `paramCbor` as a Constant ByteString. For TOA v1, the
maximum `len(paramCbor)` is ~68 bytes, well below the 255-byte single-chunk
limit; multi-chunk encoding is out of scope.

## FLAT_PREFIX_TOA_V1

- **File:** `FLAT_PREFIX_TOA_V1.bin`
- **Size:** 528 bytes
- **blake2b-256:** `6ab7ef002cda6f7e3c60e5975fce175c6e56a91b51b7d488d2ae69af23520235`
- **Hex (32 bytes per row):**

```
  01010032259932323255333573466e1d200000211328009bad35742005375c6a
  e840066eb8d5d09aba20011aba200111635573c0046aae74004dd50014888c8c
  8c8c954ccd5cd19b8748008d55ce800889929991914aa999ab9a3370e90004ac
  998011ba900a37566ae84d5d11aba200690c004dd5800a30024a564cc00cdd48
  05000c860026eb400518012400348001480002300011801454cc8c8a554ccd5c
  d19b8748008ccc8c00400488cc02000488c8ccc01cd5d09aba20012222337006
  6601201201000c66601800602c02a52d61aab9e375400490001bac3574201423
  000118014554ccd5cd19b8748008ccc8c00400488cc02000488ccc0180088888
  cdc019980400400380299980580180a80a14b5890001bac35742012230001180
  14004444646464666600a6ae8400cdd59aba100235742002646464aa666ae68c
  dc3a4004004230021155333573466e1d2000002118009bae3574200211635573
  c0046aae74004dd51aba1357440026ae88004d5d10009aab9e37540064452660
  0a6ea400800e46526600e6ea400c00646eb400690000dd5800d20000c00a0022
  3223500200133230010012213300480112a999ab9a3375e0086aae7400444a00
  26aae7800822a660060062400224002444a666aae7c004400c4cc008d5d08009
  aba200118011aab9e00111637546ae84d5d11aba2003357446ae88004d55cf1b
  aa357420026aae78dd50022293458981
```

## FLAT_SUFFIX_TOA_V1

- **File:** `FLAT_SUFFIX_TOA_V1.bin`
- **Size:** 1 byte
- **blake2b-256:** `ee155ace9c40292074cb6aff8c9ccdd273c81648ff1149ef36bcea6ebb8a3e25`
- **Hex:** `01`

## Verification

Reproducible: `cabal run toa-verify-reconstruction` (must exit 0). The
captured stdout of the verification run that produced these constants is
`R-verify-output.txt`.

## Role

Both constants are loaded as compile-time `BuiltinByteString` literals by
the Plinth module `Onchain.Derivation.R` and as runtime `ByteString`s by
the offchain mirror `TxBuilding.Toa.DerivationR` (introduced in the next
commit). The single source of truth is the binary files; both Haskell
modules `embedFile` them.
