# TOA v1 Test Vectors — Schema

This file documents the schema of `toa-v1.json`, the address-derivation test vectors for the Token-Owned Addresses (TOA) standard. The vectors are normative; this schema documentation is referenced from [CIP-Token-Owned-Addresses](https://github.com/cardano-foundation/CIPs/pull/1200) by blake2b-256 content hash.

The vectors are generated deterministically from the canonical Plinth source by `cabal run toa-gen-vectors`. Conformance rules for implementations consuming these vectors are defined in the CIP under *Test Vector Format*.

## JSON envelope

```json
{
  "unapplied_script_bytes": 476,
  "max_reference_script_bytes": 16384,
  "vectors": [
    {
      "name": "ascii",
      "toa_version": 1,
      "policy_id": "<28-byte hex>",
      "asset_name_hex": "544f412054657374204e465420303031",
      "cip14_fingerprint": "asset1...",

      "params_cbor_hex": "<canonical PlutusData CBOR of TOAParamsV1>",
      "unapplied_script_hash": "129181a58ca3716aada61244d3d4210bff5a7235f709189af2596dc0",
      "applied_script_cbor_hex": "<CBOR of applied script bytes — the bytes that go into the tx `script` field>",
      "applied_script_bytes": 551,

      "expected_script_hash": "<28-byte hex>",
      "expected_address_mainnet": "addr1...",
      "expected_address_testnet": "addr_test1...",
      "datum_policy": "inline_unit_recommended",
      "self_deposit_semantics": "controlled_by_external_nft_holder"
    }
  ]
}
```

## Top-level envelope fields

- `unapplied_script_bytes` — size in bytes of the canonical un-applied UPLC artifact, factored out of each vector because it is a single global value (476 in the current publication). A mismatch here points to a Plinth or toolchain version drift before any per-vector check.
- `max_reference_script_bytes` — the Cardano reference-script size ceiling per transaction (16384). Included so vector consumers can confirm at a glance that the un-applied template and every applied script stay well within the limit when published as a reference script.
- `vectors` — the array of per-`(toa_version, policy_id, asset_name)` records described below.

## Per-vector fields

- `cip14_fingerprint` is `bech32(hrp = "asset", blake2b-160(policy_id || asset_name))` as defined by [CIP-0014](https://cips.cardano.org/cip/CIP-0014). It is included only as a user-facing identifier and diagnostic cross-check on `(policy_id, asset_name_hex)`: an implementation that has the wrong asset-class bytes will fail the fingerprint check before reaching address derivation, yielding a much clearer diagnostic than a downstream script-hash mismatch. Because the fingerprint is one-way, resolving `asset1...` back to `(policy_id, asset_name)` requires an indexer, explorer, or wallet asset database. The fingerprint is **not** an input to TOA address derivation.

- `params_cbor_hex`, `unapplied_script_hash`, `applied_script_cbor_hex`, and `applied_script_bytes` are **debug-aid fields** included in every vector so implementers can locate failures along the encoding chain. Concretely:
    - `params_cbor_hex` is the canonical PlutusData CBOR of the `TOAParamsV1` argument. A mismatch here points to a parameter-encoding bug (CDDL serialisation, integer minimality, or constructor-tag form).
    - `unapplied_script_hash` is the template hash *before* parameter application — `blake2b_224(0x03 || unapplied_script_bytes)`. It is constant across every v1 vector (`129181a58ca3716aada61244d3d4210bff5a7235f709189af2596dc0`). A mismatch points to a Plinth or toolchain version drift.
    - `applied_script_cbor_hex` is the CBOR of the applied script bytes — the same bytes that go into a transaction's `script` field. A mismatch here points to a parameter-application bug. If this matches but `expected_script_hash` does not, the bug is in the hashing path.
    - `applied_script_bytes` is the byte length of `applied_script_cbor_hex` after hex decode — useful for confirming reference-script sizing.

- `datum_policy` documents the canonical deposit convention exercised by the vector. Address-derivation vectors use `inline_unit_recommended` to mark that the derived TOA is intended to be funded with the canonical inline `Unit Datum` deposit pattern. Validator-scenario vectors (when published) will extend the enum to cover the four-way datum-classification set — `unit_datum`, `inline_other`, `datum_hash`, `no_datum` — so each scenario records which datum class the spent UTxO actually carries.

- `self_deposit_semantics` documents whether the vector exercises the self-deposit case (`permissionless_spend_if_nft_is_at_own_toa`) or the ordinary case (`controlled_by_external_nft_holder`).

## Regenerating the vectors

The vectors and the un-applied UPLC artifact are regenerated together by:

```sh
cabal run toa-gen-vectors
```

The executable is pure — it does not contact a node — and its output is deterministic given the pinned toolchain in `cabal.project.freeze`.
