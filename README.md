# cardano-toa

[![CI](https://github.com/en7angled/toa/actions/workflows/ci.yml/badge.svg)](https://github.com/en7angled/toa/actions/workflows/ci.yml)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)

Reference implementation and tooling for **Token-Owned Addresses (TOA)** on Cardano.

A TOA is a deterministic Cardano enterprise script address derived from a native
asset class `(policyId, assetName)`. The address is spendable by any transaction
that consumes exactly one unit of the controlling NFT as input, produces exactly
one unit as output, and does not mint or burn that asset class in the same
transaction.

A formal Cardano Improvement Proposal is being prepared separately; this repo is
the reference implementation.

## What's in this repo

| Component | Path | Purpose |
|---|---|---|
| `onchain-lib`         | `src/lib/onchain-lib/`  | TOA v1 Plinth validator and canonical parameter type. Plinth-pure (no off-chain deps). |
| `offchain-lib`        | `src/lib/offchain-lib/` | Atlas-based tx building, queries, address derivation, RFC-8949 canonical CBOR, CIP-14 asset fingerprints. |
| `webapi-lib`          | `src/lib/webapi-lib/`   | Servant HTTP API (`/toa/derive`, `/toa/utxos`, `/toa/spend`, `/tx/{submit,sign,tx-status}`) + Swagger UI. |
| `onchain-test-lib`    | `src/test-onchain/`     | Test-only always-true minting policy used by the CLB scenario suite. |
| `interaction-api`     | `src/exe/interaction-api/`     | HTTP server executable. |
| `toa-gen-vectors`     | `src/exe/toa-gen-vectors/`     | Emits the six normative CIP test vectors (`test-vectors/toa-v1.json`) and the un-applied UPLC artifact (`validators/ToaV1.uplc`). |
| `toa-gen-swagger`     | `src/exe/toa-gen-swagger/`     | Emits the API Swagger JSON to `docs/generated/swagger/toa-api.json`. |
| `toa-bench`           | `src/exe/toa-bench/`           | Synthetic-context micro-benchmark for the validator with baseline-diff support. |

## Layering rule

Dependencies point inward only: `webapi-lib → offchain-lib → onchain-lib`.
The on-chain library must not import `aeson`, `servant`, `swagger2`, or any
transitive pull-in — it stays Plinth-pure.

## Bootstrap

```sh
direnv allow          # loads the Nix flake (first time: long, ~30 min cold)
cabal build all
cabal test            # 38 tests
```

If `haskell-language-server` fails to start in VS Code, run `just link-hls` to
refresh `.vscode/haskell-language-server.link` (the `.envrc` does this
automatically on reload).

## Running the HTTP API

```sh
cp config/config_atlas.example.json config/config_atlas.json
# Edit config_atlas.json: replace <REPLACE_ME> with your Maestro API token.

cp .env.example .env
# Edit .env: set BASIC_USER and BASIC_PASS for /toa/* and /tx/* basic-auth.

cabal run interaction-api
# Listening at http://0.0.0.0:8080
# Swagger UI:  http://0.0.0.0:8080/swagger-ui
```

The pre-generated Swagger JSON is committed at
`docs/generated/swagger/toa-api.json` for offline browsing.

## Regenerating committed artifacts

```sh
cabal run toa-gen-vectors   # writes test-vectors/toa-v1.json + validators/ToaV1.uplc
cabal run toa-gen-swagger   # writes docs/generated/swagger/toa-api.json
```

## Benchmarking the validator

```sh
cabal run toa-bench                                       # human-readable table
cabal run toa-bench -- --out bench/results.json           # JSON for baseline diff
cabal run toa-bench -- --baseline bench/baseline.json     # regression check (exit 1 on regression)
```

## Repo layout

```
.
├── .envrc                                # direnv + nix flake (devx ghc96-iog-full)
├── .justfile                             # `just allow`, `just reload`, `just link-hls`, `just gc`
├── .cz.toml                              # commitizen, semver, no `v` tag prefix
├── hie.yaml                              # HLS cabal cradle
├── cabal.project                         # CHaP + Atlas + Plutus pins
├── cabal.project.freeze                  # pinned package versions
├── cardano-toa.cabal                     # libs + executables + test suite
├── config/
│   └── config_atlas.example.json         # Atlas provider template (real config gitignored)
├── bench/baseline.json                   # validator-cost regression baseline
├── docs/generated/swagger/toa-api.json   # pre-generated OpenAPI spec
├── test-vectors/toa-v1.json              # six CIP address-derivation test vectors
├── validators/ToaV1.uplc                 # un-applied UPLC artifact (template)
└── src/
    ├── lib/
    │   ├── onchain-lib/
    │   ├── offchain-lib/
    │   └── webapi-lib/
    ├── test/
    ├── test-onchain/
    └── exe/
        ├── interaction-api/
        ├── toa-gen-vectors/
        ├── toa-gen-swagger/
        └── toa-bench/
```

## License

MIT — see [LICENSE](LICENSE).
