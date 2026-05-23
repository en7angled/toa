{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | JSON emission and baseline-diff for toa-bench.
module Bench.Report
  ( BenchOutput (..),
    Toolchain (..),
    encodeOutput,
    decodeOutput,
    reportText,
    diffAgainst,
  )
where

import Bench.Eval (Result (..))
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as Aeson
import Data.Aeson.Encode.Pretty (Config (..), Indent (..), defConfig, encodePretty')
import Data.ByteString.Lazy qualified as BSL
import Data.List (sortOn)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Text.Printf (printf)

-------------------------------------------------------------------------------
-- JSON shape
-------------------------------------------------------------------------------

data Toolchain = Toolchain
  { ghc :: Text,
    plutusLedgerApi :: Text,
    targetPlcVersion :: Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON Toolchain

instance FromJSON Toolchain

data BenchOutput = BenchOutput
  { version :: Int,
    toolchain :: Toolchain,
    costModelSource :: Text,
    scenarios :: [Result]
  }
  deriving (Eq, Show, Generic)

instance ToJSON BenchOutput

instance FromJSON BenchOutput

instance ToJSON Result where
  toJSON :: Result -> Aeson.Value
  toJSON Result {..} =
    Aeson.object
      [ "name" Aeson..= rName,
        "expected" Aeson..= rExpected,
        "ok" Aeson..= rOk,
        "cpu" Aeson..= rCpu,
        "mem" Aeson..= rMem,
        "logs" Aeson..= rLogs,
        "error" Aeson..= rError
      ]

instance FromJSON Result where
  parseJSON = Aeson.withObject "Result" $ \o ->
    Result
      <$> o Aeson..: "name"
      <*> o Aeson..: "expected"
      <*> o Aeson..: "ok"
      <*> o Aeson..: "cpu"
      <*> o Aeson..: "mem"
      <*> o Aeson..: "logs"
      <*> o Aeson..:? "error"

-------------------------------------------------------------------------------
-- Encoding
-------------------------------------------------------------------------------

encodeOutput :: BenchOutput -> BSL.ByteString
encodeOutput =
  encodePretty'
    defConfig
      { confIndent = Spaces 2,
        confCompare = compare,
        confTrailingNewline = True
      }

decodeOutput :: BSL.ByteString -> Either String BenchOutput
decodeOutput = Aeson.eitherDecode

-------------------------------------------------------------------------------
-- Plain-text report
-------------------------------------------------------------------------------

reportText :: [Result] -> Text
reportText rs =
  T.unlines $
    header
      : separator
      : fmap row rs
      ++ [separator, summary rs]
  where
    header = T.pack (printf "%-40s  %-9s  %12s  %10s  %4s" ("scenario" :: String) ("expected" :: String) ("cpu" :: String) ("mem" :: String) ("ok" :: String))
    separator = T.replicate 86 "-"
    row r = T.pack (printf "%-40s  %-9s  %12d  %10d  %4s" (T.unpack (rName r)) (T.unpack (rExpected r)) (rCpu r) (rMem r) (okSymbol (rOk r)))
    okSymbol :: Bool -> String
    okSymbol True = "ok"
    okSymbol False = "FAIL"
    summary results =
      let nOk = length (filter rOk results)
          nTotal = length results
       in T.pack (printf "%d/%d scenarios match expectation" nOk nTotal)

-------------------------------------------------------------------------------
-- Baseline diff
-------------------------------------------------------------------------------

-- | Compare 'current' results against a committed baseline. Returns 0 if
-- every scenario's CPU and memory unit count is ≤ baseline, 1 otherwise.
-- Prints a per-scenario delta table to stdout.
diffAgainst :: BenchOutput -> BenchOutput -> (Int, Text)
diffAgainst baseline current =
  let byName = sortOn rName . scenarios
      bs = byName baseline
      cs = byName current
      rows = zipWith diffRow bs cs
      anyRegress = any (\(_, dCpu, dMem) -> dCpu > 0 || dMem > 0) rows
      tableHeader = T.pack (printf "%-40s  %12s  %12s  %12s  %12s  %12s  %12s" ("scenario" :: String) ("base cpu" :: String) ("cur cpu" :: String) ("Δ cpu" :: String) ("base mem" :: String) ("cur mem" :: String) ("Δ mem" :: String))
      tableSeparator = T.replicate 116 "-"
      printedRows = fmap formatRow rows
      output =
        T.unlines $
          tableHeader
            : tableSeparator
            : printedRows
            ++ [tableSeparator, verdict anyRegress]
   in (if anyRegress then 1 else 0, output)
  where
    diffRow b c =
      let dCpu = rCpu c - rCpu b
          dMem = rMem c - rMem b
       in (rName c, dCpu, dMem)

    formatRow (name, dCpu, dMem) =
      T.pack
        ( printf
            "%-40s  %12s  %12s  %+12d  %12s  %12s  %+12d"
            (T.unpack name)
            ("" :: String)
            ("" :: String)
            dCpu
            ("" :: String)
            ("" :: String)
            dMem
        )

    verdict True = "REGRESSION: at least one scenario got worse"
    verdict False = "OK: every scenario unchanged or improved"
