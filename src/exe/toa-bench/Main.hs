{-# LANGUAGE OverloadedStrings #-}

-- | toa-bench CLI.
--
-- Modes:
--
--   * @cabal run toa-bench@                       — print per-scenario table
--   * @cabal run toa-bench -- --out PATH@         — write JSON to PATH
--   * @cabal run toa-bench -- --baseline PATH@    — diff current run against
--                                                   the JSON at PATH; exit 1
--                                                   on regression.
module Main (main) where

import Bench.Contexts qualified as Contexts
import Bench.Eval (Result, makeEvaluationContext, runScenario)
import Bench.Report
  ( BenchOutput (..)
  , Toolchain (..)
  , decodeOutput
  , diffAgainst
  , encodeOutput
  , reportText
  )
import Data.ByteString.Lazy qualified as BSL
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitWith)
import System.FilePath (takeDirectory)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  args <- getArgs
  ec <- makeEvaluationContext
  let results :: [Result]
      results = fmap (runScenario ec) Contexts.scenarios
      output  = currentOutput results
  case args of
    [] ->
      TIO.putStrLn (reportText results)
    ["--out", path] -> do
      createDirectoryIfMissing True (takeDirectory path)
      BSL.writeFile path (encodeOutput output)
      TIO.putStrLn (reportText results)
      hPutStrLn stderr ("wrote " <> path)
    ["--baseline", path] -> do
      raw <- BSL.readFile path
      case decodeOutput raw of
        Left err -> do
          hPutStrLn stderr ("failed to parse baseline " <> path <> ": " <> err)
          exitWith (ExitFailure 2)
        Right baseline -> do
          let (code, text) = diffAgainst baseline output
          TIO.putStrLn text
          exitWith (if code == 0 then ExitSuccess else ExitFailure code)
    _ -> do
      hPutStrLn stderr "usage: toa-bench [--out PATH | --baseline PATH]"
      exitWith (ExitFailure 2)

currentOutput :: [Result] -> BenchOutput
currentOutput results =
  BenchOutput
    { version         = 1
    , toolchain       = Toolchain
        { ghc              = "9.6.6"
        , plutusLedgerApi  = "1.43.1.0"
        , targetPlcVersion = "1.1.0"
        }
    , costModelSource =
        T.pack "PlutusLedgerApi.Test.V3.EvaluationContext.costModelParamsForTesting (testing model committed in plutus-ledger-api)"
    , scenarios       = results
    }
