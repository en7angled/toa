{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Wrap @evaluateScriptCounting@ for the toa-bench harness. Builds an
-- @EvaluationContext@ once from the plutus-ledger-api testlib's
-- @costModelParamsForTesting@ (V3, ledger-order @[Int64]@) and runs each
-- scenario against the un-typed @BuiltinData -> BuiltinUnit@ form of the
-- TOA v1 validator applied to its 'TOAParamsV1'.
--
-- The cost model is the V3 testing model committed inside plutus-ledger-api;
-- absolute CPU/mem numbers will not match a specific mainnet snapshot, but
-- the model is deterministic for a pinned plutus-ledger-api version (see
-- cabal.project) and is therefore stable across the pre- and post-refactor
-- runs. That stability is what the baseline/results diff depends on.
module Bench.Eval
  ( Expectation (..),
    Scenario (..),
    Result (..),
    makeEvaluationContext,
    runScenario,
  )
where

import Control.Monad.Except (runExcept)
import Control.Monad.Writer (runWriterT)
import Data.SatInt (unSatInt)
import Data.Text (Text)
import Data.Text qualified as T
import Onchain.Protocol.Types (TOAParamsV1)
import Onchain.Validators.ToaV1Validator (toaV1ApplyConstantData)
import PlutusCore.Evaluation.Machine.ExBudget (ExBudget (..))
import PlutusCore.Evaluation.Machine.ExMemory (ExCPU (..), ExMemory (..))
import PlutusLedgerApi.Common.Versions (futurePV)
import PlutusLedgerApi.Test.V3.EvaluationContext qualified as TestEC
import PlutusLedgerApi.V3 qualified as V3
import PlutusTx qualified as PTx

-- | What a scenario expects from the validator.
data Expectation
  = Pass
  | -- | Free-text description of the expected failure (e.g. @"T1"@). The
    -- harness does not currently match the trace prefix against the actual
    -- evaluator logs — @FailWith _@ is satisfied by any evaluation error.
    FailWith Text
  deriving (Eq, Show)

-- | One bench scenario: a name, expectation, parameters used to apply the
-- validator, and a pre-encoded @Data@ representation of the synthetic
-- @ScriptContext@ used as the script's sole argument.
data Scenario = Scenario
  { scName :: Text,
    scExpect :: Expectation,
    scParams :: TOAParamsV1,
    scContextData :: PTx.Data
  }

-- | Per-scenario evaluation result. Emitted as JSON by 'Bench.Report'.
data Result = Result
  { rName :: Text,
    rExpected :: Text,
    -- | @True@ iff the evaluator's pass/fail outcome matched 'scExpect'.
    rOk :: Bool,
    rCpu :: Integer,
    rMem :: Integer,
    rLogs :: [Text],
    rError :: Maybe Text
  }
  deriving (Eq, Show)

-- | Build the V3 evaluation context once at startup. Uses
-- @costModelParamsForTesting@ from plutus-ledger-api's testlib so we don't
-- have to commit a separate cost-model JSON in this repo.
makeEvaluationContext :: IO V3.EvaluationContext
makeEvaluationContext = do
  let paramValues = fmap snd TestEC.costModelParamsForTesting
  case runExcept (runWriterT (V3.mkEvaluationContext paramValues)) of
    Left err -> error ("Bench.Eval.makeEvaluationContext: " <> show err)
    Right (ec, _warn) -> pure ec

-- | Evaluate one scenario and produce a 'Result'.
runScenario :: V3.EvaluationContext -> Scenario -> Result
runScenario ec Scenario {scName, scExpect, scParams, scContextData} =
  case runExcept (V3.deserialiseScript futurePV serialised) of
    Left err -> failure ("deserialise: " <> T.pack (show err)) Nothing
    Right script ->
      let (logs, res) =
            V3.evaluateScriptCounting futurePV V3.Quiet ec script scContextData
          txtLogs = logs -- LogOutput = [Text]
       in case (res, scExpect) of
            (Right budget, Pass) ->
              toResult txtLogs (Just budget) True Nothing
            (Right budget, FailWith _) ->
              toResult
                txtLogs
                (Just budget)
                False
                (Just "expected failure but evaluation passed")
            (Left err, FailWith _) ->
              failureWithLogs txtLogs (Just (T.pack (show err)))
            (Left err, Pass) ->
              failureWithLogs' txtLogs (T.pack (show err))
  where
    serialised = toaV1ApplyConstantData scParams
    expectedText = case scExpect of
      Pass -> "Pass"
      FailWith reason -> "FailWith " <> reason

    toResult logs mBudget ok mErr =
      let (cpu, mem) = case mBudget of
            Just (ExBudget (ExCPU c) (ExMemory m)) ->
              (fromIntegral (unSatInt c), fromIntegral (unSatInt m))
            Nothing -> (0, 0)
       in Result
            { rName = scName,
              rExpected = expectedText,
              rOk = ok,
              rCpu = cpu,
              rMem = mem,
              rLogs = logs,
              rError = mErr
            }

    -- Evaluator returned an error AND the scenario expected failure.
    -- That's a match; record ok=True, no budget (none produced).
    failureWithLogs logs mErr =
      Result
        { rName = scName,
          rExpected = expectedText,
          rOk = True,
          rCpu = 0,
          rMem = 0,
          rLogs = logs,
          rError = mErr
        }

    -- Evaluator returned an error AND the scenario expected pass: real failure.
    failureWithLogs' logs err =
      Result
        { rName = scName,
          rExpected = expectedText,
          rOk = False,
          rCpu = 0,
          rMem = 0,
          rLogs = logs,
          rError = Just err
        }

    -- Pre-evaluation failure (deserialise error).
    failure msg _ =
      Result
        { rName = scName,
          rExpected = expectedText,
          rOk = False,
          rCpu = 0,
          rMem = 0,
          rLogs = [],
          rError = Just msg
        }
