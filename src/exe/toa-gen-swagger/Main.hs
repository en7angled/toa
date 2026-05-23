-- | One-shot Swagger JSON generator for the TOA HTTP API.
--
-- Run with @cabal run toa-gen-swagger@ from the repo root; writes
-- @docs/generated/swagger/toa-api.json@. The TOA API server no longer
-- writes Swagger on startup; refresh the committed JSON with this exe
-- whenever the API shape changes.
module Main (main) where

import Api.Server (apiSwagger)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy.Char8 qualified as BL8
import System.Directory (createDirectoryIfMissing)

main :: IO ()
main = do
  let dir  = "docs/generated/swagger"
      path = dir <> "/toa-api.json"
  createDirectoryIfMissing True dir
  BL8.writeFile path (encodePretty apiSwagger)
  putStrLn $ "Wrote " <> path
