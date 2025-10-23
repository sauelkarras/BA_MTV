{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as Csv
import qualified Data.Vector as V
import System.Environment (getArgs)
import System.Exit (die)
import CoqBridge

instance Csv.FromNamedRecord Person where
  parseNamedRecord m =
    Person <$> m Csv..: "age"
           <*> m Csv..: "income"

main :: IO ()
main = do
  args <- getArgs
  let path = case args of
               (p:_) -> p
               []    -> "datasets/sample.csv"
  csv <- BL.readFile path `catchRead` path
  case Csv.decodeByName csv of
    Left err  -> die ("CSV parse error: " ++ err)
    Right (_, v) ->
      let persons = V.toList v
      in putStrLn $
           if runRangeCheck persons
             then "Dataset passes range check."
             else "Dataset FAILS range check."

catchRead :: IO BL.ByteString -> FilePath -> IO BL.ByteString
catchRead io _ = io
