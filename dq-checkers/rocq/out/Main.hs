module Main where

import Prelude
import qualified System.Environment as Env
import qualified System.Exit        as Exit
import qualified Data.List          as L
import qualified Data.Char          as C
import qualified Data.Maybe         as M

-- Extracted Coq module
import qualified Generated as G

--------------------------------------------------------------------------------
-- Small utilities
--------------------------------------------------------------------------------

trim :: String -> String
trim = L.dropWhileEnd C.isSpace . dropWhile C.isSpace

-- super-simple CSV split (no quotes)
splitCommas :: String -> [String]
splitCommas "" = []
splitCommas s  =
  let (w, rest) = break (== ',') s
  in w : case rest of
            []     -> []
            (_:xs) -> splitCommas xs

readMaybeInteger :: String -> Maybe Integer
readMaybeInteger s =
  case reads s of
    [(n,"")] -> Just n
    _        -> Nothing

--------------------------------------------------------------------------------
-- CSV parsing: supports
--   - age,balance
--   - age,balance,duration,y
--   - age,balance,duration,y,education
--------------------------------------------------------------------------------

isHeader :: [String] -> Bool
isHeader xs =
  let lows = map (map C.toLower . trim) xs
  in case lows of
       ("age":_) -> True
       _         -> False

-- Build a Row, defaulting missing fields if needed
buildRow :: Integer -> Integer -> Integer -> String -> String -> G.Row
buildRow a b d ystr eduStr = G.Build_Row a b d ystr eduStr

parseRow :: String -> Maybe G.Row
parseRow ln =
  let cols = map trim (splitCommas ln)
  in if isHeader cols then Nothing else
     case cols of
       -- legacy: age,balance
       [aStr, bStr] ->
         case (readMaybeInteger aStr, readMaybeInteger bStr) of
           (Just a, Just b) -> Just (buildRow a b 0 "no" "unknown")
           _                -> Nothing
       -- age,balance,duration,y
       [aStr, bStr, dStr, yStr] ->
         case (readMaybeInteger aStr, readMaybeInteger bStr, readMaybeInteger dStr) of
           (Just a, Just b, Just d) ->
             let yNorm = map C.toLower (trim yStr)
             in Just (buildRow a b d yNorm "unknown")
           _ -> Nothing
       -- age,balance,duration,y,education
       [aStr, bStr, dStr, yStr, eduStr] ->
         case (readMaybeInteger aStr, readMaybeInteger bStr, readMaybeInteger dStr) of
           (Just a, Just b, Just d) ->
             let yNorm   = map C.toLower (trim yStr)
                 eduNorm = map C.toLower (trim eduStr)
             in Just (buildRow a b d yNorm eduNorm)
           _ -> Nothing
       _ -> Nothing

--------------------------------------------------------------------------------
-- Pretty printing
--------------------------------------------------------------------------------

showRow :: G.Row -> String
showRow r =
  "age="       ++ show (G.age r)
  ++ ", balance="   ++ show (G.balance r)
  ++ ", duration="  ++ show (G.duration r)
  ++ ", y="         ++ show (G.y r)
  ++ ", education=" ++ show (G.education r)

--------------------------------------------------------------------------------
-- Range checker
--------------------------------------------------------------------------------

okRowRange :: G.Row -> Bool
okRowRange r = G.rec_ok_range G.default_policy r

failsRange :: [G.Row] -> [(Int, G.Row)]
failsRange rs = [ (i,r) | (i,r) <- zip [0..] rs, not (okRowRange r) ]

summaryRange :: [G.Row] -> String
summaryRange rows =
  let total = length rows
      bad   = length (failsRange rows)
      pct   = if total == 0 then 0 else (bad * 100) `div` total
  in case total of
       0 -> "No data provided."
       _ | bad == 0  -> "✅ All datapoints pass the 'RangeChecker' check."
         | otherwise -> "❌ " ++ show bad ++ " (" ++ show pct
                        ++ "%) datapoints did not pass the 'RangeChecker' check."

--------------------------------------------------------------------------------
-- Contradiction checker (from PointwiseContradictions)
--------------------------------------------------------------------------------

okRowContr :: G.Row -> Bool
okRowContr r = G.rec_ok_contr r

failsContr :: [G.Row] -> [(Int, G.Row)]
failsContr rs = [ (i,r) | (i,r) <- zip [0..] rs, not (okRowContr r) ]

summaryContr :: [G.Row] -> String
summaryContr rows =
  let total = length rows
      bad   = length (failsContr rows)
      pct   = if total == 0 then 0 else (bad * 100) `div` total
  in case total of
       0 -> "No data provided."
       _ | bad == 0  -> "✅ All datapoints pass the 'ContradictionCheck' check."
         | otherwise -> "❌ " ++ show bad ++ " (" ++ show pct
                        ++ "%) datapoints did not pass the 'ContradictionCheck' check."

--------------------------------------------------------------------------------
-- Class-balance checker (education)
--------------------------------------------------------------------------------

summaryClass :: [G.Row] -> String
summaryClass rows =
  let (((cP, cS), cT), cU) = G.edu_counts rows   -- NOTE: nested pattern
      known = cP + cS + cT      -- only datapoints with known education
  in if known <= 0
       then
         "Class balance checker around the assumed proportions:\n"
         ++ "No datapoints with known education.\n"
         ++ "Result: Fail\n"
         ++ "This dataset does not satisfy the class-balance conditions."
       else
         let pct v = (100 * v) `div` known
             actP = pct cP
             actS = pct cS
             actT = pct cT
             header =
               "Class balance checker around the assumed proportions:\n"
               ++ "Class balance – highest level of education Assumed/Actual:\n"
             linesInfo =
               [ "primary education   20% / " ++ show actP ++ "%"
               , "secondary education 50% / " ++ show actS ++ "%"
               , "tertiary education  30% / " ++ show actT ++ "%" ]
             ok = G.ds_ok_class rows
             resLine = "Result: " ++ (if ok then "Success" else "Fail")
             explain =
               "This dataset "
               ++ (if ok then "does" else "does not")
               ++ " satisfy the class-balance conditions."
         in header
            ++ L.intercalate "\n" linesInfo ++ "\n"
            ++ resLine ++ "\n"
            ++ explain


--------------------------------------------------------------------------------
-- CLI
--------------------------------------------------------------------------------

usage :: IO a
usage = do
  putStrLn "Usage: ./run <csv-path>"
  Exit.exitFailure

main :: IO ()
main = do
  args <- Env.getArgs
  case args of
    [csvPath] -> do
      raw <- readFile csvPath
      let rows = M.mapMaybe parseRow (lines raw)

      -- Print summaries
      putStrLn (summaryRange rows)
      putStrLn (summaryContr rows)
      putStrLn (summaryClass rows)

      -- If anything failed, show offending rows per checker
      let badR = failsRange rows
          badC = failsContr rows

      if null badR && null badC
        then return ()
        else do
          if null badR
            then return ()
            else do
              putStrLn "Offending rows (RangeChecker):"
              mapM_ (\(i,r) -> putStrLn (show i ++ ": " ++ showRow r)) badR
          if null badC
            then return ()
            else do
              putStrLn "Offending rows (ContradictionCheck):"
              mapM_ (\(i,r) -> putStrLn (show i ++ ": " ++ showRow r)) badC
    _ -> usage
