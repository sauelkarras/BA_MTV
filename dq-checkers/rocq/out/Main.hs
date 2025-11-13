module Main where

import Prelude
import Text.Printf (printf)
import qualified System.Environment as Env
import qualified System.Exit        as Exit
import qualified Data.List          as L
import qualified Data.Char          as C
import qualified Data.Maybe         as M

-- Extracted Coq module
import qualified Generated as G

-- ------------------------------------------------------------
-- Small utilities
-- ------------------------------------------------------------
trim :: String -> String
trim = L.dropWhileEnd C.isSpace . dropWhile C.isSpace

splitCommas :: String -> [String]
splitCommas "" = []
splitCommas s  =
  let (w, rest) = break (== ',') s
  in w : case rest of [] -> []; (_:xs) -> splitCommas xs

readMaybeInteger :: String -> Maybe Integer
readMaybeInteger s = case reads s of [(n,"")] -> Just n; _ -> Nothing

lower :: String -> String
lower = map C.toLower

isHeaderLine :: String -> Bool
isHeaderLine ln =
  case map lower (splitCommas ln) of
    ("age":_) -> True
    _         -> False

findCol :: [String] -> String -> Maybe Int
findCol hdr keySub =
  let key  = lower (trim keySub)
      norm = lower . trim
  in L.findIndex (\h -> key `L.isInfixOf` norm h) hdr

readHeader :: String -> [String]
readHeader = splitCommas

getAt :: [String] -> Int -> String
getAt cs i = if i < length cs then trim (cs !! i) else ""

-- ------------------------------------------------------------
-- Build Row (10 fields after re-extraction with a9,a20)
--   Build_Row age balance duration a1 a3 a6 a9 a10 a20 y
-- ------------------------------------------------------------
buildRow10 :: Integer -> Integer -> Integer
           -> String -> String -> String
           -> String -> String -> String
           -> String -> G.Row
buildRow10 a b d a1 a3 a6 a9 a10 a20 ystr =
  G.Build_Row a b d a1 a3 a6 a9 a10 a20 ystr

-- Legacy (pre-a9/a20) constructor adapter: fill missing with ""
buildRow8 :: Integer -> Integer -> Integer
          -> String -> String -> String -> String -> String
          -> G.Row
buildRow8 a b d a1 a3 a6 a10 ystr =
  buildRow10 a b d a1 a3 a6 "" a10 "" ystr

-- ------------------------------------------------------------
-- Parsing (header-driven; with robust fallback)
-- ------------------------------------------------------------

-- Hardened header parser: accepts "a9"/"a20" or descriptive names; falls back to
-- positional 10-column order if header info is insufficient.
parseRowWithHeader :: [String] -> String -> Maybe G.Row
parseRowWithHeader hdr ln =
  let cols = map trim (splitCommas ln)
      readZ s = case reads s of [(n,"")] -> n; _ -> 0

      at s = maybe "" (getAt cols) (findCol hdr s)
      atFirst keys =
        let vals = [ at k | k <- keys ]
        in case filter (not . null) vals of (v:_) -> v; [] -> ""

      -- header-first attempt
      a    = readZ (atFirst ["age"])
      b    = readZ (atFirst ["balance","amount","credit amount"])
      d    = readZ (atFirst ["duration"])
      a1s  = map C.toUpper (atFirst ["a1","status of existing checking account"])
      a3s  = map C.toUpper (atFirst ["a3","credit history"])
      a6s  = map C.toUpper (atFirst ["a6","savings"])
      a9s  = map C.toUpper (atFirst ["a9","personal_status","personal status and sex","personal status"])
      a10s = map C.toUpper (atFirst ["a10","other debtors"])
      a20s = map C.toUpper (atFirst ["a20","foreign","foreign worker"])
      ys   = trim (atFirst ["y","class","risk","creditability"])

      headerRow =
        if null cols then Nothing
        else Just (buildRow10 a b d a1s a3s a6s a9s a10s a20s ys)

      -- positional fallback for exact 10-column rows in canonical order
      atIx i = if i < length cols then cols !! i else ""
      fallback10 =
        if length cols == 10
          then Just (buildRow10
                (readZ (atIx 0))                 -- age
                (readZ (atIx 1))                 -- balance
                (readZ (atIx 2))                 -- duration
                (map C.toUpper (atIx 3))         -- a1
                (map C.toUpper (atIx 4))         -- a3
                (map C.toUpper (atIx 5))         -- a6
                (map C.toUpper (atIx 6))         -- a9
                (map C.toUpper (atIx 7))         -- a10
                (map C.toUpper (atIx 8))         -- a20
                (trim (atIx 9)))                  -- y
          else Nothing
  in case headerRow of
       Just r | not (null (a9s ++ a20s)) -> Just r
       _                                 -> fallback10

-- Legacy “no header” parser for older CSVs (kept for compatibility)
parseRowNoHeader :: String -> Maybe G.Row
parseRowNoHeader ln =
  let cols = map trim (splitCommas ln)
      norm s = map C.toUpper (trim s)
  in case cols of
      [aStr, bStr, dStr, yStr] ->
        case (readMaybeInteger aStr, readMaybeInteger bStr, readMaybeInteger dStr) of
          (Just a, Just b, Just d) -> Just (buildRow8 a b d "" "" "" "" (trim yStr))
          _                        -> Nothing
      [aStr, bStr, dStr, a1s, a3s, a6s, yStr] ->
        case (readMaybeInteger aStr, readMaybeInteger bStr, readMaybeInteger dStr) of
          (Just a, Just b, Just d) -> Just (buildRow8 a b d (norm a1s) (norm a3s) (norm a6s) "" (trim yStr))
          _ -> Nothing
      [aStr, bStr, dStr, a1s, a3s, a6s, a10s, yStr] ->
        case (readMaybeInteger aStr, readMaybeInteger bStr, readMaybeInteger dStr) of
          (Just a, Just b, Just d) -> Just (buildRow8 a b d (norm a1s) (norm a3s) (norm a6s) (norm a10s) (trim yStr))
          _ -> Nothing
      [aStr, bStr, dStr, a1s, a3s, a6s, a9s, a10s, a20s, yStr] ->
        case (readMaybeInteger aStr, readMaybeInteger bStr, readMaybeInteger dStr) of
          (Just a, Just b, Just d) ->
            Just (buildRow10 a b d (norm a1s) (norm a3s) (norm a6s) (norm a9s) (norm a10s) (norm a20s) (trim yStr))
          _ -> Nothing
      _ -> Nothing

parseRow :: Maybe [String] -> String -> Maybe G.Row
parseRow (Just hdr) = parseRowWithHeader hdr
parseRow Nothing    = parseRowNoHeader

-- ------------------------------------------------------------
-- Pretty printing
-- ------------------------------------------------------------
showRow :: G.Row -> String
showRow r =
  "age=" ++ show (G.age r)
  ++ ", balance="  ++ show (G.balance r)
  ++ ", duration=" ++ show (G.duration r)
  ++ ", a1="       ++ show (G.a1 r)
  ++ ", a3="       ++ show (G.a3 r)
  ++ ", a6="       ++ show (G.a6 r)
  ++ ", a9="       ++ show (G.a9 r)
  ++ ", a10="      ++ show (G.a10 r)
  ++ ", a20="      ++ show (G.a20 r)
  ++ ", y="        ++ show (G.y r)

-- ------------------------------------------------------------
-- Range checker
-- ------------------------------------------------------------
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

-- ------------------------------------------------------------
-- Contradiction checker
-- ------------------------------------------------------------
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

-- ------------------------------------------------------------
-- Class-balance (formatted summary)
-- ------------------------------------------------------------
summaryClassBalance :: [G.Row] -> [String]
summaryClassBalance rows =
  let sx    = G.sex_diag_default rows
      fx    = G.foreign_diag_default rows
      allOK = G.all_balance_ok_default rows

      nS    = G.n_total sx
      xS    = G.x_pos sx
      nF    = G.n_total fx
      xF    = G.x_pos fx

      pct a n = if n > 0 then (fromIntegral a :: Double) * 100.0 / fromIntegral n else 0.0
      showPct d = printf "%.1f%%" d

      outGender =
        if nS == 0
          then [ "Class balance – gender:"
               , "Assumed: 50/50"
               , "Actual: (no usable values for A91–A95 found)"
               , "Result: fail" ]
          else let pM = pct xS nS; pF = 100 - pM
               in [ "Class balance – gender:"
                  , "Assumed: 50/50"
                  , "Actual: male " ++ showPct pM ++ " / female " ++ showPct pF
                  , "Result: " ++ (if G.passed sx then "pass" else "fail") ]

      outForeign =
        if nF == 0
          then [ "Class balance – foreign worker:"
               , "Assumed: 4.5/95.5"
               , "Actual: (no usable values for A201/A202 found)"
               , "Result: fail" ]
          else let pY = pct xF nF; pN = 100 - pY
               in [ "Class balance – foreign worker:"
                  , "Assumed: 4.5/95.5"
                  , "Actual: yes " ++ showPct pY ++ " / no " ++ showPct pN
                  , "Result: " ++ (if G.passed fx then "pass" else "fail") ]

      header  = "Class balance checker by a standard-deviation band around the assumed proportions"
      overall = if allOK
                then "This dataset satisfies the class-balance conditions."
                else "This dataset does not satisfy the class-balance conditions."
  in [ header, "" ] ++ outGender ++ [ "" ] ++ outForeign ++ [ "", overall ]

-- ------------------------------------------------------------
-- CLI
-- ------------------------------------------------------------
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
      let lns      = lines raw
          mHeader  = case lns of
                       (h:_) | isHeaderLine h -> Just (readHeader h)
                       _                      -> Nothing
          body     = case mHeader of
                       Just _  -> drop 1 lns
                       Nothing -> lns
          rows     = M.mapMaybe (parseRow mHeader) body

      putStrLn (summaryRange rows)
      putStrLn (summaryContr rows)
      putStrLn $ "Using k = " ++ show G.standard_deviations_boundary ++ " SD"

      let badR = failsRange rows
          badC = failsContr rows

      if null badR && null badC
        then pure ()
        else do
          if not (null badR) then do
            putStrLn "Offending rows (RangeChecker):"
            mapM_ (\(i,r) -> putStrLn (show i ++ ": " ++ showRow r)) badR
          else pure ()
          if not (null badC) then do
            putStrLn "Offending rows (ContradictionCheck):"
            mapM_ (\(i,r) -> putStrLn (show i ++ ": " ++ showRow r)) badC
          else pure ()

      putStrLn "=== Class-balance checks (SD band; defaults from ClassBalance.v) ==="
      mapM_ putStrLn (summaryClassBalance rows)
    _ -> usage
