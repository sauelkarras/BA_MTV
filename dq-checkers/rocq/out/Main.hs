module Main where

import System.Environment (getArgs)
import qualified System.Process   as P
import qualified System.Directory as Dir
import qualified System.FilePath  as FP
import Text.Printf (printf)
import Data.Char (isDigit, isSpace)
import Data.List (isPrefixOf, groupBy, sortOn)
import Data.Function (on)

import qualified Generated as G

--------------------------------------------------------------------------------
-- Basic helpers
--------------------------------------------------------------------------------

splitComma :: String -> [String]
splitComma [] = [""]
splitComma (',' : cs) = "" : rest
  where
    rest = splitComma cs
splitComma (c : cs) =
  let rest = splitComma cs
  in (c : head rest) : tail rest

readIntegerSafe :: String -> Maybe Integer
readIntegerSafe s =
  case reads s of
    [(n, "")] -> Just n
    _         -> Nothing

readIntSafe :: String -> Maybe Int
readIntSafe s =
  case reads s of
    [(n, "")] -> Just n
    _         -> Nothing

readDoubleSafe :: String -> Maybe Double
readDoubleSafe s =
  case reads s of
    [(d, "")] -> Just d
    _         -> Nothing

trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace

--------------------------------------------------------------------------------
-- Dataset handling
--------------------------------------------------------------------------------

datasetCsvPath :: FilePath -> String -> Maybe FilePath
datasetCsvPath rocqRoot "german-credit" =
  Just (rocqRoot FP.</> "scripts" FP.</> "data" FP.</> "german_credit.csv")
datasetCsvPath rocqRoot "bank-marketing" =
  Just (rocqRoot FP.</> "scripts" FP.</> "data" FP.</> "bank_marketing.csv")
datasetCsvPath _ _ = Nothing

--------------------------------------------------------------------------------
-- UCI line parsing
--------------------------------------------------------------------------------

data UciSpec = UciSpec
  { uciVar :: String  -- e.g. "adult"
  , uciId  :: Int     -- e.g. 2
  } deriving (Show)

-- find the substring after "id=" if present
findIdRest :: String -> Maybe String
findIdRest [] = Nothing
findIdRest ('i':'d':'=':rest) = Just rest
findIdRest (_:cs) = findIdRest cs

parseUciLine :: String -> Either String UciSpec
parseUciLine s =
  case break (=='=') s of
    (lhs, '=':rhs) -> do
      let var = trim lhs
      if null var
        then Left "UCI line: missing variable name on the left of '='."
        else case findIdRest rhs of
          Nothing ->
            Left "UCI line: could not find 'id=' in the fetch_ucirepo call."
          Just rest ->
            let digits = takeWhile isDigit rest
            in case readIntSafe digits of
                 Nothing ->
                   Left ("UCI line: could not parse integer id from '" ++ digits ++ "'.")
                 Just n  ->
                   Right (UciSpec var n)
    _ ->
      Left "UCI line must contain a single '=' separating variable and fetch_ucirepo call."

--------------------------------------------------------------------------------
-- Range specs
--------------------------------------------------------------------------------

data RangeSpec = RangeSpec
  { rsAttr :: Int
  , rsLo   :: Maybe Integer
  , rsHi   :: Maybe Integer
  } deriving (Show)

parseRangeSpec :: String -> Either String RangeSpec
parseRangeSpec s =
  case splitComma s of
    [aStr, loStr, hiStr] ->
      case readIntSafe aStr of
        Nothing   -> Left ("Range spec: cannot parse attribute index from '" ++ aStr ++ "'")
        Just attr ->
          case (parseBound loStr, parseBound hiStr) of
            (Left e, _) -> Left e
            (_, Left e) -> Left e
            (Right loB, Right hiB) ->
              Right (RangeSpec attr loB hiB)
    _ -> Left ("Range spec must be of the form 'attr,lo,hi', got: " ++ s)
  where
    parseBound :: String -> Either String (Maybe Integer)
    parseBound "inf"  = Right Nothing
    parseBound "+inf" = Right Nothing
    parseBound "-inf" = Right Nothing
    parseBound str =
      case readIntegerSafe str of
        Just n  -> Right (Just n)
        Nothing -> Left ("Cannot parse bound as integer or inf: '" ++ str ++ "'")

--------------------------------------------------------------------------------
-- Contradiction specs
--------------------------------------------------------------------------------

data NumCmpOp = OpLt | OpLe | OpGt | OpGe | OpEq | OpNeq
  deriving (Show)

data Premise
  = PremNum { pmAttr :: Int, pmOp :: NumCmpOp, pmConst :: Integer }
  | PremCat { pmAttr :: Int, pmLabel :: String }
  deriving (Show)

data ForbKind = ForbIsEq | ForbIsNeq
  deriving (Show)

data ContrSpec = ContrSpec
  { csPrem      :: Premise
  , csForbAttr  :: Int
  , csForbKind  :: ForbKind
  , csForbLabel :: String
  } deriving (Show)

parseOp :: String -> Either String NumCmpOp
parseOp "<"  = Right OpLt
parseOp "<=" = Right OpLe
parseOp ">"  = Right OpGt
parseOp ">=" = Right OpGe
parseOp "="  = Right OpEq
parseOp "!=" = Right OpNeq
parseOp s    = Left ("Unknown comparison operator: " ++ s)

evalNumCmp :: NumCmpOp -> Integer -> Integer -> Bool
evalNumCmp op x c =
  case op of
    OpLt  -> G.num_lt  x c
    OpLe  -> G.num_le  x c
    OpGt  -> G.num_gt  x c
    OpGe  -> G.num_ge  x c
    OpEq  -> G.num_eq  x c
    OpNeq -> G.num_neq x c

-- split s into (left,right) at "=>"
splitOnArrow :: String -> Maybe (String, String)
splitOnArrow s = go "" s
  where
    go _ [] = Nothing
    go acc ('=':'>':rest) = Just (acc, rest)
    go acc (c:cs) = go (acc ++ [c]) cs

-- Parse left side: numeric or categorical premise.
--  numeric:     attr12<30, attr12>=5, ...
--  categorical: attr7=A75
parsePremise :: String -> Either String Premise
parsePremise s =
  if "attr" `isPrefixOf` s
    then
      let afterAttr = drop 4 s
          (idxStr, rest) = span isDigit afterAttr
      in if null idxStr
           then Left ("Premise: missing attribute index in '" ++ s ++ "'")
           else case readIntSafe idxStr of
             Nothing   -> Left ("Premise: cannot parse attribute index from '" ++ idxStr ++ "'")
             Just aIdx ->
               case rest of
                 '<':'=':cs -> parseNum aIdx "<=" cs
                 '>':'=':cs -> parseNum aIdx ">=" cs
                 '!':'=':cs -> parseNum aIdx "!=" cs
                 '<':cs     -> parseNum aIdx "<"  cs
                 '>':cs     -> parseNum aIdx ">"  cs
                 '=':cs     -> parseEq aIdx cs
                 _          -> Left ("Premise: cannot parse operator in '" ++ s ++ "'")
    else Left ("Premise must start with 'attr', got: '" ++ s ++ "'")
  where
    parseNum :: Int -> String -> String -> Either String Premise
    parseNum aIdx opStr cs =
      case parseOp opStr of
        Left e  -> Left e
        Right op ->
          case readIntegerSafe cs of
            Nothing -> Left ("Premise: cannot parse numeric constant from '" ++ cs ++ "'")
            Just c  -> Right (PremNum aIdx op c)

    -- "=" is ambiguous: numeric or categorical.
    parseEq :: Int -> String -> Either String Premise
    parseEq aIdx cs =
      case readIntegerSafe cs of
        Just c  -> Right (PremNum aIdx OpEq c)
        Nothing -> Right (PremCat aIdx cs)

-- parse right side "attrY!=LABEL" or "attrY=LABEL" (categorical)
parseForbCat :: String -> Either String (Int, ForbKind, String)
parseForbCat s =
  if "attr" `isPrefixOf` s
    then
      let afterAttr      = drop 4 s
          (idxStr, rest) = span isDigit afterAttr
      in if null idxStr
           then Left ("Forbidden part: missing attribute index in '" ++ s ++ "'")
           else case readIntSafe idxStr of
             Nothing   -> Left ("Forbidden part: cannot parse attribute index from '" ++ idxStr ++ "'")
             Just aIdx ->
               case rest of
                 '!':'=':lab ->
                   if null lab
                     then Left ("Forbidden part: missing label after '!=' in '" ++ s ++ "'")
                     else Right (aIdx, ForbIsNeq, lab)
                 '=':lab ->
                   if null lab
                     then Left ("Forbidden part: missing label after '=' in '" ++ s ++ "'")
                     else Right (aIdx, ForbIsEq, lab)
                 _ ->
                   Left ("Forbidden part must contain '=' or '!=', got: '" ++ s ++ "'")
    else Left ("Forbidden part must start with 'attr', got: '" ++ s ++ "'")

parseContrSpec :: String -> Either String ContrSpec
parseContrSpec s =
  case splitOnArrow s of
    Nothing -> Left ("Contr spec must contain '=>', got: " ++ s)
    Just (left, right) -> do
      prem                <- parsePremise left
      (aF, kindF, labF)   <- parseForbCat right
      pure (ContrSpec prem aF kindF labF)

--------------------------------------------------------------------------------
-- Class balance specs
--------------------------------------------------------------------------------

data ClassSpec = ClassSpec
  { cbAttrIdx    :: Int
  , cbLabel      :: String
  , cbExpShare   :: Double
  , cbTolerance  :: Double
  } deriving (Show)

parseClassSpec :: String -> Either String ClassSpec
parseClassSpec s =
  case splitComma s of
    [aStr, labStr, expStr, tolStr] -> do
      attr <- case readIntSafe aStr of
                Nothing -> Left ("Class spec: cannot parse attribute index from '" ++ aStr ++ "'")
                Just a  -> Right a
      expS <- case readDoubleSafe expStr of
                Nothing -> Left ("Class spec: cannot parse expected share from '" ++ expStr ++ "'")
                Just d  -> Right d
      tol  <- case readDoubleSafe tolStr of
                Nothing -> Left ("Class spec: cannot parse tolerance from '" ++ tolStr ++ "'")
                Just d  -> Right d
      pure (ClassSpec attr labStr expS tol)
    _ -> Left ("Class spec must be of the form 'attr,label,expected_share,tolerance', got: " ++ s)

--------------------------------------------------------------------------------
-- Global config
--------------------------------------------------------------------------------

data Config = Config
  { cfgDataset :: Maybe String   -- legacy named dataset
  , cfgUci     :: Maybe UciSpec  -- generic UCI source
  , cfgRanges  :: [RangeSpec]
  , cfgContrs  :: [ContrSpec]
  , cfgClasses :: [ClassSpec]
  } deriving (Show)

emptyConfig :: Config
emptyConfig = Config
  { cfgDataset = Nothing
  , cfgUci     = Nothing
  , cfgRanges  = []
  , cfgContrs  = []
  , cfgClasses = []
  }

parseArgs :: [String] -> Either String Config
parseArgs = go emptyConfig
  where
    go :: Config -> [String] -> Either String Config
    go cfg [] = Right cfg

    -- named dataset mode
    go cfg ("--dataset" : name : rest) =
      case cfgUci cfg of
        Just _  -> Left "Cannot use --dataset and --uci-line together."
        Nothing -> go cfg{ cfgDataset = Just name } rest

    -- generic UCI mode
    go cfg ("--uci-line" : line : rest) =
      case parseUciLine line of
        Left e  -> Left e
        Right u ->
          case cfgDataset cfg of
            Just _  -> Left "Cannot use --dataset and --uci-line together."
            Nothing -> go cfg{ cfgUci = Just u } rest

    -- range specs
    go cfg ("--range" : spec : rest) =
      case parseRangeSpec spec of
        Left e  -> Left e
        Right r -> go cfg{ cfgRanges = cfgRanges cfg ++ [r] } rest

    -- contradiction specs
    go cfg ("--contr" : spec : rest) =
      case parseContrSpec spec of
        Left e  -> Left e
        Right c -> go cfg{ cfgContrs = cfgContrs cfg ++ [c] } rest

    -- class balance specs
    go cfg ("--class" : spec : rest) =
      case parseClassSpec spec of
        Left e  -> Left e
        Right c -> go cfg{ cfgClasses = cfgClasses cfg ++ [c] } rest

    -- unknown flag
    go _ (flag : _) =
      Left ("Unknown or malformed argument near: " ++ flag)


--------------------------------------------------------------------------------
-- Range checking (Coq in_range + num_le/num_ge)
--------------------------------------------------------------------------------

evalRangeSpec :: RangeSpec -> Integer -> Bool
evalRangeSpec (RangeSpec _ loB hiB) v =
  case (loB, hiB) of
    (Just lo, Just hi) ->
      G.in_range (G.Build_Range lo hi) v
    (Just lo, Nothing) ->
      G.num_ge v lo
    (Nothing, Just hi) ->
      G.num_le v hi
    (Nothing, Nothing) ->
      True

runRangeCheck :: [String] -> [[String]] -> RangeSpec -> IO ()
runRangeCheck headers rows spec@(RangeSpec attrIdx loB hiB) = do
  let colIndex = attrIdx - 1
  putStrLn "-------------------------------------------------------"
  putStrLn $ "RANGE CHECK on attr" ++ show attrIdx
  putStrLn "-------------------------------------------------------"
  if colIndex < 0 || colIndex >= length headers
    then putStrLn $ "Error: attribute index " ++ show attrIdx ++ " is out of bounds."
    else do
      let colName = headers !! colIndex
      putStrLn $ "Column name:           " ++ colName
      putStrLn $ "Range specification:   " ++ showBound loB ++ " <= x <= " ++ showBound hiB

      let parseRow :: (Int, [String]) -> Either String (Integer, Int)
          parseRow (i, cols) =
            if length cols <= colIndex
              then Left ("Row " ++ show i ++ ": not enough columns")
              else
                let s = cols !! colIndex
                in case readIntegerSafe s of
                     Nothing ->
                       Left ("Row " ++ show i ++ ": cannot parse Integer from '" ++ s ++ "'")
                     Just v ->
                       Right (v, i)

          parsed       = map parseRow (zip [0..] rows)
          values       = [ v | Right (v, _) <- parsed ]
          indices      = [ i | Right (_, i) <- parsed ]
          parseErrors  = [ err | Left err <- parsed ]

          totalRows    = length rows
          parsedCount  = length values
          withIdx      = zip indices values
          okPairs      = [ (i,v) | (i,v) <- withIdx, evalRangeSpec spec v ]
          badPairs     = [ (i,v) | (i,v) <- withIdx, not (evalRangeSpec spec v) ]

          below        = [ (i,v) | (i,v) <- badPairs
                                 , case loB of
                                     Just lo -> v < lo
                                     Nothing -> False ]
          above        = [ (i,v) | (i,v) <- badPairs
                                 , case hiB of
                                     Just hi -> v > hi
                                     Nothing -> False ]

          violationCount = length badPairs
          minVal = if null values then Nothing else Just (minimum values)
          maxVal = if null values then Nothing else Just (maximum values)

          percViol :: Double
          percViol =
            if parsedCount == 0
              then 0
              else fromIntegral violationCount * 100.0 / fromIntegral parsedCount

          percPass :: Double
          percPass = 100.0 - percViol

      putStrLn ""
      putStrLn "Summary:"
      putStrLn $ "  Total rows:            " ++ show totalRows
      putStrLn $ "  Parsed numeric values: " ++ show parsedCount
      putStrLn $ "  Parse errors:          " ++ show (length parseErrors)

      putStrLn ""
      putStrLn "Column statistics (parsed values):"
      case (minVal, maxVal) of
        (Just mn, Just mx) -> do
          putStrLn $ "  min:                   " ++ show mn
          putStrLn $ "  max:                   " ++ show mx
        _ -> putStrLn "  no values"

      putStrLn ""
      putStrLn "Range check (Coq-based):"
      putStrLn $ "  values in range:       " ++ show (length okPairs)
      putStrLn $ "  violations total:      " ++ show violationCount
      putStrLn $ "    below lower bound:   " ++ show (length below)
      putStrLn $ "    above upper bound:   " ++ show (length above)
      putStrLn $ printf "  share in range:        %.2f%%" percPass
      putStrLn $ printf "  share violating:       %.2f%%" percViol

      if not (null parseErrors)
        then do
          putStrLn ""
          putStrLn "First few parse errors:"
          mapM_ putStrLn (take 5 parseErrors)
        else return ()

      putStrLn ""
      putStrLn "Violating rows (index, value):"
      if null badPairs
        then putStrLn "  (none)"
        else mapM_ (\(i,v) -> putStrLn $ "  " ++ show i ++ " -> " ++ show v) badPairs

  where
    showBound :: Maybe Integer -> String
    showBound (Just x) = show x
    showBound Nothing  = "inf"

--------------------------------------------------------------------------------
-- Contradiction checking
--------------------------------------------------------------------------------

runContrCheck :: [String] -> [[String]] -> ContrSpec -> IO ()
runContrCheck headers rows (ContrSpec prem forbAttr forbKind forbLabel) =
  case prem of
    PremNum pAttr op c ->
      runContrNumCat headers rows pAttr op c forbAttr forbKind forbLabel
    PremCat pAttr labP ->
      runContrCatCat headers rows pAttr labP forbAttr forbKind forbLabel

runContrNumCat :: [String] -> [[String]]
               -> Int -> NumCmpOp -> Integer
               -> Int -> ForbKind -> String
               -> IO ()
runContrNumCat headers rows premAttr op c forbAttr forbKind forbLabel = do
  let premIdx = premAttr - 1
      forbIdx = forbAttr - 1

  putStrLn "-------------------------------------------------------"
  putStrLn "POINTWISE CONTRADICTION CHECK (numeric premise)"
  putStrLn "-------------------------------------------------------"
  if premIdx < 0 || premIdx >= length headers
     || forbIdx < 0 || forbIdx >= length headers
    then putStrLn "Error: one of the column indices is out of bounds."
    else do
      let premName = headers !! premIdx
          forbName = headers !! forbIdx

      putStrLn $ "Premise column:          " ++ premName ++ " (attr" ++ show premAttr ++ ")"
      putStrLn $ "Premise:                 attr" ++ show premAttr
                                 ++ " " ++ showOp op ++ " " ++ show c
      putStrLn $ "Target column:           " ++ forbName ++ " (attr" ++ show forbAttr ++ ")"
      putStrLn $ "Target label:            " ++ forbLabel
      putStrLn ""
      putStrLn "Interpretation: property ="
      case forbKind of
        ForbIsNeq ->
          putStrLn "  if numeric premise holds, target column must NOT equal label."
        ForbIsEq  ->
          putStrLn "  if numeric premise holds, target column MUST equal label."
      putStrLn "Violation condition uses Coq num_* and Coq cat_eq."
      putStrLn ""

      let parseRow :: (Int, [String]) -> Either String (Integer, String, Int)
          parseRow (i, cols) =
            if length cols <= premIdx || length cols <= forbIdx
              then Left ("Row " ++ show i ++ ": not enough columns")
              else
                let premStr = cols !! premIdx
                    forbStr = cols !! forbIdx
                in case readIntegerSafe premStr of
                     Nothing ->
                       Left ("Row " ++ show i ++ ": cannot parse Integer from '"
                             ++ premStr ++ "' in premise column")
                     Just v ->
                       Right (v, forbStr, i)

          parsed      = map parseRow (zip [0..] rows)
          triples     = [ (vP, vF, i) | Right (vP, vF, i) <- parsed ]
          parseErrors = [ err         | Left err         <- parsed ]

          totalRows   = length rows
          parsedCount = length triples

          withFlags =
            [ (i, vP, vF, premHolds, forbEq, viol)
            | (vP, vF, i) <- triples
            , let premHolds = evalNumCmp op vP c
            , let forbEq    = G.cat_eq vF forbLabel
            , let viol      = case forbKind of
                                ForbIsNeq -> forbEq       -- require !=, violation if equal
                                ForbIsEq  -> not forbEq   -- require ==, violation if not equal
            ]

          premiseTrue    = [ (i,vP,vF) | (i,vP,vF,premH,_eq,_viol) <- withFlags, premH ]
          violations     = [ (i,vP,vF) | (i,vP,vF,premH,_eq,viol)  <- withFlags, premH && viol ]
          premiseCount   = length premiseTrue
          violationCount = length violations

          passed = null parseErrors && violationCount == 0

          percViol :: Double
          percViol =
            if premiseCount == 0
              then 0
              else fromIntegral violationCount * 100.0 / fromIntegral premiseCount

          percSafe :: Double
          percSafe =
            if premiseCount == 0
              then 100.0
              else 100.0 - percViol

      putStrLn "Summary:"
      putStrLn $ "  Total rows:              " ++ show totalRows
      putStrLn $ "  Parsed rows:             " ++ show parsedCount
      putStrLn $ "  Parse errors:            " ++ show (length parseErrors)
      putStrLn $ "  Rows with premise true:  " ++ show premiseCount
      putStrLn $ "  Violation rows:          " ++ show violationCount
      putStrLn $ printf "  Among premise-true rows, share violating: %.2f%%" percViol
      putStrLn $ printf "  Among premise-true rows, share OK:        %.2f%%" percSafe

      if not (null parseErrors)
        then do
          putStrLn ""
          putStrLn "First few parse errors:"
          mapM_ putStrLn (take 5 parseErrors)
        else return ()

      putStrLn ""
      putStrLn "Violating rows (index, premise-value, target-value):"
      if null violations
        then putStrLn "  (none)"
        else mapM_ (\(i,vP,vF) ->
                       putStrLn $ "  " ++ show i ++ " -> ("
                                  ++ show vP ++ ", " ++ vF ++ ")")
                   violations

      putStrLn ""
      putStrLn $ "CHECK STATUS:           " ++ (if passed then "PASS" else "FAIL")
  where
    showOp :: NumCmpOp -> String
    showOp OpLt  = "<"
    showOp OpLe  = "<="
    showOp OpGt  = ">"
    showOp OpGe  = ">="
    showOp OpEq  = "="
    showOp OpNeq = "!="

runContrCatCat :: [String] -> [[String]]
               -> Int -> String
               -> Int -> ForbKind -> String
               -> IO ()
runContrCatCat headers rows premAttr premLabel forbAttr forbKind forbLabel = do
  let premIdx = premAttr - 1
      forbIdx = forbAttr - 1

  putStrLn "-------------------------------------------------------"
  putStrLn "POINTWISE CONTRADICTION CHECK (categorical premise)"
  putStrLn "-------------------------------------------------------"
  if premIdx < 0 || premIdx >= length headers
     || forbIdx < 0 || forbIdx >= length headers
    then putStrLn "Error: one of the column indices is out of bounds."
    else do
      let premName = headers !! premIdx
          forbName = headers !! forbIdx

      putStrLn $ "Premise column:          " ++ premName ++ " (attr" ++ show premAttr ++ ")"
      putStrLn $ "Premise:                 attr" ++ show premAttr
                                 ++ " = " ++ premLabel
      putStrLn $ "Target column:           " ++ forbName ++ " (attr" ++ show forbAttr ++ ")"
      putStrLn $ "Target label:            " ++ forbLabel
      putStrLn ""
      putStrLn "Interpretation: property ="
      case forbKind of
        ForbIsNeq ->
          putStrLn "  if premise column == premLabel, target must NOT equal label."
        ForbIsEq  ->
          putStrLn "  if premise column == premLabel, target MUST equal label."
      putStrLn "Violation condition uses Coq cat_eq."
      putStrLn ""

      let parseRow :: (Int, [String]) -> Either String (String, String, Int)
          parseRow (i, cols) =
            if length cols <= premIdx || length cols <= forbIdx
              then Left ("Row " ++ show i ++ ": not enough columns")
              else
                let premStr = cols !! premIdx
                    forbStr = cols !! forbIdx
                in Right (premStr, forbStr, i)

          parsed      = map parseRow (zip [0..] rows)
          triples     = [ (vP, vF, i) | Right (vP, vF, i) <- parsed ]
          parseErrors = [ err         | Left err         <- parsed ]

          totalRows   = length rows
          parsedCount = length triples

          withFlags =
            [ (i, vP, vF, premEq, forbEq, viol)
            | (vP, vF, i) <- triples
            , let premEq = G.cat_eq vP premLabel
            , let forbEq = G.cat_eq vF forbLabel
            , let viol   = case forbKind of
                             ForbIsNeq -> forbEq      -- require !=, violation if equal
                             ForbIsEq  -> not forbEq  -- require ==, violation if not equal
            ]

          premiseTrue    = [ (i,vP,vF) | (i,vP,vF,premEq,_eq,_viol) <- withFlags, premEq ]
          violations     = [ (i,vP,vF) | (i,vP,vF,premEq,_eq,viol)  <- withFlags, premEq && viol ]
          premiseCount   = length premiseTrue
          violationCount = length violations

          passed = null parseErrors && violationCount == 0

          percViol :: Double
          percViol =
            if premiseCount == 0
              then 0
              else fromIntegral violationCount * 100.0 / fromIntegral premiseCount

          percSafe :: Double
          percSafe =
            if premiseCount == 0
              then 100.0
              else 100.0 - percViol

      putStrLn "Summary:"
      putStrLn $ "  Total rows:              " ++ show totalRows
      putStrLn $ "  Parsed rows:             " ++ show parsedCount
      putStrLn $ "  Parse errors:            " ++ show (length parseErrors)
      putStrLn $ "  Rows with premise true:  " ++ show premiseCount
      putStrLn $ "  Violation rows:          " ++ show violationCount
      putStrLn $ printf "  Among premise-true rows, share violating: %.2f%%" percViol
      putStrLn $ printf "  Among premise-true rows, share OK:        %.2f%%" percSafe

      if not (null parseErrors)
        then do
          putStrLn ""
          putStrLn "First few parse errors:"
          mapM_ putStrLn (take 5 parseErrors)
        else return ()

      putStrLn ""
      putStrLn "Violating rows (index, premise-value, target-value):"
      if null violations
        then putStrLn "  (none)"
        else mapM_ (\(i,vP,vF) ->
                       putStrLn $ "  " ++ show i ++ " -> ("
                                  ++ vP ++ ", " ++ vF ++ ")")
                   violations

      putStrLn ""
      putStrLn $ "CHECK STATUS:           " ++ (if passed then "PASS" else "FAIL")

--------------------------------------------------------------------------------
-- Class balance checking (Haskell aggregation, string column)
--------------------------------------------------------------------------------

runClassCheck :: [String] -> [[String]] -> ClassSpec -> IO ()
runClassCheck headers rows (ClassSpec attrIdx lab expShare tol) = do
  let colIndex = attrIdx - 1
  putStrLn "-------------------------------------------------------"
  putStrLn "CLASS BALANCE CHECK"
  putStrLn "-------------------------------------------------------"
  if colIndex < 0 || colIndex >= length headers
    then putStrLn $ "Error: attribute index " ++ show attrIdx ++ " is out of bounds."
    else do
      let colName    = headers !! colIndex
          indexed    = zip [0..] rows
          validRows  = [ (i, cols !! colIndex)
                      | (i, cols) <- indexed
                      , length cols > colIndex
                      ]
          totalRows  = length validRows
          posRows    = [ (i,v) | (i,v) <- validRows, v == lab ]
          posCount   = length posRows
          share      =
            if totalRows == 0
              then 0
              else fromIntegral posCount / fromIntegral totalRows
          diff       = abs (share - expShare)
          passed     = diff <= tol

      putStrLn $ "Column (attr):          " ++ colName ++ " (attr" ++ show attrIdx ++ ")"
      putStrLn $ "Label of interest:      " ++ lab
      putStrLn $ printf "Expected share:         %.4f" expShare
      putStrLn $ printf "Tolerance:              %.4f" tol
      putStrLn ""

      putStrLn "Summary:"
      putStrLn $ "  Total usable rows:     " ++ show totalRows
      putStrLn $ "  Count of label:        " ++ show posCount
      putStrLn $ printf "  Observed share:        %.4f" share
      putStrLn $ printf "  Absolute deviation:    %.4f" diff
      putStrLn ""
      putStrLn $ "CHECK STATUS:           " ++ (if passed then "PASS" else "FAIL")

      putStrLn ""
      putStrLn "First few rows with the label (index, value):"
      if null posRows
        then putStrLn "  (none)"
        else mapM_ (\(i,v) -> putStrLn $ "  " ++ show i ++ " -> " ++ v)
                   (take 20 posRows)

--------------------------------------------------------------------------------
-- Grouped class-balance overview
--------------------------------------------------------------------------------

runAllClassChecks :: [String] -> [[String]] -> [ClassSpec] -> IO ()
runAllClassChecks _ _ [] = return ()
runAllClassChecks headers rows specs = do
  let grouped = groupBy ((==) `on` cbAttrIdx) (sortOn cbAttrIdx specs)
  mapM_ (runGroup headers rows) grouped
  where
    runGroup :: [String] -> [[String]] -> [ClassSpec] -> IO ()
    runGroup _ _ [] = return ()
    runGroup hs rs specsForAttr@(spec0:_) = do
      let attr     = cbAttrIdx spec0
          colIndex = attr - 1

      putStrLn "-------------------------------------------------------"
      putStrLn "CLASS BALANCE OVERVIEW"
      putStrLn "-------------------------------------------------------"

      if colIndex < 0 || colIndex >= length hs
        then putStrLn $ "Error: attribute index " ++ show attr ++ " is out of bounds."
        else do
          let colName   = hs !! colIndex
              indexed   = zip [0..] rs
              validRows = [ (i, cols !! colIndex)
                          | (i, cols) <- indexed
                          , length cols > colIndex
                          ]
              totalRows = length validRows

          putStrLn $ "Column (attr):          " ++ colName ++ " (attr" ++ show attr ++ ")"
          putStrLn $ "Total usable rows:      " ++ show totalRows
          putStrLn ""
          putStrLn "Per-label targets:"

          let compute spec =
                let lab    = cbLabel spec
                    hits   = [ () | (_,v) <- validRows, v == lab ]
                    c      = length hits
                    share  = if totalRows == 0
                               then 0
                               else fromIntegral c / fromIntegral totalRows
                    diff   = abs (share - cbExpShare spec)
                    passed = diff <= cbTolerance spec
                in (spec, c, share, diff, passed)

              results = map compute specsForAttr
              anyFail = any (\(_,_,_,_,p) -> not p) results

          mapM_ (\(spec,c,share,diff,passed) -> do
                    putStrLn $ "  Label:                " ++ cbLabel spec
                    putStrLn $ printf "    expected:           %.4f ± %.4f"
                                      (cbExpShare spec) (cbTolerance spec)
                    putStrLn $ printf "    observed:           %.4f (%d rows)" share c
                    putStrLn $ printf "    deviation:          %.4f" diff
                    putStrLn $ "    status:             " ++ (if passed then "PASS" else "FAIL")
                 ) results

          putStrLn ""
          putStrLn $ "OVERVIEW STATUS:       " ++ (if anyFail then "FAIL" else "PASS")
          putStrLn ""

          -- Detailed per-label checks as before
          mapM_ (runClassCheck hs rs) specsForAttr

--------------------------------------------------------------------------------
-- Main driver
--------------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Left msg -> do
      putStrLn "Error parsing arguments:"
      putStrLn ("  " ++ msg)
      putStrLn ""
      printUsage
    Right cfg -> do
      case (cfgDataset cfg, cfgUci cfg) of
        (Nothing, Nothing) -> do
          putStrLn "No dataset specified."
          printUsage

        (Just datasetName, Nothing) -> do
          cwd <- Dir.getCurrentDirectory
          let rocqRoot = FP.takeDirectory cwd
              script   = rocqRoot FP.</> "scripts" FP.</> "fetch_dataset.py"
          case datasetCsvPath rocqRoot datasetName of
            Nothing -> putStrLn $ "Unknown dataset: " ++ datasetName
            Just csvPath -> do
              putStrLn "======================================================="
              putStrLn "                      MTV CHECKER"
              putStrLn "======================================================="
              putStrLn $ "Dataset: " ++ datasetName
              putStrLn ""

              putStrLn $ "Fetching dataset '" ++ datasetName ++ "' via Python..."
              _ <- P.rawSystem "python3" [script, "--dataset", datasetName]

              putStrLn $ "Reading CSV from: " ++ csvPath
              contents <- readFile csvPath
              let ls = lines contents
              case ls of
                [] -> putStrLn "Empty CSV file."
                (headerLine : rowLines) -> do
                  let headers = splitComma headerLine
                      rows    = map splitComma rowLines

                  putStrLn $ "Header columns: " ++ show headers
                  putStrLn ""

                  mapM_ (runRangeCheck  headers rows) (cfgRanges  cfg)
                  mapM_ (runContrCheck  headers rows) (cfgContrs  cfg)
                  runAllClassChecks headers rows (cfgClasses cfg)

                  putStrLn "======================================================="

        (Nothing, Just uci) -> do
          cwd <- Dir.getCurrentDirectory
          let rocqRoot = FP.takeDirectory cwd
              script   = rocqRoot FP.</> "scripts" FP.</> "fetch_dataset.py"
              baseName = uciVar uci
              csvPath  = rocqRoot FP.</> "scripts" FP.</> "data" FP.</> (baseName ++ ".csv")

          putStrLn "======================================================="
          putStrLn "                      MTV CHECKER"
          putStrLn "======================================================="
          putStrLn $ "Dataset (UCI): " ++ baseName
                     ++ " (id=" ++ show (uciId uci) ++ ")"
          putStrLn ""

          putStrLn $ "Fetching dataset via Python (id="
                     ++ show (uciId uci) ++ ", name='" ++ baseName ++ "')..."
          _ <- P.rawSystem "python3"
                 [script, "--uci-id", show (uciId uci), "--uci-name", baseName]

          putStrLn $ "Reading CSV from: " ++ csvPath
          contents <- readFile csvPath
          let ls = lines contents
          case ls of
            [] -> putStrLn "Empty CSV file."
            (headerLine : rowLines) -> do
              let headers = splitComma headerLine
                  rows    = map splitComma rowLines

              putStrLn $ "Header columns: " ++ show headers
              putStrLn ""

              mapM_ (runRangeCheck  headers rows) (cfgRanges  cfg)
              mapM_ (runContrCheck  headers rows) (cfgContrs  cfg)
              runAllClassChecks headers rows (cfgClasses cfg)

              putStrLn "======================================================="

        (Just _, Just _) -> do
          -- Should be ruled out by parseArgs, but keep a guard.
          putStrLn "Internal error: both dataset and UCI source set."

printUsage :: IO ()
printUsage = do
  putStrLn "Usage:"
  putStrLn "  ./run --dataset <name> [--range \"attr,lo,hi\"]... [--contr \"...\"]... [--class \"...\"]..."
  putStrLn "  ./run --uci-line \"var = fetch_ucirepo(id=N)\" [--range ...] [--contr ...] [--class ...]"
  putStrLn ""
  putStrLn "Datasets (named mode):"
  putStrLn "  german-credit"
  putStrLn "  bank-marketing"
  putStrLn ""
  putStrLn "Generic UCI mode:"
  putStrLn "  --uci-line \"adult = fetch_ucirepo(id=2)\""
  putStrLn "    var : variable name (used as CSV base name)"
  putStrLn "    id  : UCI id passed to fetch_ucirepo(id=...)"
  putStrLn ""
  putStrLn "Range checks:"
  putStrLn "  --range \"attr,lo,hi\""
  putStrLn "    attr : 1-based column index"
  putStrLn "    lo   : integer or -inf"
  putStrLn "    hi   : integer or inf"
  putStrLn ""
  putStrLn "Contradiction checks:"
  putStrLn "  Numeric premise -> categorical target:"
  putStrLn "    --contr \"attrP<k=>attrY!=LABEL\"   (require Y != LABEL)"
  putStrLn "    --contr \"attrP<k=>attrY=LABEL\"    (require Y = LABEL)"
  putStrLn ""
  putStrLn "  Categorical premise -> categorical target:"
  putStrLn "    --contr \"attrX=LAB1=>attrY!=LAB2\" (require Y != LAB2)"
  putStrLn "    --contr \"attrX=LAB1=>attrY=LAB2\"  (require Y = LAB2)"
  putStrLn ""
  putStrLn "Class-balance checks (binary-style):"
  putStrLn "  --class \"attr,label,expected_share,tolerance\""
  putStrLn "    attr           : 1-based column index"
  putStrLn "    label          : the class label to track"
  putStrLn "    expected_share : in [0,1], e.g. 0.5"
  putStrLn "    tolerance      : non-negative, e.g. 0.1"
  putStrLn "  You can repeat --class for the same column to get a combined overview."
  putStrLn ""
  putStrLn "Property semantics:"
  putStrLn "  Range:      values must lie in the given interval (with Coq comparators)."
  putStrLn "  Contradict: if premise holds, the stated requirement on target label must hold."
  putStrLn "  Class:      observed share of the label must be within [expected_share±tolerance]."
