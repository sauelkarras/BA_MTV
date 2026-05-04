module Generated where

import qualified Prelude

data Comparison =
   Eq
 | Lt
 | Gt

compOpp :: Comparison -> Comparison
compOpp r =
  case r of {
   Eq -> Eq;
   Lt -> Gt;
   Gt -> Lt}

sub :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer
sub = (\n m -> Prelude.max 0 (n Prelude.- m))

compare_cont :: Comparison -> Prelude.Integer -> Prelude.Integer ->
                Comparison
compare_cont r x y =
  (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
    (\p ->
    (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
      (\q -> compare_cont r p q)
      (\q -> compare_cont Gt p q)
      (\_ -> Gt)
      y)
    (\p ->
    (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
      (\q -> compare_cont Lt p q)
      (\q -> compare_cont r p q)
      (\_ -> Gt)
      y)
    (\_ ->
    (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
      (\_ -> Lt)
      (\_ -> Lt)
      (\_ -> r)
      y)
    x

compare :: Prelude.Integer -> Prelude.Integer -> Comparison
compare =
  compare_cont Eq

eqb :: Prelude.Integer -> Prelude.Integer -> Prelude.Bool
eqb p q =
  (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
    (\p0 ->
    (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
      (\q0 -> eqb p0 q0)
      (\_ -> Prelude.False)
      (\_ -> Prelude.False)
      q)
    (\p0 ->
    (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
      (\_ -> Prelude.False)
      (\q0 -> eqb p0 q0)
      (\_ -> Prelude.False)
      q)
    (\_ ->
    (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
      (\_ -> Prelude.False)
      (\_ -> Prelude.False)
      (\_ -> Prelude.True)
      q)
    p

compare0 :: Prelude.Integer -> Prelude.Integer -> Comparison
compare0 x y =
  (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
    (\_ ->
    (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
      (\_ -> Eq)
      (\_ -> Lt)
      (\_ -> Gt)
      y)
    (\x' ->
    (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
      (\_ -> Gt)
      (\y' -> compare x' y')
      (\_ -> Gt)
      y)
    (\x' ->
    (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
      (\_ -> Lt)
      (\_ -> Lt)
      (\y' -> compOpp (compare x' y'))
      y)
    x

leb :: Prelude.Integer -> Prelude.Integer -> Prelude.Bool
leb x y =
  case compare0 x y of {
   Gt -> Prelude.False;
   _ -> Prelude.True}

ltb :: Prelude.Integer -> Prelude.Integer -> Prelude.Bool
ltb x y =
  case compare0 x y of {
   Lt -> Prelude.True;
   _ -> Prelude.False}

eqb0 :: Prelude.Integer -> Prelude.Integer -> Prelude.Bool
eqb0 x y =
  (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
    (\_ ->
    (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
      (\_ -> Prelude.True)
      (\_ -> Prelude.False)
      (\_ -> Prelude.False)
      y)
    (\p ->
    (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
      (\_ -> Prelude.False)
      (\q -> eqb p q)
      (\_ -> Prelude.False)
      y)
    (\p ->
    (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
      (\_ -> Prelude.False)
      (\_ -> Prelude.False)
      (\q -> eqb p q)
      y)
    x

data Range =
   Build_Range Prelude.Integer Prelude.Integer

lo :: Range -> Prelude.Integer
lo r =
  case r of {
   Build_Range lo0 _ -> lo0}

hi :: Range -> Prelude.Integer
hi r =
  case r of {
   Build_Range _ hi0 -> hi0}

in_range :: Range -> Prelude.Integer -> Prelude.Bool
in_range rg z =
  (Prelude.&&) (leb (lo rg) z) (leb z (hi rg))

num_lt :: Prelude.Integer -> Prelude.Integer -> Prelude.Bool
num_lt =
  ltb

num_le :: Prelude.Integer -> Prelude.Integer -> Prelude.Bool
num_le =
  leb

num_gt :: Prelude.Integer -> Prelude.Integer -> Prelude.Bool
num_gt x c =
  ltb c x

num_ge :: Prelude.Integer -> Prelude.Integer -> Prelude.Bool
num_ge x c =
  leb c x

num_eq :: Prelude.Integer -> Prelude.Integer -> Prelude.Bool
num_eq =
  eqb0

num_neq :: Prelude.Integer -> Prelude.Integer -> Prelude.Bool
num_neq x c =
  Prelude.not (eqb0 x c)

cat_eq :: Prelude.String -> Prelude.String -> Prelude.Bool
cat_eq =
  ((Prelude.==) :: Prelude.String -> Prelude.String -> Prelude.Bool)

nat_abs :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer
nat_abs n m =
  case (Prelude.<=) n m of {
   Prelude.True -> sub m n;
   Prelude.False -> sub n m}

within_tolerance :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer ->
                    Prelude.Bool
within_tolerance exp obs tol =
  (Prelude.<=) (nat_abs exp obs) tol

