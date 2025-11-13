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

succ :: Prelude.Integer -> Prelude.Integer
succ x =
  (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
    (\p -> (\x -> 2 Prelude.* x) (succ p))
    (\p -> (\x -> 2 Prelude.* x Prelude.+ 1) p)
    (\_ -> (\x -> 2 Prelude.* x) 1)
    x

add :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer
add x y0 =
  (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
    (\p ->
    (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
      (\q -> (\x -> 2 Prelude.* x) (add_carry p q))
      (\q -> (\x -> 2 Prelude.* x Prelude.+ 1) (add p q))
      (\_ -> (\x -> 2 Prelude.* x) (succ p))
      y0)
    (\p ->
    (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
      (\q -> (\x -> 2 Prelude.* x Prelude.+ 1) (add p q))
      (\q -> (\x -> 2 Prelude.* x) (add p q))
      (\_ -> (\x -> 2 Prelude.* x Prelude.+ 1) p)
      y0)
    (\_ ->
    (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
      (\q -> (\x -> 2 Prelude.* x) (succ q))
      (\q -> (\x -> 2 Prelude.* x Prelude.+ 1) q)
      (\_ -> (\x -> 2 Prelude.* x) 1)
      y0)
    x

add_carry :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer
add_carry x y0 =
  (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
    (\p ->
    (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
      (\q -> (\x -> 2 Prelude.* x Prelude.+ 1) (add_carry p q))
      (\q -> (\x -> 2 Prelude.* x) (add_carry p q))
      (\_ -> (\x -> 2 Prelude.* x Prelude.+ 1) (succ p))
      y0)
    (\p ->
    (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
      (\q -> (\x -> 2 Prelude.* x) (add_carry p q))
      (\q -> (\x -> 2 Prelude.* x Prelude.+ 1) (add p q))
      (\_ -> (\x -> 2 Prelude.* x) (succ p))
      y0)
    (\_ ->
    (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
      (\q -> (\x -> 2 Prelude.* x Prelude.+ 1) (succ q))
      (\q -> (\x -> 2 Prelude.* x) (succ q))
      (\_ -> (\x -> 2 Prelude.* x Prelude.+ 1) 1)
      y0)
    x

pred_double :: Prelude.Integer -> Prelude.Integer
pred_double x =
  (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
    (\p -> (\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x) p))
    (\p -> (\x -> 2 Prelude.* x Prelude.+ 1) (pred_double p))
    (\_ -> 1)
    x

mul :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer
mul x y0 =
  (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
    (\p -> add y0 ((\x -> 2 Prelude.* x) (mul p y0)))
    (\p -> (\x -> 2 Prelude.* x) (mul p y0))
    (\_ -> y0)
    x

compare_cont :: Comparison -> Prelude.Integer -> Prelude.Integer ->
                Comparison
compare_cont r x y0 =
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
      y0)
    (\p ->
    (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
      (\q -> compare_cont Lt p q)
      (\q -> compare_cont r p q)
      (\_ -> Gt)
      y0)
    (\_ ->
    (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
      (\_ -> Lt)
      (\_ -> Lt)
      (\_ -> r)
      y0)
    x

compare :: Prelude.Integer -> Prelude.Integer -> Comparison
compare =
  compare_cont Eq

of_succ_nat :: Prelude.Integer -> Prelude.Integer
of_succ_nat n =
  (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
    (\_ -> 1)
    (\x -> succ (of_succ_nat x))
    n

forallb :: (a1 -> Prelude.Bool) -> (([]) a1) -> Prelude.Bool
forallb f l =
  case l of {
   ([]) -> Prelude.True;
   (:) a l0 -> (Prelude.&&) (f a) (forallb f l0)}

double :: Prelude.Integer -> Prelude.Integer
double x =
  (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
    (\_ -> 0)
    (\p -> (\x -> x) ((\x -> 2 Prelude.* x) p))
    (\p -> Prelude.negate ((\x -> 2 Prelude.* x) p))
    x

succ_double :: Prelude.Integer -> Prelude.Integer
succ_double x =
  (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
    (\_ -> (\x -> x) 1)
    (\p -> (\x -> x) ((\x -> 2 Prelude.* x Prelude.+ 1) p))
    (\p -> Prelude.negate (pred_double p))
    x

pred_double0 :: Prelude.Integer -> Prelude.Integer
pred_double0 x =
  (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
    (\_ -> Prelude.negate 1)
    (\p -> (\x -> x) (pred_double p))
    (\p -> Prelude.negate ((\x -> 2 Prelude.* x Prelude.+ 1) p))
    x

pos_sub :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer
pos_sub x y0 =
  (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
    (\p ->
    (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
      (\q -> double (pos_sub p q))
      (\q -> succ_double (pos_sub p q))
      (\_ -> (\x -> x) ((\x -> 2 Prelude.* x) p))
      y0)
    (\p ->
    (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
      (\q -> pred_double0 (pos_sub p q))
      (\q -> double (pos_sub p q))
      (\_ -> (\x -> x) (pred_double p))
      y0)
    (\_ ->
    (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
      (\q -> Prelude.negate ((\x -> 2 Prelude.* x) q))
      (\q -> Prelude.negate (pred_double q))
      (\_ -> 0)
      y0)
    x

opp :: Prelude.Integer -> Prelude.Integer
opp x =
  (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
    (\_ -> 0)
    (\x0 -> Prelude.negate x0)
    (\x0 -> (\x -> x) x0)
    x

compare0 :: Prelude.Integer -> Prelude.Integer -> Comparison
compare0 x y0 =
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
      y0)
    (\x' ->
    (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
      (\_ -> Gt)
      (\y' -> compare x' y')
      (\_ -> Gt)
      y0)
    (\x' ->
    (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
      (\_ -> Lt)
      (\_ -> Lt)
      (\y' -> compOpp (compare x' y'))
      y0)
    x

leb :: Prelude.Integer -> Prelude.Integer -> Prelude.Bool
leb x y0 =
  case compare0 x y0 of {
   Gt -> Prelude.False;
   _ -> Prelude.True}

of_nat :: Prelude.Integer -> Prelude.Integer
of_nat n =
  (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
    (\_ -> 0)
    (\n0 -> (\x -> x) (of_succ_nat n0))
    n

data Row =
   Build_Row Prelude.Integer Prelude.Integer Prelude.Integer Prelude.String 
 Prelude.String Prelude.String Prelude.String Prelude.String Prelude.String 
 Prelude.String

age :: Row -> Prelude.Integer
age r =
  case r of {
   Build_Row age0 _ _ _ _ _ _ _ _ _ -> age0}

balance :: Row -> Prelude.Integer
balance r =
  case r of {
   Build_Row _ balance0 _ _ _ _ _ _ _ _ -> balance0}

duration :: Row -> Prelude.Integer
duration r =
  case r of {
   Build_Row _ _ duration0 _ _ _ _ _ _ _ -> duration0}

a1 :: Row -> Prelude.String
a1 r =
  case r of {
   Build_Row _ _ _ a2 _ _ _ _ _ _ -> a2}

a3 :: Row -> Prelude.String
a3 r =
  case r of {
   Build_Row _ _ _ _ a4 _ _ _ _ _ -> a4}

a6 :: Row -> Prelude.String
a6 r =
  case r of {
   Build_Row _ _ _ _ _ a7 _ _ _ _ -> a7}

a9 :: Row -> Prelude.String
a9 r =
  case r of {
   Build_Row _ _ _ _ _ _ a11 _ _ _ -> a11}

a10 :: Row -> Prelude.String
a10 r =
  case r of {
   Build_Row _ _ _ _ _ _ _ a11 _ _ -> a11}

a20 :: Row -> Prelude.String
a20 r =
  case r of {
   Build_Row _ _ _ _ _ _ _ _ a21 _ -> a21}

y :: Row -> Prelude.String
y r =
  case r of {
   Build_Row _ _ _ _ _ _ _ _ _ y0 -> y0}

type Dataset = ([]) Row

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

data Policy =
   Build_Policy Range Range Range

ageRange :: Policy -> Range
ageRange p =
  case p of {
   Build_Policy ageRange0 _ _ -> ageRange0}

amountRange :: Policy -> Range
amountRange p =
  case p of {
   Build_Policy _ amountRange0 _ -> amountRange0}

durationRange :: Policy -> Range
durationRange p =
  case p of {
   Build_Policy _ _ durationRange0 -> durationRange0}

default0 :: Policy
default0 =
  Build_Policy (Build_Range ((\x -> x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) 1))))) ((\x -> x)
    ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x) 1))))))))
    (Build_Range 0 ((\x -> x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x Prelude.+ 1)
    ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) 1))))))))))))))) (Build_Range ((\x -> x) 1)
    ((\x -> x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x Prelude.+ 1)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) 1))))))))

in_range :: Range -> Prelude.Integer -> Prelude.Bool
in_range rg z =
  (Prelude.&&) (leb (lo rg) z) (leb z (hi rg))

check_rec_range :: Policy -> Row -> Prelude.Bool
check_rec_range p r =
  (Prelude.&&)
    ((Prelude.&&) (in_range (ageRange p) (age r))
      (in_range (amountRange p) (balance r)))
    (in_range (durationRange p) (duration r))

check_ds_range :: Policy -> Dataset -> Prelude.Bool
check_ds_range p ds =
  forallb (check_rec_range p) ds

data ContrCfg =
   Build_ContrCfg (([]) Prelude.String) (([]) Prelude.String) (([])
                                                              Prelude.String) 
 (([]) Prelude.String) Prelude.String

a1_bad :: ContrCfg -> ([]) Prelude.String
a1_bad c =
  case c of {
   Build_ContrCfg a1_bad0 _ _ _ _ -> a1_bad0}

a3_bad :: ContrCfg -> ([]) Prelude.String
a3_bad c =
  case c of {
   Build_ContrCfg _ a3_bad0 _ _ _ -> a3_bad0}

a6_bad :: ContrCfg -> ([]) Prelude.String
a6_bad c =
  case c of {
   Build_ContrCfg _ _ a6_bad0 _ _ -> a6_bad0}

a10_req :: ContrCfg -> ([]) Prelude.String
a10_req c =
  case c of {
   Build_ContrCfg _ _ _ a10_req0 _ -> a10_req0}

bad_lbl :: ContrCfg -> Prelude.String
bad_lbl c =
  case c of {
   Build_ContrCfg _ _ _ _ bad_lbl0 -> bad_lbl0}

default_contr_cfg :: ContrCfg
default_contr_cfg =
  Build_ContrCfg ((:) "A11" ((:) "A14" ([]))) ((:) "A33" ((:) "A34" ([])))
    ((:) "A61" ((:) "A65" ([]))) ((:) "A101" ([])) "2"

str_in :: Prelude.String -> (([]) Prelude.String) -> Prelude.Bool
str_in x xs =
  case xs of {
   ([]) -> Prelude.False;
   (:) y0 ys ->
    case ((Prelude.==) :: Prelude.String -> Prelude.String -> Prelude.Bool) x
           y0 of {
     Prelude.True -> Prelude.True;
     Prelude.False -> str_in x ys}}

antecedent :: ContrCfg -> Row -> Prelude.Bool
antecedent cfg r =
  (Prelude.&&)
    ((Prelude.&&)
      ((Prelude.&&) (str_in (a1 r) (a1_bad cfg))
        (str_in (a3 r) (a3_bad cfg))) (str_in (a6 r) (a6_bad cfg)))
    (str_in (a10 r) (a10_req cfg))

rule_contr :: ContrCfg -> Row -> Prelude.Bool
rule_contr cfg r =
  (Prelude.||) (Prelude.not (antecedent cfg r))
    (((Prelude.==) :: Prelude.String -> Prelude.String -> Prelude.Bool) 
      (y r) (bad_lbl cfg))

rec_ok_range :: Policy -> Row -> Prelude.Bool
rec_ok_range =
  check_rec_range

ds_ok_range :: Policy -> Dataset -> Prelude.Bool
ds_ok_range =
  check_ds_range

default_policy :: Policy
default_policy =
  default0

rec_ok_contr :: Row -> Prelude.Bool
rec_ok_contr r =
  rule_contr default_contr_cfg r

ds_ok_contr :: Dataset -> Prelude.Bool
ds_ok_contr ds =
  forallb rec_ok_contr ds

standard_deviations_boundary :: Prelude.Integer
standard_deviations_boundary =
  Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ
    0))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

sex_a :: Prelude.Integer
sex_a =
  (\x -> x) 1

sex_b :: Prelude.Integer
sex_b =
  (\x -> 2 Prelude.* x) 1

foreign_a :: Prelude.Integer
foreign_a =
  (\x -> x) ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) 1)))

foreign_b :: Prelude.Integer
foreign_b =
  (\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x Prelude.+ 1) 1))))))

is_male_code :: Prelude.String -> Prelude.Bool
is_male_code s =
  (Prelude.||)
    ((Prelude.||)
      (((Prelude.==) :: Prelude.String -> Prelude.String -> Prelude.Bool) s
        "A91")
      (((Prelude.==) :: Prelude.String -> Prelude.String -> Prelude.Bool) s
        "A93"))
    (((Prelude.==) :: Prelude.String -> Prelude.String -> Prelude.Bool) s
      "A94")

is_female_code :: Prelude.String -> Prelude.Bool
is_female_code s =
  (Prelude.||)
    (((Prelude.==) :: Prelude.String -> Prelude.String -> Prelude.Bool) s
      "A92")
    (((Prelude.==) :: Prelude.String -> Prelude.String -> Prelude.Bool) s
      "A95")

usable_sex :: Row -> Prelude.Bool
usable_sex r =
  let {s = a9 r} in (Prelude.||) (is_male_code s) (is_female_code s)

is_male :: Row -> Prelude.Bool
is_male r =
  is_male_code (a9 r)

is_foreign_code :: Prelude.String -> Prelude.Bool
is_foreign_code s =
  ((Prelude.==) :: Prelude.String -> Prelude.String -> Prelude.Bool) s "A201"

is_not_foreign_code :: Prelude.String -> Prelude.Bool
is_not_foreign_code s =
  ((Prelude.==) :: Prelude.String -> Prelude.String -> Prelude.Bool) s "A202"

usable_foreign :: Row -> Prelude.Bool
usable_foreign r =
  let {s = a20 r} in (Prelude.||) (is_foreign_code s) (is_not_foreign_code s)

is_foreign :: Row -> Prelude.Bool
is_foreign r =
  is_foreign_code (a20 r)

count_nx :: (a1 -> Prelude.Bool) -> (a1 -> Prelude.Bool) -> (([]) a1) -> (,)
            Prelude.Integer Prelude.Integer
count_nx usable is_pos d =
  case d of {
   ([]) -> (,) 0 0;
   (:) r rs ->
    case count_nx usable is_pos rs of {
     (,) n x ->
      case usable r of {
       Prelude.True -> (,) (Prelude.succ n)
        (case is_pos r of {
          Prelude.True -> Prelude.succ x;
          Prelude.False -> x});
       Prelude.False -> (,) n x}}}

count_male :: Dataset -> (,) Prelude.Integer Prelude.Integer
count_male d =
  count_nx usable_sex is_male d

count_foreign :: Dataset -> (,) Prelude.Integer Prelude.Integer
count_foreign d =
  count_nx usable_foreign is_foreign d

within_k_sigma :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer ->
                  Prelude.Integer -> Prelude.Integer -> Prelude.Bool
within_k_sigma a b k n x =
  (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
    (\_ -> Prelude.False)
    (\_ ->
    let {bZ = (\x -> x) b} in
    let {kZ = of_nat k} in
    let {k2 = (Prelude.*) kZ kZ} in
    let {nZ = of_nat n} in
    let {xZ = of_nat x} in
    let {diff = (Prelude.-) ((Prelude.*) xZ bZ) ((Prelude.*) nZ a)} in
    let {lhs = (Prelude.*) diff diff} in
    let {
     rhs = (Prelude.*) ((Prelude.*) ((Prelude.*) k2 nZ) a) ((Prelude.-) bZ a)}
    in
    case leb 0 rhs of {
     Prelude.True -> leb lhs rhs;
     Prelude.False -> Prelude.False})
    n

sex_balance_ok_default :: Dataset -> Prelude.Bool
sex_balance_ok_default d =
  case count_male d of {
   (,) n x -> within_k_sigma sex_a sex_b standard_deviations_boundary n x}

foreign_balance_ok_default :: Dataset -> Prelude.Bool
foreign_balance_ok_default d =
  case count_foreign d of {
   (,) n x ->
    within_k_sigma foreign_a foreign_b standard_deviations_boundary n x}

all_balance_ok_default :: Dataset -> Prelude.Bool
all_balance_ok_default d =
  (Prelude.&&) (sex_balance_ok_default d) (foreign_balance_ok_default d)

data Diag =
   Build_Diag Prelude.Integer Prelude.Integer Prelude.Bool

n_total :: Diag -> Prelude.Integer
n_total d =
  case d of {
   Build_Diag n_total0 _ _ -> n_total0}

x_pos :: Diag -> Prelude.Integer
x_pos d =
  case d of {
   Build_Diag _ x_pos0 _ -> x_pos0}

passed :: Diag -> Prelude.Bool
passed d =
  case d of {
   Build_Diag _ _ passed0 -> passed0}

sex_diag_default :: Dataset -> Diag
sex_diag_default d =
  case count_male d of {
   (,) n x -> Build_Diag n x
    (within_k_sigma sex_a sex_b standard_deviations_boundary n x)}

foreign_diag_default :: Dataset -> Diag
foreign_diag_default d =
  case count_foreign d of {
   (,) n x -> Build_Diag n x
    (within_k_sigma foreign_a foreign_b standard_deviations_boundary n x)}

tiny_ds :: Dataset
tiny_ds =
  (:) (Build_Row ((\x -> x) ((\x -> 2 Prelude.* x Prelude.+ 1)
    ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x Prelude.+ 1)
    ((\x -> 2 Prelude.* x) 1))))) ((\x -> x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x Prelude.+ 1)
    ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x Prelude.+ 1)
    1)))))))))) ((\x -> x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x Prelude.+ 1) 1)))) "A12" "A31" "A61" "A93" "A101"
    "A202" "1") ([])

