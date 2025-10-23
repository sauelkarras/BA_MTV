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

forallb :: (a1 -> Prelude.Bool) -> (([]) a1) -> Prelude.Bool
forallb f l =
  case l of {
   ([]) -> Prelude.True;
   (:) a l0 -> (Prelude.&&) (f a) (forallb f l0)}

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

ltb :: Prelude.Integer -> Prelude.Integer -> Prelude.Bool
ltb x y0 =
  case compare0 x y0 of {
   Lt -> Prelude.True;
   _ -> Prelude.False}

data Row =
   Build_Row Prelude.Integer Prelude.Integer Prelude.Integer Prelude.String

age :: Row -> Prelude.Integer
age r =
  case r of {
   Build_Row age0 _ _ _ -> age0}

balance :: Row -> Prelude.Integer
balance r =
  case r of {
   Build_Row _ balance0 _ _ -> balance0}

duration :: Row -> Prelude.Integer
duration r =
  case r of {
   Build_Row _ _ duration0 _ -> duration0}

y :: Row -> Prelude.String
y r =
  case r of {
   Build_Row _ _ _ y0 -> y0}

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
   Build_Policy Range Range

ageR :: Policy -> Range
ageR p =
  case p of {
   Build_Policy ageR0 _ -> ageR0}

balR :: Policy -> Range
balR p =
  case p of {
   Build_Policy _ balR0 -> balR0}

default0 :: Policy
default0 =
  Build_Policy (Build_Range ((\x -> x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) 1))))) ((\x -> x)
    ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x) 1))))))))
    (Build_Range (Prelude.negate ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x Prelude.+ 1)
    ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x Prelude.+ 1)
    ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x) 1))))))))))))
    ((\x -> x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x Prelude.+ 1)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x Prelude.+ 1)
    ((\x -> 2 Prelude.* x Prelude.+ 1) 1)))))))))))))))))))))

in_range :: Range -> Prelude.Integer -> Prelude.Bool
in_range rg z =
  (Prelude.&&) (leb (lo rg) z) (leb z (hi rg))

check_rec_range :: Policy -> Row -> Prelude.Bool
check_rec_range p r =
  (Prelude.&&) (in_range (ageR p) (age r)) (in_range (balR p) (balance r))

check_ds_range :: Policy -> Dataset -> Prelude.Bool
check_ds_range p ds =
  forallb (check_rec_range p) ds

data ContrCfg =
   Build_ContrCfg Prelude.Integer Prelude.String

min_conv_duration :: ContrCfg -> Prelude.Integer
min_conv_duration c =
  case c of {
   Build_ContrCfg min_conv_duration0 _ -> min_conv_duration0}

yes_label :: ContrCfg -> Prelude.String
yes_label c =
  case c of {
   Build_ContrCfg _ yes_label0 -> yes_label0}

default_contr_cfg :: ContrCfg
default_contr_cfg =
  Build_ContrCfg ((\x -> x) ((\x -> 2 Prelude.* x Prelude.+ 1)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x Prelude.+ 1) 1))))) "yes"

rule_shortcall_not_yes :: ContrCfg -> Row -> Prelude.Bool
rule_shortcall_not_yes cfg r =
  (Prelude.||) (Prelude.not (ltb (duration r) (min_conv_duration cfg)))
    (Prelude.not
      (((Prelude.==) :: Prelude.String -> Prelude.String -> Prelude.Bool)
        (y r) (yes_label cfg)))

check_rec_contradiction_cfg :: ContrCfg -> Row -> Prelude.Bool
check_rec_contradiction_cfg =
  rule_shortcall_not_yes

check_ds_contradiction_cfg :: ContrCfg -> (([]) Row) -> Prelude.Bool
check_ds_contradiction_cfg cfg ds =
  forallb (check_rec_contradiction_cfg cfg) ds

check_rec_contradiction :: Row -> Prelude.Bool
check_rec_contradiction r =
  check_rec_contradiction_cfg default_contr_cfg r

check_ds_contradiction :: (([]) Row) -> Prelude.Bool
check_ds_contradiction ds =
  check_ds_contradiction_cfg default_contr_cfg ds

rec_ok_range :: Policy -> Row -> Prelude.Bool
rec_ok_range =
  check_rec_range

ds_ok_range :: Policy -> Dataset -> Prelude.Bool
ds_ok_range =
  check_ds_range

rec_ok_contr :: Row -> Prelude.Bool
rec_ok_contr =
  check_rec_contradiction

ds_ok_contr :: (([]) Row) -> Prelude.Bool
ds_ok_contr =
  check_ds_contradiction

default_policy :: Policy
default_policy =
  default0

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
    ((\x -> 2 Prelude.* x Prelude.+ 1) 1)))) "no") ((:) (Build_Row ((\x -> x)
    ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x Prelude.+ 1)
    ((\x -> 2 Prelude.* x) 1)))))) 0 ((\x -> x)
    ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x Prelude.+ 1)
    ((\x -> 2 Prelude.* x) 1)))))) "yes") ((:) (Build_Row ((\x -> x)
    ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) 1))))) ((\x -> x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x Prelude.+ 1) 1))))))))
    ((\x -> x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x) 1))))) "no")
    ([])))

tiny_run_ranges :: Prelude.Bool
tiny_run_ranges =
  ds_ok_range default_policy tiny_ds

tiny_run_contr :: Prelude.Bool
tiny_run_contr =
  ds_ok_contr tiny_ds

