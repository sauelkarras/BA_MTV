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

fold_left :: (a1 -> a2 -> a1) -> (([]) a2) -> a1 -> a1
fold_left f l a0 =
  case l of {
   ([]) -> a0;
   (:) b t -> fold_left f t (f a0 b)}

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

ltb :: Prelude.Integer -> Prelude.Integer -> Prelude.Bool
ltb x y0 =
  case compare0 x y0 of {
   Lt -> Prelude.True;
   _ -> Prelude.False}

data Row =
   Build_Row Prelude.Integer Prelude.Integer Prelude.Integer Prelude.String 
 Prelude.String

age :: Row -> Prelude.Integer
age r =
  case r of {
   Build_Row age0 _ _ _ _ -> age0}

balance :: Row -> Prelude.Integer
balance r =
  case r of {
   Build_Row _ balance0 _ _ _ -> balance0}

duration :: Row -> Prelude.Integer
duration r =
  case r of {
   Build_Row _ _ duration0 _ _ -> duration0}

y :: Row -> Prelude.String
y r =
  case r of {
   Build_Row _ _ _ y0 _ -> y0}

education :: Row -> Prelude.String
education r =
  case r of {
   Build_Row _ _ _ _ education0 -> education0}

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

exp_primary_pct :: Prelude.Integer
exp_primary_pct =
  (\x -> x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x) 1))))

exp_secondary_pct :: Prelude.Integer
exp_secondary_pct =
  (\x -> x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x Prelude.+ 1)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x Prelude.+ 1) 1)))))

exp_tertiary_pct :: Prelude.Integer
exp_tertiary_pct =
  (\x -> x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x Prelude.+ 1)
    ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x Prelude.+ 1)
    1))))

tol_pct :: Prelude.Integer
tol_pct =
  (\x -> x) ((\x -> 2 Prelude.* x) 1)

data Edu_cat =
   Primary
 | Secondary
 | Tertiary

edu_primary :: Prelude.String
edu_primary =
  "primary"

edu_secondary :: Prelude.String
edu_secondary =
  "secondary"

edu_tertiary :: Prelude.String
edu_tertiary =
  "tertiary"

edu_of_row :: Row -> Prelude.Maybe Edu_cat
edu_of_row r =
  let {e = education r} in
  case ((Prelude.==) :: Prelude.String -> Prelude.String -> Prelude.Bool) e
         edu_primary of {
   Prelude.True -> Prelude.Just Primary;
   Prelude.False ->
    case ((Prelude.==) :: Prelude.String -> Prelude.String -> Prelude.Bool) e
           edu_secondary of {
     Prelude.True -> Prelude.Just Secondary;
     Prelude.False ->
      case ((Prelude.==) :: Prelude.String -> Prelude.String -> Prelude.Bool)
             e edu_tertiary of {
       Prelude.True -> Prelude.Just Tertiary;
       Prelude.False -> Prelude.Nothing}}}

type Counts =
  (,) ((,) ((,) Prelude.Integer Prelude.Integer) Prelude.Integer)
  Prelude.Integer

add_edu :: Counts -> Row -> Counts
add_edu acc r =
  case acc of {
   (,) p cU ->
    case p of {
     (,) p0 cT ->
      case p0 of {
       (,) cP cS ->
        case edu_of_row r of {
         Prelude.Just e ->
          case e of {
           Primary -> (,) ((,) ((,) ((Prelude.+) cP ((\x -> x) 1)) cS) cT) cU;
           Secondary -> (,) ((,) ((,) cP ((Prelude.+) cS ((\x -> x) 1))) cT)
            cU;
           Tertiary -> (,) ((,) ((,) cP cS) ((Prelude.+) cT ((\x -> x) 1)))
            cU};
         Prelude.Nothing -> (,) ((,) ((,) cP cS) cT)
          ((Prelude.+) cU ((\x -> x) 1))}}}}

count_edu :: Dataset -> Counts
count_edu ds =
  fold_left add_edu ds ((,) ((,) ((,) 0 0) 0) 0)

within_band :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer ->
               Prelude.Integer -> Prelude.Bool
within_band exp tol cnt tot =
  case leb tot 0 of {
   Prelude.True -> Prelude.False;
   Prelude.False ->
    let {lower = (Prelude.*) ((Prelude.-) exp tol) tot} in
    let {upper = (Prelude.*) ((Prelude.+) exp tol) tot} in
    let {
     scaled = (Prelude.*) ((\x -> x) ((\x -> 2 Prelude.* x)
                ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x Prelude.+ 1)
                ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
                ((\x -> 2 Prelude.* x Prelude.+ 1) 1))))))) cnt}
    in
    (Prelude.&&) (leb lower scaled) (leb scaled upper)}

ok_class_balance :: Dataset -> Prelude.Bool
ok_class_balance ds =
  case count_edu ds of {
   (,) p _ ->
    case p of {
     (,) p0 cT ->
      case p0 of {
       (,) cP cS ->
        let {tot = (Prelude.+) ((Prelude.+) cP cS) cT} in
        case leb tot 0 of {
         Prelude.True -> Prelude.False;
         Prelude.False ->
          (Prelude.&&)
            ((Prelude.&&) (within_band exp_primary_pct tol_pct cP tot)
              (within_band exp_secondary_pct tol_pct cS tot))
            (within_band exp_tertiary_pct tol_pct cT tot)}}}}

ds_ok_class :: Dataset -> Prelude.Bool
ds_ok_class =
  ok_class_balance

edu_counts :: Dataset -> Counts
edu_counts =
  count_edu

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
    ((\x -> 2 Prelude.* x Prelude.+ 1) 1)))) "no" "unknown") ((:) (Build_Row
    ((\x -> x) ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x Prelude.+ 1)
    ((\x -> 2 Prelude.* x) 1)))))) 0 ((\x -> x)
    ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x Prelude.+ 1)
    ((\x -> 2 Prelude.* x) 1)))))) "yes" "secondary") ((:) (Build_Row
    ((\x -> x) ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) 1))))) ((\x -> x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x Prelude.+ 1) 1))))))))
    ((\x -> x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x) 1))))) "no"
    "primary") ([])))

tiny_run_ranges :: Prelude.Bool
tiny_run_ranges =
  ds_ok_range default_policy tiny_ds

tiny_run_contr :: Prelude.Bool
tiny_run_contr =
  ds_ok_contr tiny_ds

tiny_run_class :: Prelude.Bool
tiny_run_class =
  ds_ok_class tiny_ds

