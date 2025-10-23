module Checker where

import qualified Prelude
import qualified BinInt
import qualified List

data DQ__Record =
   DQ__Build_Record Prelude.Integer Prelude.Integer

_DQ__age :: DQ__Record -> Prelude.Integer
_DQ__age r =
  case r of {
   DQ__Build_Record age0 _ -> age0}

_DQ__income :: DQ__Record -> Prelude.Integer
_DQ__income r =
  case r of {
   DQ__Build_Record _ income0 -> income0}

type DQ__Dataset = ([]) DQ__Record

check_rec_range :: DQ__Record -> Prelude.Bool
check_rec_range r =
  (Prelude.&&)
    ((Prelude.&&) (BinInt._Z__leb 0 (_DQ__age r))
      (BinInt._Z__leb (_DQ__age r) ((\x -> x) ((\x -> 2 Prelude.* x)
        ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
        ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x Prelude.+ 1)
        ((\x -> 2 Prelude.* x Prelude.+ 1) 1)))))))))
    (BinInt._Z__leb 0 (_DQ__income r))

forallb :: (a1 -> Prelude.Bool) -> (([]) a1) -> Prelude.Bool
forallb p xs =
  List.fold_right (\x acc ->
    case p x of {
     Prelude.True -> acc;
     Prelude.False -> Prelude.False}) Prelude.True xs

check_ds_range :: (([]) DQ__Record) -> Prelude.Bool
check_ds_range ds =
  forallb check_rec_range ds

