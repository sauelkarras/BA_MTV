module BinPos where

import qualified Prelude
import qualified Datatypes

_Pos__compare_cont :: Datatypes.Coq_comparison -> Prelude.Integer ->
                      Prelude.Integer -> Datatypes.Coq_comparison
_Pos__compare_cont r x y =
  (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
    (\p ->
    (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
      (\q -> _Pos__compare_cont r p q)
      (\q -> _Pos__compare_cont Datatypes.Gt p q)
      (\_ -> Datatypes.Gt)
      y)
    (\p ->
    (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
      (\q -> _Pos__compare_cont Datatypes.Lt p q)
      (\q -> _Pos__compare_cont r p q)
      (\_ -> Datatypes.Gt)
      y)
    (\_ ->
    (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
      (\_ -> Datatypes.Lt)
      (\_ -> Datatypes.Lt)
      (\_ -> r)
      y)
    x

_Pos__compare :: Prelude.Integer -> Prelude.Integer ->
                 Datatypes.Coq_comparison
_Pos__compare =
  _Pos__compare_cont Datatypes.Eq

