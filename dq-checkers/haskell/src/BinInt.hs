module BinInt where

import qualified Prelude
import qualified BinPos
import qualified Datatypes

_Z__compare :: Prelude.Integer -> Prelude.Integer -> Datatypes.Coq_comparison
_Z__compare x y =
  (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
    (\_ ->
    (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
      (\_ -> Datatypes.Eq)
      (\_ -> Datatypes.Lt)
      (\_ -> Datatypes.Gt)
      y)
    (\x' ->
    (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
      (\_ -> Datatypes.Gt)
      (\y' -> BinPos._Pos__compare x' y')
      (\_ -> Datatypes.Gt)
      y)
    (\x' ->
    (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
      (\_ -> Datatypes.Lt)
      (\_ -> Datatypes.Lt)
      (\y' -> Datatypes.coq_CompOpp (BinPos._Pos__compare x' y'))
      y)
    x

_Z__leb :: Prelude.Integer -> Prelude.Integer -> Prelude.Bool
_Z__leb x y =
  case _Z__compare x y of {
   Datatypes.Gt -> Prelude.False;
   _ -> Prelude.True}

