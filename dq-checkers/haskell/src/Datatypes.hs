module Datatypes where

import qualified Prelude

data Coq_comparison =
   Eq
 | Lt
 | Gt

coq_CompOpp :: Coq_comparison -> Coq_comparison
coq_CompOpp r =
  case r of {
   Eq -> Eq;
   Lt -> Gt;
   Gt -> Lt}

