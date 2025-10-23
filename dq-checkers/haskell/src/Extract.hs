module Extract where

import qualified Prelude

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

