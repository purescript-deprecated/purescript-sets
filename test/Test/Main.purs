module Test.Main where

import Prelude

import Control.Monad.Eff.Console (log)
import Test.Assert (assert)

import Data.Set (Set())
import Data.Set as S

main = do
  log "fromFoldable - empty"
  assert $ S.fromFoldable [] == S.empty :: Set Unit

  log "fromFoldable - non empty"
  do let set = S.fromFoldable [0, 1, 1, 2]
     assert $ S.size set == 3
     assert $ S.member 0 set
     assert $ S.member 1 set
     assert $ S.member 2 set
