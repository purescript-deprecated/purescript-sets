module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Map as M
import Data.Set (Set)
import Data.Set as S
import Data.Tuple (Tuple(..))
import Test.Assert (ASSERT, assert)

main :: Eff (console :: CONSOLE, assert :: ASSERT) Unit
main = do
  log "fromFoldable - empty"
  assert $ S.fromFoldable [] == S.empty :: Set Unit

  log "fromFoldable - non empty"
  do let set = S.fromFoldable [0, 1, 1, 2]
     assert $ S.size set == 3
     assert $ S.member 0 set
     assert $ S.member 1 set
     assert $ S.member 2 set

  log "intersection"
  do let s1 = S.fromFoldable [1,2,3,4,5]
         s2 = S.fromFoldable [2,4,6,8,10]
         s3 = S.fromFoldable [2,4]
     assert $ S.intersection s1 s2 == s3

  log "keysSet"
  do let s1 = M.fromFoldable [Tuple 1 1, Tuple 2 2, Tuple 3 3]
         s2 = S.fromFoldable [1, 2, 3]
     assert $ S.keysSet s1 == s2
