import Test.Tasty.Bench

import Data.IntMap.Strict (IntMap, (!?))
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Array.Unboxed as A
import Day15

setupEnv = bigger . mkGrid <$> input

bfsWrap grid = res
  where
    visited = bfs (0,0) grid
    (_, end) = A.bounds grid
    index idx = A.bounds grid `A.index` idx
    m !? idx = IntMap.lookup (index idx) m
    Just res = visited !? end

aStarWrap grid = res
  where
    (_, end) = A.bounds grid
    Just res = aStar (0,0) grid end


main :: IO ()
main = defaultMain
  [env setupEnv $ \ grid ->
    bgroup "Grid search"
    [ bench "bfs" $ nf bfsWrap grid
    , bench "A*"  $ nf aStarWrap grid
    ]
  ]
