import Criterion.Main

import qualified Data.List.Unique as U
import qualified Data.List.UniqueStrict as US
import qualified Data.List.UniqueUnsorted as UU

main :: IO ()
main = defaultMain [
  bgroup "isUnique" [ bench "Unique"          $ whnf (U.isUnique  (1000 :: Int)) [1,2000]
                    , bench "UniqueStrict"    $ whnf (US.isUnique (1000 :: Int)) [1,2000]
                    , bench "UniqueUnsorted"  $ whnf (UU.isUnique (1000 :: Int)) [1,2000]
                    ]
  , bgroup "allUnique" [ bench "Unique"          $ whnf U.allUnique  ([1,2000] :: [Int])
                       , bench "UniqueStrict"    $ whnf US.allUnique ([1,2000] :: [Int])
                       , bench "UniqueUnsorted"  $ whnf UU.allUnique ([1,2000] :: [Int])
                       ]
  , bgroup "repeated" [ bench "Unique"          $ whnf U.repeated  ([1,2000] :: [Int])
                       , bench "UniqueStrict"    $ whnf US.repeated ([1,2000] :: [Int])
                       , bench "UniqueUnsorted"  $ whnf UU.repeated ([1,2000] :: [Int])
                       ]
  ]
