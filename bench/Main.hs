{-# LANGUAGE RankNTypes #-}

import Criterion.Main
import Test.QuickCheck
import Test.QuickCheck.Instances()
import Data.ByteString.Lazy (ByteString)
import Data.Hashable

import qualified Data.List.Unique as U
import qualified Data.List.UniqueStrict as US
import qualified Data.List.UniqueUnsorted as UU


setupEnv :: IO ([Int], [String], [ByteString])
setupEnv = do
  int        <- fmap getLarge $ generate (arbitrary  :: Gen (Large [Int]))
  string     <- generate (arbitrary  :: Gen [String])
  bytestring <- generate (arbitrary  :: Gen [ByteString])
  return (int, string, bytestring)


benchGroups :: (Hashable a, Ord a) => [a] -> [Benchmark]
benchGroups xs =
  [
    -- bgroup "isUnique" [ bench "Unique"          $ whnf (U.isUnique  'h') xs
    --                   , bench "UniqueStrict"    $ whnf (US.isUnique 'h') xs 
    --                   , bench "UniqueUnsorted"    $ whnf (UU.isUnique 'h') xs
    --                   ]
    bgroup "sortUniq" [ bench "Unique"          $ whnf U.sortUniq  xs
                      , bench "UniqueStrict"    $ whnf US.sortUniq xs
                      , bench "UniqueUnsorted"  $ whnf UU.removeDuplicates xs
                      ]
  , bgroup "allUnique" [ bench "Unique"          $ whnf U.allUnique  xs
                       , bench "UniqueStrict"    $ whnf US.allUnique xs
                       , bench "UniqueUnsorted"  $ whnf UU.allUnique xs
                       ]
  , bgroup "repeated" [ bench "Unique"           $ whnf U.repeated  xs
                       , bench "UniqueStrict"    $ whnf US.repeated xs
                       , bench "UniqueUnsorted"  $ whnf UU.repeated xs
                       ]
  , bgroup "count" [ bench "Unique"          $ whnf U.count  xs
                   , bench "UniqueStrict"    $ whnf US.count xs
                   , bench "UniqueUnsorted"  $ whnf UU.count xs
                   ]
  , bgroup "count_" [ bench "Unique"          $ whnf U.count_  xs
                    , bench "UniqueStrict"    $ whnf US.count_ xs
                    , bench "UniqueUnsorted"  $ whnf UU.count_ xs
                    ]
  ]

main :: IO ()
main = defaultMain [
      env setupEnv $ \ ~(int,st,byst) ->
          bgroup "Main" $
          [ bgroup "Int" $ benchGroups int
          , bgroup "String" $ benchGroups st
          , bgroup "ByteString" $ benchGroups byst
          ]
      ]

