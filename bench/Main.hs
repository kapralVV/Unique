{-# LANGUAGE RankNTypes #-}

import Criterion.Main
import Test.QuickCheck
import Test.QuickCheck.Instances()
import Data.ByteString.Lazy (ByteString)
import Data.Hashable

import qualified Data.List.Unique as U
import qualified Data.List.UniqueStrict as US
import qualified Data.List.UniqueUnsorted as UU


setupEnv :: IO (Int, String, ByteString, [Int], [String], [ByteString])
setupEnv = do
  int         <- generate (arbitrary  :: Gen Int )
  string      <- generate (arbitrary  :: Gen String )
  bytestring  <- generate (arbitrary  :: Gen ByteString )
  ints        <- generate (arbitrary  :: Gen [Int] )
  strings     <- generate (arbitrary  :: Gen [String] )
  bytestrings <- generate (arbitrary  :: Gen [ByteString] )
  return (int, string, bytestring, ints, strings, bytestrings )


benchGroups :: (Hashable a, Ord a) => a -> [a] -> [Benchmark]
benchGroups x xs =
  [
    bgroup "isUnique" [ bench "Unique"          $ whnf (U.isUnique x) xs
                      , bench "UniqueStrict"    $ whnf (US.isUnique x) xs
                      , bench "UniqueUnsorted"  $ whnf (UU.isUnique x) xs
                      ]
  , bgroup "sortUniq" [ bench "Unique"          $ whnf U.sortUniq  xs
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
  , bgroup "occurrences" [ bench "Unique" $ whnf U.occurrences xs
                         , bench "UniqueStrict" $ whnf US.occurrences xs
                         , bench "UniqueUnsorted"  $ whnf UU.occurrences xs
                         ]
  ]

main :: IO ()
main = defaultMain [
  env setupEnv $ \ ~(int,st,byst,ints,sts,bysts) ->
      bgroup "Main" $
      [ bgroup "Int" $ benchGroups int ints
      , bgroup "String" $ benchGroups st sts
      , bgroup "ByteString" $ benchGroups byst bysts
      ]
  ]

