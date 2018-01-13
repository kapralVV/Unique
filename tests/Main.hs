import qualified Unique.IsUnique as U
import qualified Unique.SortUniq as U
import qualified Unique.RepeatedBy as U
import qualified Unique.Complex as U
import qualified Unique.AllUnique as U

import qualified UniqueStrict.IsUnique as US
import qualified UniqueStrict.SortUniq as US
import qualified UniqueStrict.RepeatedBy as US
import qualified UniqueStrict.AllUnique as US

import Test.Hspec

main :: IO ()
main = mapM_ hspec [ U.isUniqueTests
                   , U.sortUniqTests
                   , U.repeatedByTests
                   , U.complexTests
                   , U.allUniqueTests
                   , US.isUniqueTests
                   , US.sortUniqTests
                   , US.repeatedByTests
                   , US.allUniqueTests                   
                   ]
