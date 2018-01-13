import IsUnique
import SortUniq
import RepeatedBy
import Complex
import AllUnique
import Test.Hspec

main :: IO ()
main = mapM_ hspec [isUniqueTests
                   , sortUniqTests
                   , repeatedByTests
                   , complexTests
                   , allUniqueTests
                   ]
