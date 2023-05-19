import Data.List
import Data.Char

revRange :: (Char,Char) -> [Char]
revRange = unfoldr g
  where g (first, last) =  if last < first then Nothing else Just (last, (first, pred last))