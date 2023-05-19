import Control.Monad.Writer
import Data.Monoid

type Shopping = Writer [(String, Integer)] ()

purchase :: String -> Integer -> Shopping
purchase name price = tell $ [(name, price)]

total :: Shopping -> Integer
total = sum . (map snd) . execWriter

items :: Shopping -> [String]
items = (map fst) . execWriter

shopping1 :: Shopping
shopping1 = do
  purchase "Jeans"   19200
  purchase "Water"     180
  purchase "Lettuce"   328