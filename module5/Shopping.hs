import Control.Monad.Writer
import Data.Monoid

type Shopping = Writer (Sum Integer) ()

purchase :: String -> Integer -> Shopping
purchase name price = tell $ Sum price

total = getSum . execWriter

shopping1 :: Shopping
shopping1 = do
  purchase "Jeans"   19200
  purchase "Water"     180
  purchase "Lettuce"   328

