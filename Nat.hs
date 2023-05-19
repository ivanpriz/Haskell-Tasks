import Control.Monad (guard)
import qualified Text.Read as Read
import           Text.Read    (readPrec, step)

data Nat = Zero | Suc Nat






fromNat :: Nat -> Integer
fromNat Zero = 0
fromNat (Suc n) = fromNat n + 1

instance Show Nat where
  show = show . fromNat

instance Read Nat where
  readPrec = do
    x <- step readPrec
    guard (x >= 0)
    return (toNat x)
    where
      toNat :: Integer -> Nat
      toNat 0 = Zero
      toNat x = Suc (toNat (x - 1))



add :: Nat -> Nat -> Nat
add x Zero = x
add x (Suc y) = Suc (add x y)

mul :: Nat -> Nat -> Nat
mul x Zero = Zero
mul x (Suc Zero) = x
mul x (Suc y) = add (mul x y) x

fac :: Nat -> Nat
fac Zero = Suc Zero
fac (Suc Zero) = Suc Zero
fac (Suc x) = mul (Suc x) (fac x)