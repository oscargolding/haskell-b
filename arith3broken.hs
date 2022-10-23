-- artih3broken.hs

module Arith3Broken where

main :: IO ()
main = do
  print (1 + 2)
  putStrLn "10"
  print (negate (-1))
  print ((+) 0 blah)
  where
    blah = negate 1

f :: Int -> String
f = undefined

g :: String -> Char
g = undefined

h :: Int -> Char
h num = g $ f num

data A

data B

data C

q :: A -> B
q = undefined

w :: B -> C
w = undefined

e :: A -> C
e a = w $ q a

data X

data Y

data Z

xz :: X -> Z
xz = undefined

yz :: Y -> Z
yz = undefined

xform :: (X, Y) -> (Z, Z)
xform (x, y) = (xz x, yz y)

munge :: (x -> y) -> (y -> (w, z)) -> x -> w
munge xToY yToWZ x = fst $ yToWZ $ xToY x

data Trivial = Trivial'

instance Eq Trivial where Trivial' == Trivial' = True

data DayOfWeek = Mon | Tue | Weds | Thu | Fri | Sat | Sun deriving (Show)

data Date = Date DayOfWeek Int deriving (Show)

instance Eq DayOfWeek where
  (==) Mon Mon = True
  (==) Tue Tue = True
  (==) Weds Weds = True
  (==) Thu Thu = True
  (==) Fri Fri = True
  (==) Sat Sat = True
  (==) Sun Sun = True
  (==) _ _ = False

instance Eq Date where
  (==)
    (Date weekday dayOfMonth)
    (Date weekday' dayOfMonth') =
      weekday == weekday'
        && dayOfMonth == dayOfMonth'