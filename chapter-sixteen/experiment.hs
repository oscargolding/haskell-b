{-# LANGUAGE FlexibleInstances #-}

module ReplaceExperiment where

replaceWithP :: b -> Char
replaceWithP = const 'p'

lms :: [Maybe [Char]]
lms = [Just "Ave", Nothing, Just "woohoo"]

-- making more specific
replaceWithP' :: [Maybe [Char]] -> Char
replaceWithP' = replaceWithP

liftedReplace :: Functor f => f a -> f Char
liftedReplace = fmap replaceWithP

liftedReplace' :: [Maybe [Char]] -> [Char]
liftedReplace' = liftedReplace

twiceLifted :: (Functor f1, Functor f) => f (f1 a) -> f (f1 Char)
twiceLifted = (fmap . fmap) replaceWithP

twiceLifted' :: [Maybe [Char]] -> [Maybe Char]
twiceLifted' = twiceLifted

thriceLifted ::
  (Functor f2, Functor f1, Functor f) =>
  f (f1 (f2 a)) ->
  f (f1 (f2 Char))
thriceLifted = (fmap . fmap . fmap) replaceWithP

thriceLifted' :: [Maybe [Char]] -> [Maybe [Char]]
thriceLifted' = thriceLifted

main :: IO ()
main = do
  putStr "replaceWithP' lms: "
  print (replaceWithP' lms)

  putStr "liftedReplace lms: "
  print (liftedReplace lms)

  putStr "liftedReplace' lms: "
  print (liftedReplace' lms)

  putStr "twiceLifted lms: "
  print (twiceLifted lms)

  putStr "twiceLifted' lms: "
  print (twiceLifted' lms)

  putStr "thriceLifted lms: "
  print (thriceLifted lms)

  putStr "thricelifted' lms: "
  print (thriceLifted' lms)

a = (+ 1) <$> read "[1]" :: [Int]

b = (fmap . fmap) (++ "lol") (Just ["Hi, ", "Hello"])

c = (* 2) <$> (\x -> x - 2)

d = ((return '1' ++) . show) <$> (\x -> [x, 1 .. 3])

e :: IO Integer
e =
  let ioi = readIO "1" :: IO Integer
      changed = ("123" ++) . show <$> ioi
   in (* 3) . read <$> changed

data Possibly a = LolNope | Yeppers a deriving (Eq, Show)

instance Functor Possibly where
  fmap f (Yeppers a) = Yeppers (f a)
  fmap _ LolNope = LolNope

data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
  fmap f (First x) = First x
  fmap f (Second y) = Second (f y)

data Quant a b = Finance | Desk a | Bloor b deriving (Eq, Show)

instance Functor (Quant a) where
  fmap f (Bloor b) = Bloor (f b)
  fmap _ Finance = Finance
  fmap _ (Desk a) = Desk a

-- data K a b = K a deriving (Eq, Show)

-- instance Functor (K a) where
-- fmap f (K x) = K x

newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

newtype K a b = K a

instance Functor (Flip K a) where
  fmap f (Flip (K a)) = Flip $ K (f a)

data EvilGoateeConst a b = GoatyConst b

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst x) = GoatyConst (f x)

data LiftItOut f a = LiftItOut (f a)

instance Functor f => Functor (LiftItOut f) where
  fmap f (LiftItOut fa) = LiftItOut (fmap f fa)

data Parappa f g a = DaWrappa (f a) (g a)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrappa fa ga) = DaWrappa (fmap f fa) (fmap f ga)

data IgnoreOne f g a b = IgnoreSomething (f a) (g b)

instance (Functor f, Functor g) => Functor (IgnoreOne f g a) where
  fmap f (IgnoreSomething fa ga) = IgnoreSomething fa (fmap f ga)

data Notorious g o a t = Notorious (g o) (g a) (g t)

instance (Functor f) => Functor (Notorious f a b) where
  fmap f (Notorious fa fb fc) = Notorious fa fb (fmap f fc)

data List a = Nil | Cons a (List a)

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons item rem) = Cons (f item) (fmap f rem)

data GoatLord a
  = NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)

instance Functor GoatLord where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat x) = OneGoat (f x)
  fmap f (MoreGoats x1 x2 x3) = MoreGoats (fmap f x1) (fmap f x2) (fmap f x3)

data TalkToMe a = Halt | Print String a | Read (String -> a)

instance Show (TalkToMe a) where
  show Halt = "TalkToMe Halt"
  show (Print x _) = "TalkToMe a " ++ x
  show (Read _) = "TalkToMe fn"

instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print str x) = Print str (f x)
  fmap f (Read fx) = Read (fmap f fx)