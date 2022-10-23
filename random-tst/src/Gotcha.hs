{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use first" #-}

module Gotcha where

import Data.Foldable (Foldable (fold))

newtype Compose f g a = Compose {getCompose :: f (g a)} deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) = Compose $ (f <$>) <$> fga

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure a = Compose $ pure $ pure a

  (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
  (Compose f) <*> (Compose a) = Compose $ ((<*>) <$> f) <*> a

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldMap f (Compose fga) = (foldMap . foldMap) f fga

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  traverse ::
    Applicative f1 =>
    (a -> f1 b) ->
    Compose f g a ->
    f1 (Compose f g b)
  traverse f (Compose fga) = Compose <$> (traverse . traverse) f fga

class Bifunctor p where
  {-# MINIMAL bimap | first, second #-}
  bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
  bimap f g = first f . second g

  first :: (a -> b) -> p a c -> p b c
  first f = bimap f id

  second :: (b -> c) -> p a b -> p a c
  second = bimap id

data Deux a b = Deux a b deriving (Show)

instance Bifunctor Deux where
  first :: (a -> b) -> Deux a c -> Deux b c
  first fn (Deux a1 a2) = Deux (fn a1) a2

  second :: (b -> c) -> Deux a b -> Deux a c
  second fn (Deux a1 a2) = Deux a1 (fn a2)

newtype Const a b = Const a deriving (Show)

instance Bifunctor Const where
  first :: (a -> b) -> Const a c -> Const b c
  first fn (Const a) = Const (fn a)

  second :: (b -> c) -> Const a b -> Const a c
  second fn (Const a) = Const a

data Drei a b c = Drei a b c deriving (Show)

instance Bifunctor (Drei a) where
  first :: (b -> c) -> Drei a b d -> Drei a c d
  first fn (Drei a1 a2 a3) = Drei a1 (fn a2) a3

  second :: (c -> d) -> Drei a b c -> Drei a b d
  second fn (Drei a1 a2 a3) = Drei a1 a2 (fn a3)

data SuperDrei a b c = SuperDrei a b deriving (Show)

instance Bifunctor (SuperDrei a) where
  first :: (b -> c) -> SuperDrei a b d -> SuperDrei a c d
  first fn (SuperDrei a1 a2) = SuperDrei a1 (fn a2)

  second :: (c -> d) -> SuperDrei a b c -> SuperDrei a b d
  second _ (SuperDrei a1 a2) = SuperDrei a1 a2

newtype SemiDrei a b c = SemiDrei a deriving (Show)

instance Bifunctor (SemiDrei a) where
  first :: (b -> c) -> SemiDrei a b d -> SemiDrei a c d
  first _ (SemiDrei a) = SemiDrei a

  second :: (c -> d) -> SemiDrei a b c -> SemiDrei a b d
  second _ (SemiDrei a) = SemiDrei a

data Quadriceps a b c d = Quadzzz a b c d deriving (Show)

instance Bifunctor (Quadriceps a b) where
  first :: (c -> c1) -> Quadriceps a b c d -> Quadriceps a b c1 d
  first fn (Quadzzz a1 a2 a3 a4) = Quadzzz a1 a2 (fn a3) a4

  second :: (d -> d1) -> Quadriceps a b c d -> Quadriceps a b c d1
  second fn (Quadzzz a1 a2 a3 a4) = Quadzzz a1 a2 a3 (fn a4)

data Either' a b = Left' a | Right' b deriving (Show)

instance Bifunctor Either' where
  first :: (a -> a1) -> Either' a b -> Either' a1 b
  first fn (Left' a) = Left' (fn a)
  first _ (Right' a) = Right' a

  second :: (b -> b1) -> Either' a b -> Either' a b1
  second fn (Right' a) = Right' (fn a)
  second _ (Left' a) = Left' a

newtype IdentityT f a = IdentityT {runIdentityT :: f a} deriving (Eq, Show)

instance (Functor m) => Functor (IdentityT m) where
  fmap f (IdentityT fa) = IdentityT $ fmap f fa

instance (Applicative m) => Applicative (IdentityT m) where
  pure :: a -> IdentityT m a
  pure x = IdentityT $ pure x

  (<*>) :: IdentityT m (a -> b) -> IdentityT m a -> IdentityT m b
  (IdentityT fab) <*> (IdentityT fa) = IdentityT $ fab <*> fa

instance (Monad m) => Monad (IdentityT m) where
  return :: a -> IdentityT m a
  return = pure

  (>>=) :: IdentityT m a -> (a -> IdentityT m b) -> IdentityT m b
  (IdentityT ma) >>= f = IdentityT $ ma >>= runIdentityT . f

newtype EitherT e m a = EitherT {runEither :: m (Either e a)}

instance Functor m => Functor (EitherT e m) where
  fmap fn (EitherT eitherEA) = EitherT $ (fmap . fmap) fn eitherEA

instance Applicative m => Applicative (EitherT e m) where
  pure = EitherT . pure . pure
  f <*> a = EitherT $ (<*>) <$> runEither f <*> runEither a

instance Monad m => Monad (EitherT e m) where
  return = pure
  (>>=) :: EitherT e m a -> (a -> EitherT e m b) -> EitherT e m b
  v >>= f = EitherT $ do
    eitherResult <- runEither v
    case eitherResult of
      Left a -> return $ Left a
      Right a' -> runEither (f a')

swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT ema) = EitherT $ swapEither <$> ema

swapEither :: Either a b -> Either b a
swapEither (Left a) = Right a
swapEither (Right a) = Left a

eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT amc bmc (EitherT amb) = do
  eab <- amb
  case eab of
    Left a -> amc a
    Right b -> bmc b

newtype ReaderT r m a = ReaderT {runReaderT :: r -> m a}

instance (Functor m) => Functor (ReaderT r m) where
  fmap :: (a -> b) -> ReaderT r m a -> ReaderT r m b
  fmap f (ReaderT rma) = ReaderT $ (fmap . fmap) f rma

instance (Applicative m) => Applicative (ReaderT r m) where
  pure :: a -> ReaderT r m a
  pure a = ReaderT (pure (pure a))

  (<*>) :: ReaderT r m (a -> b) -> ReaderT r m a -> ReaderT r m b
  (ReaderT fmab) <*> (ReaderT rma) = ReaderT $ (<*>) <$> fmab <*> rma

instance (Monad m) => Monad (ReaderT r m) where
  return = pure

  (>>=) :: ReaderT r m a -> (a -> ReaderT r m b) -> ReaderT r m b
  (ReaderT rma) >>= f = ReaderT $ \r -> do
    a <- rma r
    runReaderT (f a) r

newtype StateT s m a = StateT {runStateT :: s -> m (a, s)}

instance (Functor m) => Functor (StateT s m) where
  fmap :: (a -> b) -> StateT s m a -> StateT s m b
  fmap f (StateT smas) =
    StateT $
      (fmap . fmap)
        (\(a1, s1) -> (f a1, s1))
        smas

instance (Monad m) => Applicative (StateT s m) where
  pure a = StateT $ \s -> pure (a, s)

  (<*>) :: StateT s m (a -> b) -> StateT s m a -> StateT s m b
  (StateT smab) <*> (StateT sma) = StateT $ \s -> do
    res <- smab s
    res' <- sma s
    return (fst res (fst res'), snd res)

instance (Monad m) => Monad (StateT s m) where
  return = pure

  (>>=) :: StateT s m a -> (a -> StateT s m b) -> StateT s m b
  (StateT sma) >>= f = StateT $ \s -> do
    groupA <- sma s
    (runStateT $ f $ fst groupA) s
