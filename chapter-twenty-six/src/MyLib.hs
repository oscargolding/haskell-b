{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module MyLib (someFunc) where

import Control.Monad (liftM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class
import Data.Monoid (mconcat)
import Web.Scotty

someFunc :: IO ()
someFunc = scotty 3000 $ do
  get "/:word" $ do
    beam <- param "word"
    lift (putStrLn "hello this is running within a transformer")
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]

newtype ExceptT' e m a = ExceptT' {runExceptT :: m (Either e a)}

newtype MaybeT' m a = MaybeT' {runMaybe :: m (Maybe a)}

newtype ReaderT' r m a = ReaderT' {runReaderT' :: r -> m a}

newtype StateT' s m a = StateT' {runStateT' :: s -> m (a, s)}

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

instance Functor m => Functor (ExceptT' e m) where
  fmap fn (ExceptT' eitherEA) = ExceptT' $ (fmap . fmap) fn eitherEA

instance Applicative m => Applicative (ExceptT' e m) where
  pure = ExceptT' . pure . pure
  f <*> a = ExceptT' $ (<*>) <$> runExceptT f <*> runExceptT a

instance Monad m => Monad (ExceptT' e m) where
  return = pure
  (>>=) :: ExceptT' e m a -> (a -> ExceptT' e m b) -> ExceptT' e m b
  v >>= f = ExceptT' $ do
    eitherResult <- runExceptT v
    case eitherResult of
      Left a -> return $ Left a
      Right a' -> runExceptT (f a')

swapExceptT' :: (Functor m) => ExceptT' e m a -> ExceptT' a m e
swapExceptT' (ExceptT' ema) = ExceptT' $ swapEither <$> ema

swapEither :: Either a b -> Either b a
swapEither (Left a) = Right a
swapEither (Right a) = Left a

test'' :: Monad m => (a -> m c) -> (b -> m c) -> ExceptT' a m b -> m c
test'' amc bmc (ExceptT' amb) = do
  eab <- amb
  case eab of
    Left a -> amc a
    Right b -> bmc b

instance (Functor m) => Functor (ReaderT' r m) where
  fmap :: (a -> b) -> ReaderT' r m a -> ReaderT' r m b
  fmap f (ReaderT' rma) = ReaderT' $ (fmap . fmap) f rma

instance (Applicative m) => Applicative (ReaderT' r m) where
  pure :: a -> ReaderT' r m a
  pure a = ReaderT' (pure (pure a))

  (<*>) :: ReaderT' r m (a -> b) -> ReaderT' r m a -> ReaderT' r m b
  (ReaderT' fmab) <*> (ReaderT' rma) = ReaderT' $ (<*>) <$> fmab <*> rma

instance (Monad m) => Monad (ReaderT' r m) where
  return = pure

  (>>=) :: ReaderT' r m a -> (a -> ReaderT' r m b) -> ReaderT' r m b
  (ReaderT' rma) >>= f = ReaderT' $ \r -> do
    a <- rma r
    runReaderT' (f a) r

instance (Functor m) => Functor (StateT' s m) where
  fmap :: (a -> b) -> StateT' s m a -> StateT' s m b
  fmap f (StateT' smas) =
    StateT' $
      (fmap . fmap)
        (\(a1, s1) -> (f a1, s1))
        smas

instance (Monad m) => Applicative (StateT' s m) where
  pure a = StateT' $ \s -> pure (a, s)

  (<*>) :: StateT' s m (a -> b) -> StateT' s m a -> StateT' s m b
  (StateT' smab) <*> (StateT' sma) = StateT' $ \s -> do
    res <- smab s
    res' <- sma s
    return (fst res (fst res'), snd res)

instance (Monad m) => Monad (StateT' s m) where
  return = pure

  (>>=) :: StateT' s m a -> (a -> StateT' s m b) -> StateT' s m b
  (StateT' sma) >>= f = StateT' $ \s -> do
    groupA <- sma s
    (runStateT' $ f $ fst groupA) s

instance MonadTrans (ReaderT' r) where
  lift = ReaderT' . const

instance MonadTrans MaybeT' where
  lift = MaybeT' . liftM Just

instance MonadTrans (ExceptT' e) where
  lift :: (Monad m) => m a -> ExceptT' e m a
  lift = ExceptT' . fmap Right

instance MonadTrans (StateT' s) where
  lift :: (Monad m) => m a -> StateT' s m a
  lift ma = StateT' $ \s -> do
    x <- ma
    return (x, s)

instance (MonadIO m) => MonadIO (ExceptT' e m) where
  liftIO :: IO a -> ExceptT' e m a
  liftIO = lift . liftIO

instance (MonadIO m) => MonadIO (ReaderT' r m) where
  liftIO :: IO a -> ReaderT' r m a
  liftIO = lift . liftIO

instance (MonadIO m) => MonadIO (StateT' s m) where
  liftIO :: IO a -> StateT' s m a
  liftIO = lift . liftIO