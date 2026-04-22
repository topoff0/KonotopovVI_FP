module Core.MyStateT (MyStateT (..), MonadTrans (..), get, put) where

newtype MyStateT s m a
  = MyStateT {runMyStateT :: s -> m (a, s)}

instance (Functor m) => Functor (MyStateT s m) where
  fmap f st = MyStateT $ \s ->
    fmap (\(a, s1) -> (f a, s1)) (runMyStateT st s)

instance (Monad m) => Applicative (MyStateT s m) where
  pure x = MyStateT $ \s -> return (x, s)
  (<*>) st1 st2 =
    MyStateT $ \s ->
      runMyStateT st1 s >>= \(g, s1) ->
        runMyStateT st2 s1 >>= \(x, s2) ->
          return (g x, s2)

instance (Monad m) => Monad (MyStateT s m) where
  m >>= p =
    MyStateT $ \s ->
      runMyStateT m s >>= \(x, s1) ->
        runMyStateT (p x) s1

class MonadTrans t where
  lift :: (Monad m) => m a -> t m a

instance MonadTrans (MyStateT s) where
  lift m =
    MyStateT $ \s ->
      m >>= \x -> return (x, s)

get :: (Monad m) => MyStateT s m s
get =
  MyStateT $ \s ->
    return (s, s)

put :: (Monad m) => s -> MyStateT s m ()
put s =
  MyStateT $ \_ ->
    return ((), s)
