{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Data.Tuple.GetSet where

import Control.Monad.RWS
import Data.Tuple.Select
import Data.Tuple.Update

class Get1 t s | t -> s where get1 :: (Monoid w, Monad m) => RWST r w t m s

instance Get1 (s1, s2) s1 where get1 = RWST $ \_ s -> return (sel1 s, s, mempty)

instance Get1 (s1, s2, s3) s1 where get1 = RWST $ \_ s -> return (sel1 s, s, mempty)

class Get2 t s | t -> s where get2 :: (Monoid w, Monad m) => RWST r w t m s

instance Get2 (s1, s2) s2 where get2 = RWST $ \_ s -> return (sel2 s, s, mempty)

instance Get2 (s1, s2, s3) s2 where get2 = RWST $ \_ s -> return (sel2 s, s, mempty)

class Put1 t s | t -> s where put1 :: (Monoid w, Monad m) => s -> RWST r w t m ()

instance Put1 (s1, s2) s1 where put1 s1 = RWST $ \_ s -> return ((), upd1 s1 s, mempty)

instance Put1 (s1, s2, s3) s1 where put1 s1 = RWST $ \_ s -> return ((), upd1 s1 s, mempty)

class Put2 t s | t -> s where put2 :: (Monoid w, Monad m) => s -> RWST r w t m ()

instance Put2 (s1, s2) s2 where put2 s2 = RWST $ \_ s -> return ((), upd2 s2 s, mempty)

instance Put2 (s1, s2, s3) s2 where put2 s2 = RWST $ \_ s -> return ((), upd2 s2 s, mempty)

class Get3 t s | t -> s where get3 :: (Monoid w, Monad m) => RWST r w t m s

instance Get3 (s1, s2, s3) s3 where get3 = RWST $ \_ s -> return (sel3 s, s, mempty)

class Put3 t s | t -> s where put3 :: (Monoid w, Monad m) => s -> RWST r w t m ()

instance Put3 (s1, s2, s3) s3 where put3 s3 = RWST $ \_ s -> return ((), upd3 s3 s, mempty)
