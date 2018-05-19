{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Codec.Binary.UTF8.String as UTF8 (encode, decode)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT)
import qualified Control.Monad.Trans.State.Strict as State (get, put, evalStateT)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS (unpack)
import Data.Either (either)
import Data.List (uncons)
import qualified Database.Redis as Redis

type KvsConvert a = StateT [Maybe ByteString] Maybe a

data KvsGets a = KvsGets [ByteString] (KvsConvert a)

instance Functor KvsGets where
    fmap f (KvsGets keys convert) = KvsGets keys (fmap f convert)

instance Applicative KvsGets where
    pure a = KvsGets [] (return a)
    KvsGets keys0 f <*> KvsGets keys1 a = KvsGets (keys0 ++ keys1) (f <*> a)

kvsGet :: ByteString -> (Maybe ByteString -> Maybe a) -> KvsGets a
kvsGet key convert = KvsGets [key] m
    where
    m = do
        vals <- State.get
        (h, tail) <- lift . uncons $ vals
        State.put tail
        lift . convert $ h

hmgetAll :: KvsGets a -> Redis.Redis (Maybe a)
hmgetAll (KvsGets keys m) = do
    vals <- Redis.mget keys
    either (const $ return Nothing) (return . State.evalStateT m) vals

data SomeData = SomeData Int String String [Int] deriving (Show)

main :: IO ()
main = do
    let entries = [("a", "999"), ("b", "hello"), ("c", "world"), ("d", "[1,2,3]")]
        getList = sequenceA $ map (flip kvsGet id) ["a", "b", "c", "d"] :: KvsGets [ByteString]
        getSomeData = SomeData
            <$> kvsGet "a" (fmap $ read . UTF8.decode . BS.unpack)
            <*> kvsGet "b" (fmap $ UTF8.decode . BS.unpack)
            <*> kvsGet "c" (fmap $ UTF8.decode . BS.unpack)
            <*> kvsGet "d" (fmap $ read . UTF8.decode . BS.unpack)
        connectionInfo = Redis.defaultConnectInfo { Redis.connectHost = "redis" }
    connection <- Redis.checkedConnect connectionInfo
    Redis.runRedis connection $ do
        Redis.mset entries
        a <- hmgetAll getList
        liftIO . print $ a
        b <- hmgetAll getSomeData
        liftIO . print $ b
