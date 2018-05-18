{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.IO.Class (liftIO)
import Database.Redis

main :: IO ()
main = do
    let connectionInfo = defaultConnectInfo { connectHost = "redis" }
    connection <- checkedConnect connectionInfo
    runRedis connection $ do
        set "hello" "hello"
        set "world" "world"
        hello <- get "hello"
        world <- get "world"
        liftIO $ print (hello,world)
