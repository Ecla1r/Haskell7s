{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Monad (forM_, when)
import Control.Monad.Reader
import Control.Monad.State
import System.Directory
import System.Environment ( getArgs )
import Text.Read ( readMaybe )
import Du ( du )
import Data.Maybe ( isJust )

walkDirectory ::
  ( MonadState [FilePath] m
  , MonadIO m)
  => m ()
walkDirectory = do
  paths <- get
  case paths of
    [] -> pure ()
    path : rest -> do
      liftIO $ putStrLn path
      put rest
      isDir <- liftIO $ doesDirectoryExist path
      when isDir $ do
        ds <- liftIO $ listDirectory path
        put $ rest ++ map ((path ++ "/") ++) ds
      walkDirectory


main :: IO ()
main = do
    args <- getArgs
    let argsLen = length args
    if argsLen == 1 then
        du (head args) Nothing
    else if argsLen == 2 then do
        let depthMb = readMaybe (args !! 1) :: Maybe Integer
        if isJust depthMb then
            du (head args) depthMb
        else
            putStrLn "Error: wrong depth"
    else
        putStrLn "Error: wrong args length"