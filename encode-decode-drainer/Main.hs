{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Options.Applicative (execParser, info)
import ProgramOptions
import qualified Streamly.Prelude as S
import Streamly.Internal.Data.Stream.IsStream.Transform (tapRate)
import Data.Aeson
import qualified Data.ByteString as BS
import System.IO ( hClose, openFile, IOMode(ReadWriteMode) )
import Control.Exception (evaluate)
import qualified  Streamly.FileSystem.Handle as Handle
import Streamly.Internal.Data.Stream.Chunked

main :: IO ()
main = do
  CommandLineArgs { fName } <- execParser (info cmdArgsParser mempty)
  let
    mainLoop !hndl =
        S.mapM_ (\logLine -> do
          let
            encodedLog = (BS.toStrict $ encode logLine)
          evaluate (BS.length encodedLog)
          ) $
         tapRate
          (fromIntegral $ updateInterval prometheusSettings)
          (updateMetrics logsCounter (updateInterval prometheusSettings))
          (messagesFromFile hndl)
         & S.mapMaybeM (\line -> do
          let val = (decodeStrict line :: Maybe Value)
          pure val)
  bracket
    (openFile fName ReadWriteMode)
    (hClose)
    (\handle ->
       Streamly.unfold Handle.chunkReader handle
       & splitOnSuffix (toEnum (fromEnum '\n'))
       & fmap fromArray
       & mapMaybe (fmap (BS.length . encode) . decodeStrict bs)
       & S.mapM evaluate)
    mainLoop
  where
  messagesFromFile hndl =
    S.repeatM $ BS.hGetLine hndl
    -- BS.getLine
    -- BS.hGetLine hndl
