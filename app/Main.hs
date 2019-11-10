{-# LANGUAGE LambdaCase #-}

module Main
  ( main
  ) where

import           ClassyPrelude
import           Control.Concurrent             (forkIO, threadDelay)
import           Control.Concurrent.Classy.Chan as Chan

import           Bot
import           Defs
import           Interface

freshModel :: Model
freshModel = mkModel predictionSize
  where
    predictionSize = 2

loop ::
     PlayerIdentity -> NextRound -> Chan.Chan IO ChanContent -> Model -> IO ()
loop identity nextRound chan model = do
  threadDelay 2000000
  ours <- performPlay identity nextRound model
  Chan.readChan chan >>= \case
    First payload -> do
      print ("Received game started in the middle of the game..." :: Text)
      print payload
    Second (theirs, nextRound') ->
      let model' = learn theirs ours model
       in loop identity nextRound' chan model'
    Third () -> awaitBeginning chan freshModel

awaitBeginning :: Chan.Chan IO ChanContent -> Model -> IO ()
awaitBeginning chan model = do
  notifyResult <- notifyServer
  case notifyResult of
    Nothing -> do
      print
        ("Could not get a proper response from notifying the server." :: Text)
      threadDelay 5000000
      awaitBeginning chan model
    Just identity ->
      Chan.readChan chan >>= \case
        First payload -> loop identity payload chan model
        Second payload -> do
          print
            ("Received round finished when game wasn't even started..." :: Text)
          print payload
        Third () ->
          print
            ("Received game finished when game wasn't even started..." :: Text)

main :: IO ()
main = do
  chan <- Chan.newChan
  _ <- forkIO $ startScotty chan
  awaitBeginning chan freshModel
