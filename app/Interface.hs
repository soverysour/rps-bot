module Interface
  ( startScotty
  , performPlay
  , notifyServer
  ) where

import           ClassyPrelude
import           Control.Concurrent.Classy.Chan as Chan
import           Data.Aeson
import qualified Data.HashMap.Strict            as HM
import qualified Data.Text                      as T
import qualified Network.HTTP.Client.Conduit    as C
import           Network.HTTP.Simple
import           Network.Wai.Middleware.Cors
import           Web.Scotty

import           Bot
import           Defs

startScotty :: Chan.Chan IO ChanContent -> IO ()
startScotty chan =
  scotty 3000 $ do
    middleware simpleCors
    post "/cb" $ jsonData >>= liftIO . signalCallback chan

signalCallback :: Chan.Chan IO ChanContent -> Value -> IO ()
signalCallback chan value =
  let gameStart = parseGameStart value
      roundFinished = parseRoundFinished value
      gameFinished = parseGameFinished value
   in case catMaybes [gameStart, roundFinished, gameFinished] of
        [] -> do
          print ("Unrecognized message received from callback." :: Text)
          print value
        payload:_ -> Chan.writeChan chan payload

performPlay :: (GameId, PlayerId) -> NextRound -> Model -> IO Rps
performPlay (gameId, playerId) nextRound model = do
  ours <- play model
  let body' = getPlayBody gameId playerId nextRound ours
      request' =
        setRequestBody
          (C.RequestBodyLBS $ encode body')
          "POST http://201ca2f8.ngrok.io/play"
  void $ httpNoBody request'
  return ours

notifyServer :: IO (Maybe PlayerIdentity)
notifyServer = do
  let request' =
        setRequestBody
          (C.RequestBodyLBS $ encode helloPostArgs)
          "POST http://201ca2f8.ngrok.io/hello"
  eitherJson <- getResponseBody <$> httpJSONEither request'
  case eitherJson of
    Left err -> do
      print ("Could not understand JSON response" :: Text)
      print err
      return Nothing
    Right payload -> do
      print payload
      return $ parseHelloResponse payload

getPlayBody :: GameId -> PlayerId -> NextRound -> Rps -> Value
getPlayBody gameId playerId nextRound rps =
  object
    [ "gameId" .= gameId
    , "playerId" .= playerId
    , "round" .= nextRound
    , "move" .= object ["value" .= jsonify rps]
    ]

jsonify :: Rps -> Text
jsonify Rock     = "rock"
jsonify Paper    = "paper"
jsonify Scissors = "scissors"

parseHelloResponse :: Value -> Maybe (GameId, PlayerId)
parseHelloResponse (Object obj) = do
  gameId <- "gameId" `HM.lookup` obj >>= unText
  playerId <-
    "player" `HM.lookup` obj >>= unObject >>= ("id" `HM.lookup`) >>= unText
  return (gameId, playerId)
parseHelloResponse _ = Nothing

parseGameStart :: Value -> Maybe ChanContent
parseGameStart (Object obj) = do
  tagName <- "type" `HM.lookup` obj >>= unText
  obj' <- "body" `HM.lookup` obj >>= unObject
  if tagName == "startGame"
    then First <$> ("nextRound" `HM.lookup` obj' >>= unInt)
    else Nothing
parseGameStart _ = Nothing

parseRoundFinished :: Value -> Maybe ChanContent
parseRoundFinished (Object obj) = do
  tagName <- "type" `HM.lookup` obj >>= unText
  obj' <- "body" `HM.lookup` obj >>= unObject
  if tagName == "roundFinished"
    then do
      roundResult <-
        "roundResult" `HM.lookup` obj' >>= unObject >>= ("moves" `HM.lookup`) >>=
        unObject >>=
        ("pstrmybot" `HM.lookup`) >>=
        unObject >>=
        ("value" `HM.lookup`) >>=
        unText >>=
        unRps
      nextRound <- "nextRound" `HM.lookup` obj' >>= unInt
      return $ Second (roundResult, nextRound)
    else Nothing
parseRoundFinished _ = Nothing

parseGameFinished :: Value -> Maybe ChanContent
parseGameFinished (Object obj) = do
  tagName <- "type" `HM.lookup` obj >>= unText
  if tagName == "gameFinished"
    then Just $ Third ()
    else Nothing
parseGameFinished _ = Nothing

helloPostArgs :: Value
helloPostArgs =
  object
    [ "game" .= gameObj
    , "playerName" .= ("pstrmybot" :: Text)
    , "eventCallback" .= ("http://7deba681.ngrok.io/cb" :: Text)
    ]
  where
    gameObj =
      object
        [ "name" .= ("rps" :: Text)
        , "connectionToken" .= ("op2" :: Text)
        , "numberOfTotalPlayers" .= (2 :: Int)
        ]

unText :: Value -> Maybe Text
unText (String txt) = Just txt
unText _            = Nothing

unInt :: Value -> Maybe Int
unInt (Number n) = Just $ truncate n
unInt _          = Nothing

unObject :: Value -> Maybe Object
unObject (Object obj) = Just obj
unObject _            = Nothing

unRps :: Text -> Maybe Rps
unRps txt =
  case T.strip $ toLower txt of
    "rock"     -> Just Rock
    "paper"    -> Just Paper
    "scissors" -> Just Scissors
    _          -> Nothing
