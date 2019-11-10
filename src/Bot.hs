module Bot
  ( Model
  , Rps(..)
  , mkModel
  , learn
  , play
  ) where

import           ClassyPrelude
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict    as M
import qualified Data.Vector        as V
import           System.Random

{-
Plan:
  1. Random moves until seqSize is reached.
  2. Start looking in the history to attempt to make predictions.
  3. If a sequence matches, perform the move that has the highest chance of winning.
  * If you win / stale a sequence, add sqrt of the corresponding value in the outcome map + some flat value.
  * If you lose a sequence, rotate the outcomes in the outcome map for that sequence.

Note:
  - Rotation = rotate all probabilities of all Rps members into one direction.
-}

play :: Model -> IO Rps
play (Model predictions seqSize history) =
  if length history <= seqSize
    then randomRps
    else case predictions M.!? V.fromList (take seqSize history) of
           Nothing       -> randomRps
           Just outcomes -> return $ highest outcomes

learn :: Rps -> Rps -> Model -> Model
learn theirs ours (Model predictions seqSize history) =
  Model predictions' seqSize (theirs : history)
  where
    lost = theirs `beat` ours
    predictions' =
      if length history < seqSize
        then predictions
        else let prefix = V.fromList $ take seqSize history
                 outcomes = predictions M.!? prefix
                 updatedPrediction =
                   newPrediction prefix (fromMaybe M.empty outcomes) theirs lost
              in updatedPrediction `M.union` predictions

mkModel :: Int -> Model
mkModel seqSize = Model M.empty seqSize []

data Rps
  = Rock
  | Paper
  | Scissors
  deriving (Eq, Show, Ord, Generic)

data Model =
  Model
    { _predictions :: Map (Vector Rps) (Map Rps Word) -- ^ A map from the sequence of actions to their outcome.
    , _seqSize     :: Int -- ^ Number of elements in the Vector (number of actions to predict).
    , _history     :: [Rps]
    }
  deriving (Eq, Show, Generic)

randomRps :: IO Rps
randomRps = wordToRps <$> randomIO
  where
    wordToRps :: Word -> Rps
    wordToRps w =
      case w `mod` 3 of
        0 -> Rock
        1 -> Paper
        _ -> Scissors

highest :: Map Rps Word -> Rps
highest outcomes =
  let getOutcome x = (fromMaybe 0 (outcomes M.!? x), x)
      rockO = getOutcome Rock
      paperO = getOutcome Paper
      scissorsO = getOutcome Scissors
      options = NE.sortBy (flip compare) $ rockO NE.:| [paperO, scissorsO]
   in snd $ NE.head options

beat :: Rps -> Rps -> Bool
beat Paper Rock     = True
beat Rock Scissors  = True
beat Scissors Paper = True
beat _ _            = False

newPrediction ::
     V.Vector Rps
  -> Map Rps Word
  -> Rps
  -> Bool
  -> Map (Vector Rps) (Map Rps Word)
newPrediction prefix outcomes theirs lost = M.singleton prefix outcomesRotated
  where
    prevScore = fromMaybe 0 $ outcomes M.!? theirs
    weightIncrease = sqrt $ fromIntegral prevScore :: Double
    newScore = 2 + prevScore + round weightIncrease
    outcomes' = M.insert theirs newScore outcomes
    outcomesRotated =
      if lost
        then rotate outcomes'
        else outcomes'

rotate :: Map Rps Word -> Map Rps Word
rotate map' =
  M.fromList [(Rock, getO Paper), (Paper, getO Scissors), (Scissors, getO Rock)]
  where
    getO x = fromMaybe 0 $ map' M.!? x
