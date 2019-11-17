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
  * If you win a sequence, add sqrt of the corresponding value in the outcome map + some flat value.
  * If you stale a sequence, subtract sqrt / 4 - 2 of the corresponding value in the outcome map + some flat value.
  * If you lose a sequence, subtract sqrt - 2 from the corresponding value in the outcome map for that sequence.

Note:
  * Rotation = rotate all probabilities of all Rps members into one direction.
  * Subtracting when staling is so bots don't get blocked.
  * Winning a sequence implies that the prediction held and our counter move worked.
    - e.g. we expected them to play scissors, we played rock and we won.
-}
play :: Model -> IO Rps
play (Model predictions seqSize history _ _ _) =
  if length history <= seqSize
    then randomRps
    else case predictions M.!? V.fromList (take seqSize history) of
           Nothing       -> randomRps
           Just outcomes -> return $ highest outcomes

learn :: Rps -> Rps -> Model -> Model
learn theirs ours (Model predictions seqSize historyTheirs historyOurs scoreTheirs scoreOurs) =
  Model
    predictions'
    seqSize
    (theirs : historyTheirs)
    (ours : historyOurs)
    (scoreTheirs + toInt (theirs `beat` ours))
    (scoreOurs + toInt (ours `beat` theirs))
  where
    predictions' =
      if length historyTheirs < seqSize
        then predictions
        else let prefix = V.fromList $ take seqSize historyTheirs
                 outcomes = predictions M.!? prefix
                 updatedPrediction =
                   newPrediction prefix (fromMaybe M.empty outcomes) theirs
              in updatedPrediction `M.union` predictions

mkModel :: Int -> Model
mkModel seqSize = Model M.empty seqSize [] [] 0 0

data Rps
  = Rock
  | Paper
  | Scissors
  deriving (Eq, Show, Ord, Generic)

data Model =
  Model
    { _predictions   :: Map (Vector Rps) (Map Rps Word) -- ^ A map from the sequence of actions to their outcome.
    , _seqSize       :: Int -- ^ Number of elements in the Vector (number of actions to predict).
    , _historyTheirs :: [Rps]
    , _historyOurs   :: [Rps]
    , _pointsTheirs  :: Word
    , _pointsOurs    :: Word
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
  let rockO = (getScore Rock outcomes, Rock)
      paperO = (getScore Paper outcomes, Paper)
      scissorsO = (getScore Scissors outcomes, Scissors)
      options = sortBy (flip compare) $ rockO NE.:| [paperO, scissorsO]
   in snd . NE.head $ options

beat :: Rps -> Rps -> Bool
beat Paper Rock     = True
beat Rock Scissors  = True
beat Scissors Paper = True
beat _ _            = False

newPrediction ::
     V.Vector Rps -> Map Rps Word -> Rps -> Map (Vector Rps) (Map Rps Word)
newPrediction prefix outcomes theirs = M.singleton prefix outcomes'
  where
    addWeight Rock Rock =
      let prevScore = getScore Rock outcomes
       in prevScore + 1 +
          (round (sqrt $ fromIntegral prevScore :: Double) `div` 4)
    addWeight Paper Paper =
      let prevScore = getScore Paper outcomes
       in prevScore + 1 +
          (round (sqrt $ fromIntegral prevScore :: Double) `div` 4)
    addWeight Scissors Scissors =
      let prevScore = getScore Scissors outcomes
       in prevScore + 1 +
          (round (sqrt $ fromIntegral prevScore :: Double) `div` 4)
    addWeight Rock Scissors =
      let prevScore = getScore Rock outcomes
       in prevScore + 2 + round (sqrt $ fromIntegral prevScore :: Double)
    addWeight Paper Rock =
      let prevScore = getScore Paper outcomes
       in prevScore + 2 + round (sqrt $ fromIntegral prevScore :: Double)
    addWeight Scissors Paper =
      let prevScore = getScore Scissors outcomes
       in prevScore + 2 + round (sqrt $ fromIntegral prevScore :: Double)
    addWeight Rock Paper =
      let prevScore = getScore Rock outcomes
          newScore =
            prevScore - 2 - round (sqrt $ fromIntegral prevScore :: Double)
       in if newScore > prevScore
            then 0
            else newScore
    addWeight Paper Scissors =
      let prevScore = getScore Rock outcomes
          newScore =
            prevScore - 2 - round (sqrt $ fromIntegral prevScore :: Double)
       in if newScore > prevScore
            then 0
            else newScore
    addWeight Scissors Rock =
      let prevScore = getScore Rock outcomes
          newScore =
            prevScore - 2 - round (sqrt $ fromIntegral prevScore :: Double)
       in if newScore > prevScore
            then 0
            else newScore
    outcomes' =
      M.fromList
        [ (Rock, addWeight Rock theirs)
        , (Paper, addWeight Paper theirs)
        , (Scissors, addWeight Scissors theirs)
        ]

getScore :: Rps -> Map Rps Word -> Word
getScore rps map' = fromMaybe 0 $ map' M.!? rps

toInt :: Bool -> Word
toInt False = 0
toInt True  = 1
