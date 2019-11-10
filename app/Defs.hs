module Defs
  ( NextRound
  , GameId
  , PlayerId
  , PlayerIdentity
  , ChanContent
  , OneOf(..)
  ) where

import           ClassyPrelude

import           Bot           (Rps)

type NextRound = Int

type GameId = Text

type PlayerId = Text

type PlayerIdentity = (GameId, PlayerId)

data OneOf a b c
  = First a
  | Second b
  | Third c
  deriving (Eq, Show)

type ChanContent = OneOf NextRound (Rps, NextRound) ()
