{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Finite (Finite)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Vector.Sized (Vector)
import Data.Vector.Sized qualified as Vector
import GHC.TypeNats (Nat, type (+))

newtype PlayerIdx (n :: Nat) = PlayerIdx (Finite n)
  deriving newtype (Eq, Ord, Enum)
  deriving stock (Show)

newtype Chips = Chips Int
  deriving newtype (Eq, Ord, Enum, Num, Show)

data AI = AI deriving (Show)

data Player = Player
  { playerName :: String,
    playerChips :: Chips,
    playerAI :: Maybe AI
  }
  deriving (Show)

data BlindLevel = BlindLevel
  { blindSmall :: Chips,
    blindBig :: Chips
  }
  deriving (Eq, Show)

data Suit = Clubs | Diamonds | Hearts | Spades
  deriving (Eq, Show)

newtype Rank = Rank (Finite 13)
  deriving newtype (Eq, Ord, Enum, Bounded)

instance Show Rank where
  show (Rank rank) =
    case fromEnum rank of
      0 -> "A"
      9 -> "T"
      10 -> "J"
      11 -> "Q"
      12 -> "K"
      n -> show (n + 1)

data Card = Card
  { cardRank :: Rank,
    cardSuit :: Suit
  }
  deriving (Eq, Show)

data BoardState
  = BoardEmpty
  | BoardFlop (Vector 3 Card)
  | BoardTurn (Vector 4 Card)
  | BoardRiver (Vector 5 Card)
  deriving (Eq, Show)

data GameConfig = GameConfig
  { gcStartStack :: Chips,
    gcHandsPerLevel :: Int,
    gcLevels :: [BlindLevel]
  }
  deriving (Show)

data GameState (n :: Nat) = GameState
  { gsPlayers :: Vector n Player,
    gsDealerPos :: PlayerIdx n,
    gsCurrentHand :: Int,
    gsCurrentBlindLvl :: Int
  }
  deriving (Show)

data HandState (n :: Nat) = HandState
  { hsPlayersInHand :: Set (PlayerIdx n),
    hsHoleCards :: Vector n (Vector 2 Card),
    hsBoardState :: BoardState,
    hsContributions :: Vector n Chips,
    hsFirstAction :: Bool,
    hsNextPlayerToAct :: PlayerIdx n
  }
  deriving (Show)

data StepResult (n :: Nat)
  = RequestPlayerAction (PlayerIdx n)
  | HandDone
  deriving (Show)

anyPlayerYetToAct :: forall n m. (n ~ m + 1) => GameState n -> HandState n -> Bool
anyPlayerYetToAct GameState {..} HandState {..}
  | hsFirstAction = True
  | otherwise =
      -- Act while a live non-all-in player is behind maxContribution,
      -- or until action returns to hsNextPlayerToAct.
      undefined
  where
    maxContribution :: Chips
    maxContribution = Vector.maximum hsContributions

    allInPlayers :: Set (PlayerIdx n)
    allInPlayers = Vector.ifoldr addAllInPlayer Set.empty gsPlayers

    addAllInPlayer idx Player {playerChips} players
      | playerChips - Vector.index hsContributions idx == 0 =
          Set.insert (PlayerIdx idx) players
      | otherwise = players

step :: GameState n -> HandState n -> StepResult n
step GameState {} HandState {} = undefined

defaultGameConfig :: GameConfig
defaultGameConfig =
  GameConfig
    { gcStartStack = 1500,
      gcHandsPerLevel = 10,
      gcLevels =
        [ BlindLevel 1 2,
          BlindLevel 2 5,
          BlindLevel 5 10,
          BlindLevel 10 20
        ]
    }

main :: IO ()
main = pure ()
