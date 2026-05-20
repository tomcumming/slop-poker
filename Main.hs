module Main where

import Data.Finite (Finite)
import Data.Set (Set)
import Data.Vector.Sized (Vector)
import GHC.TypeNats (Nat)

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

newtype Rank = Rank Int
  deriving newtype (Eq, Ord)

instance Show Rank where
  show (Rank 1) = "A"
  show (Rank 10) = "T"
  show (Rank 11) = "J"
  show (Rank 12) = "Q"
  show (Rank 13) = "K"
  show (Rank n) = show n

instance Bounded Rank where
  minBound = Rank 1
  maxBound = Rank 13

data Card = Card
  { cardRank :: Rank,
    cardSuit :: Suit
  }
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
    hsContributions :: Vector n Chips,
    hsNextPlayerToAct :: Maybe (PlayerIdx n)
  }
  deriving (Show)

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
