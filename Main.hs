module Main where

import Data.Vector.Sized (Vector)
import GHC.TypeNats (Nat)

newtype PlayerIdx = PlayerIdx Int
  deriving newtype (Eq, Ord, Enum)
  deriving stock (Show)

newtype Chips = Chips Int
  deriving newtype (Eq, Ord, Enum, Num, Show)

data Player = Player
  { playerName :: String,
    playerChips :: Chips
  }
  deriving (Eq, Show)

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

data HandState (n :: Nat) = HandState
  { hsPlayersInHand :: Vector n Bool,
    hsContributions :: Vector n Chips,
    hsHoleCards :: Vector n [Card],
    hsBoard :: [Card]
  }
  deriving (Eq, Show)

data GameConfig = GameConfig
  { gcStartStack :: Chips,
    gcHandsPerLevel :: Int,
    gcLevels :: [BlindLevel]
  }
  deriving (Eq, Show)

data GameState (n :: Nat) = GameState
  { gsPlayers :: Vector n Player,
    gsDealerPos :: PlayerIdx,
    gsCurrentHand :: Int,
    gsCurrentBlindLvl :: Int,
    gsHand :: HandState n
  }
  deriving (Eq, Show)

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
