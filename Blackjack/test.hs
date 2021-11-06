-- blackjack game --
data Suit = Hearts | Diamonds | Clubs | Spades
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Rank = Numeric Int | Jack | Queen | King | Ace
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

colour :: Suit -> String
colour Hearts   = "red"
colour Diamonds = "red"
colour Clubs    = "black"
colour Spades   = "black"

data Card = Card { rank :: Rank, suit :: Suit }
  deriving (Eq, Ord, Show, Read)

data Hand = Hand [Card]
  deriving (Eq, Ord, Show, Read)

data Player = Player { name :: String, hand :: Hand }
  deriving (Eq, Ord, Show, Read)

data Game = Game { players :: [Player], dealer :: Player }
  deriving (Eq, Ord, Show, Read)

data GameState = GameState { game :: Game, player :: Player }
  deriving (Eq, Ord, Show, Read)

data Action = Hit | Stand | DoubleDown | Split | Surrender
  deriving (Eq, Ord, Show, Read)

data Result = Win | Lose | Push
  deriving (Eq, Ord, Show, Read)

data Bet = Bet { amount :: Int, result :: Result }
  deriving (Eq, Ord, Show, Read)

data BettingState = BettingState { bets :: [Bet], player :: Player }
  deriving (Eq, Ord, Show, Read)

data GameState' = GameState' { game :: Game, player :: Player, bettingState :: BettingState }
  deriving (Eq, Ord, Show, Read)

rankbeats :: Rank -> Rank -> Bool
rankbeats (Numeric n) (Numeric m) = n > m
rankbeats Jack  = True
rankbeats Queen  = True
rankbeats King  = True
rankbeats Ace  = True
rankbeats   = False

handbeats :: Hand -> Hand -> Bool
handbeats (Hand [])  = False
handbeats  (Hand []) = True
handbeats (Hand (c1:cs)) (Hand (c2:cs')) =
  if rank c1 == rank c2
    then handbeats (Hand cs) (Hand cs')
    else rankbeats (rank c1) (rank c2)

suit :: Card -> Suit
suit (Card _ s) = s

cardbeats :: Card -> Card -> Bool
cardbeats c1 c2 = rankbeats (rank c1) (rank c2)



-- start the game
start :: Game
start = Game { players = [], dealer = Player { name = "Dealer", hand = Hand [] } }
