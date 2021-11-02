-- Create the type SUIT --
data Suit = Spades | Hearts | Diamonds | Clubs
  deriving (Show, Eq)

-- Each card has a Colour --
data Colour = Black | Red
  deriving (Show)

-- Define color of type --
colour :: Suit -> Colour
colour Spades = Black
colour Hearts = Red
colour Diamonds = Red
colour Clubs = Black

-- Define rank of card --
data Rank = Numeric Integer | Jack | Queen | King | Ace
  deriving (Show, Eq)

-- Function that takes 2 cards and compares their value returning true if value 1 is greater than value 2
rankBeats :: Rank -> Rank -> Bool
rankBeats _ Ace = False -- No Card in the entire deck beats ACE
rankBeats Ace _ = True
rankBeats _ King = False -- Every card in the deck does NOT BEAT KING. everyone but ACE
rankBeats King _ = True
rankBeats _ Queen = False
rankBeats Queen _ = True
rankBeats _ Jack = False
rankBeats Jack _ = True
rankBeats (Numeric m) (Numeric n) = m > n

-- prop_rankBeats a b = a /= b ==> rankBeats a b || rankBeats b a -- either a beats b and returns True, or B win and => false

-- define card --
data Card = Card Rank Suit
  deriving (Show)

-- functions to inspect Suit and Rank --
rank :: Card -> Rank
rank (Card r s) = r

suit :: Card -> Suit
suit (Card r s) = s

-- When does one card beat annother  -> when both cards have the same suit and the rank is higher--
cardBeats :: Card -> Card -> Bool
cardBeats c d
  | suit c == suit d = rankBeats (rank c) (rank d)
  | otherwise = False

--  Model a hand --
data Hand = Empty | Add Card Hand
  deriving (Show)

--When can a hand beat a card --
handBeats :: Hand -> Card -> Bool
handBeats Empty card = False
handBeats (Add c h) card =
  cardBeats c card || handBeats h card