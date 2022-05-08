{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

-- | Data types for card games
module Cards where

-- | A card has a rank and belongs to a suit.
data Card = Card Rank Suit
  deriving (Eq, Show)

-- | rank and suit give the respective parts of a card
rank :: Card -> Rank
rank (Card r _) = r

suit :: Card -> Suit
suit (Card _ s) = s¨åä

-- | A rank is either a numeric card, a face card, or an ace. The
-- numeric cards range from two to ten.
data Rank = Numeric Integer | Jack | Queen | King | Ace
  deriving (Eq, Show)

-- | All the different suits.
data Suit = Hearts | Spades | Diamonds | Clubs
  deriving (Eq, Show)

-- | A hand of cards. This data type can also be used to represent a
-- deck of cards.
data Hand = Empty | Add Card Hand
  deriving (Eq)

ex1 :: Card
ex1 = Card King Spades

ex2 :: Card
ex2 = Card King Hearts

ex1h :: Hand
ex1h = Add ex1 (Add ex2 Empty)

-- | The size of a hand.
size :: Num a => Hand -> a
size Empty = 0
size (Add card hand) = 1 + size hand

display :: Hand -> String
display Empty = ""
display (Add c h) = "(" ++ displayCard c ++ ") " ++ display h

displayCard :: Card -> String
displayCard (Card r s) = show r ++ " of " ++ show s

value :: Hand -> Integer
value hand
  | initialValue hand <= 21 = initialValue hand
  | otherwise = initialValue hand - 10 * numberOfAces hand

numberOfAces :: Hand -> Integer
numberOfAces Empty = 0
numberOfAces (Add card hand)
  | rank card == Ace = 1 + numberOfAces hand
  | otherwise = numberOfAces hand

valueRank :: Rank -> Integer
valueRank (Numeric n) = n
valueRank Ace = 11
valueRank _ = 10

initialValue :: Hand -> Integer
initialValue Empty = 0
initialValue (Add c h) = valueRank (rank c) + value h

gameOver :: Hand -> Bool
gameOver hand = value hand > 21

data Player = Guest | Bank
  deriving (Show, Eq)

winner :: Hand -> Hand -> Player
winner h1 h2
  | gameOver h1 = Bank
  | gameOver h2 = Guest
  | value h1 <= value h2 = Bank
  | otherwise = Bank

ex2h :: Hand
ex2h = Add (Card (Numeric 5) Spades) (Add ex2 Empty)

ex3h :: Hand
ex3h = Add (Card (Numeric 8) Spades) (Add ex2 Empty)

(<+) :: Hand -> Hand -> Hand
Empty <+ h2 = h2
h1 <+ Empty = h1
(Add c h) <+ h2 = Add c (h <+ h2)

instance Show Hand where
  show = display

suitDeck :: Suit -> Hand
suitDeck suit =
  let ranks = [Ace, King, Queen, Jack] ++ [Numeric n | n <- [2 .. 10]]
      cards = [Card rank suit | rank <- ranks]
      hands = [Add card Empty | card <- cards]
   in foldr (<+) Empty hands

-- returns full deck of cards by adding all cards of each suit on top of each other
fullDeck :: Hand
fullDeck =
  suitDeck Spades
    <+ suitDeck Diamonds
    <+ suitDeck Hearts
    <+ suitDeck Clubs

draw :: Hand -> Hand -> (Hand, Hand)
draw Empty hand = error "the deck is empty"
draw (Add card deck) hand = (deck, Add card hand)

playBank :: Hand -> Hand
playBank deck = playbank deck Empty

playbank :: Hand -> Hand -> Hand
playbank deck hand
  | value biggerHand >= 16 = biggerHand
  | otherwise = playbank smallerDeck biggerHand
  where
    (smallerDeck, biggerHand) = draw deck hand
