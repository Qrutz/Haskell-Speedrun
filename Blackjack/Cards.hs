-- | Data types for card games
module Cards where

import Test.QuickCheck
import System.Random


-- | A card has a rank and belongs to a suit.
data Card = Card Rank Suit
      deriving (Eq, Show)

-- | rank and suit give the respective parts of a card
rank :: Card -> Rank
rank (Card r _) = r

suit :: Card -> Suit
suit (Card _ s) = s


-- | A rank is either a numeric card, a face card, or an ace. The
-- numeric cards range from two to ten.
data Rank = Numeric Int | Jack | Queen | King | Ace
            deriving (Eq, Show)

-- | All the different suits.
data Suit = Hearts | Spades | Diamonds | Clubs
            deriving (Eq, Show)

-- | A hand of cards. This data type can also be used to represent a
-- deck of cards.
type Hand = [Card]

-- | The size of a hand. UPPGIFT A0 
size :: Hand -> Int
size [] = 0 
size (_:xs) = 1 + size xs

sizeSteps :: [Int]
sizeSteps = [   size hand2Cards,
                size [Card (Numeric 2) Hearts, Card Jack Spades],
                1 + size [Card Jack Spades],
                1 + 1 + size [],
                1 + 1 + 0,
                2
            ]








-- Example Cards --  

aCard1 :: Card
aCard1 = Card King Spades

aCard2 :: Card
aCard2 = Card Queen Spades

aCard3 :: Card
aCard3 = Card King Spades

aCard4 :: Card
aCard4 = Card Ace Spades

aCard5 :: Card
aCard5 = Card (Numeric 9) Hearts

aCard6 :: Card
aCard6 = Card Ace Hearts

-- Example hands -- 


hand2Cards = [aCard1, aCard2]

hand3Cards = [aCard1, aCard2, aCard3]


handz :: Hand 
handz = [aCard1, aCard2, aCard3, aCard4, aCard5]

doubleAce :: Hand
doubleAce = [aCard4, aCard4]

bustHand :: Hand 
bustHand = [aCard1, aCard2, aCard5]





--------------------------------------------------------------------
-- Functions below are to tell QuickCheck how to generate random cards
-- We will see how to do this in week 4.

-- | Generate a random Card
-- instance Arbitrary Card where
--   arbitrary = Card <$> arbitrary <*> arbitrary

-- -- | Random generator for Suit
-- instance Arbitrary Suit where
--   arbitrary = elements [Hearts, Spades, Diamonds, Clubs]

-- -- | Random generator for Rank
-- instance Arbitrary Rank where
--   arbitrary = frequency [ (4, elements [Jack,Queen,King,Ace])
--                         , (9, Numeric <$> choose (2, 10))
--                         ]

-- -- | Random generator for Hand
-- instance Arbitrary Hand where
--   arbitrary = frequency [  (1,  return Empty)
--                         ,  (7, Add <$> arbitrary <*> arbitrary)
--                         ]
--   shrink Empty = []
--   shrink (Add c h) = Empty : h : [Add c h' | h'<-shrink h]


-- -- We also need to be able to generate random number generators. (This
-- -- does not really belong in this file, but is placed here to reduce
-- -- the number of files needed.)
-- instance Arbitrary StdGen where
--   arbitrary = mkStdGen <$> arbitrary

