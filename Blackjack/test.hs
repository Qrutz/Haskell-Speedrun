{-# LANGUAGE BlockArguments #-}
module BlackJack where
import Cards
import RunGame
import Test.QuickCheck

-- Cards for testing
hand  = Add (Card Ace Spades) Empty
hand2 = Add(Card (Numeric 2) Hearts)(Add (Card Jack Spades) Empty)
hand3 = Add(Card Ace Diamonds)(Add(Card (Numeric 6) Hearts)(Add (Card Jack Spades) Empty))
hand4 = Add(Card Ace Diamonds)(Add(Card (Numeric 10) Diamonds)(Add(Card (Numeric 2) Hearts)(Add (Card Jack Spades) Empty)))
hand5 = Add(Card Ace Diamonds)(Add(Card (Numeric 8) Diamonds)(Add(Card (Numeric 2) Hearts)(Add (Card Jack Spades) Empty)))
hand6 = Add(Card Ace Diamonds)(Add(Card (Numeric 10) Diamonds)(Add(Card (Numeric 3) Hearts)(Add (Card Jack Spades) Empty)))
hand7 = Add(Card Ace Hearts)(Add (Card Ace Spades) Empty)

-- A0
-- Shows how the size function works
sizeSteps :: [Integer]
sizeSteps = [ size hand2
            , size (Add (Card (Numeric 2) Hearts)
                   (Add (Card Jack Spades) Empty))
            , 1 + size (Add (Card Jack Spades) Empty)
            , 1 + 1 + size Empty
            , 1 + 1 + 0
            ,2]

-- A1
-- Helper function used in display. Function 'Show' converts types to String
displayCard :: Card -> String
displayCard (Card r s) = show r ++ " of " ++ show s 
        
-- show values hardcoded to fix "Numeric n issue" --              -- Ta bort show från datatypen rank för att de ska fungera--
instance Show Rank where
  show (Numeric n) = show n
  show Jack = "Jack"
  show Queen = "Queen"
  show King = "King"
  show Ace = "Ace"


-- Shows the cards in String
display ::Hand->String
display Empty = ""
display (Add card hand) ="(" ++ displayCard card ++ ") " ++ display hand 

       
-- A2
-- Gives the card its numeric value
-- The last case covers Jack, Queen and King
valueRank :: Rank -> Integer
valueRank (Numeric n) = n
valueRank Ace         = 11
valueRank _           = 10

--Calculates the values of the cards
initialValue :: Hand -> Integer
initialValue Empty = 0
initialValue (Add card hand) = valueRank(rank card) + value hand

-- Helper function to shift the Ace's value 
numberOfAces :: Hand -> Integer
numberOfAces Empty = 0
numberOfAces (Add card hand) | rank card == Ace = 1 + numberOfAces hand
                             | otherwise         = numberOfAces hand

-- Two cases: standard case and one where the Ace's shift
value :: Hand->Integer
value hand | initialValue hand <= 21 = initialValue hand
           | otherwise                = initialValue hand - 10*numberOfAces hand

-- A3
-- Helper function to declare a winner
gameOver ::Hand->Bool
gameOver h = value h > 21

-- A4
-- Declares a winner
winner :: Hand -> Hand -> Player 
winner guest bank
        | gameOver bank = Guest 
        | gameOver guest = Bank
        | value guest <= value bank = Bank 
        | otherwise = Guest






-- B1 -- 

-- associative function that puts one hand on top of anntoher using recursion
(<+) :: Hand -> Hand -> Hand
(<+) firstHand Empty            = firstHand 
(<+) Empty secondHand           = secondHand 
(<+) (Add card hand) secondHand = Add card (hand <+ secondHand)


prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 =
    p1<+(p2<+p3) == (p1<+p2)<+p3


sizeOfNew :: Hand -> Hand -> Bool 
sizeOfNew h1 h2 = sizeOfHand == size (h1 <+ h2)
            where sizeOfHand = size h1 + size h2
          

-- B2 -- 


-- use <+ function to stack every card in a suit toghether

suitHand :: Suit -> Hand 
suitHand suit = Add card hand
  let ranks = [Jack, Queen, King, Ace]
