module BlackJack where
import Cards
import RunGame
import Test.QuickCheck



-- A1 --
displayCard :: Card -> String 
displayCard (Card r s) = show r ++ " of " ++ show s 


-- display cards in hand as string --
display :: Hand -> String
display [] = []                                        -- Empty Hand
display [h] = displayCard h                            -- In case theres only one Card
display (h:hs) = displayCard h ++ ", " ++ display hs  -- display head of list ++  the tail of list using recursion


-- A2 --
valueRank :: Rank -> Int
valueRank (Numeric n) = n 
valueRank Ace = 11
valueRank _ = 10

valueCard :: Card -> Int
valueCard (Card r s) = valueRank r

value :: Hand -> Int
value h
    | val > 21 = val - numberOfAces h * 10
    | otherwise = val
    where val = sum [valueCard c | c <- h]

-- get number of aces in a hand
numberOfAces :: Hand -> Int
-- filter the list of Cards where x has Rank Ace and get the length of that list
numberOfAces hand = length (filter (\x -> rank x == Ace) hand)        

-- A3 -- 
-- If the value of hand is greater than 21 , print false-- 
gameOver :: Hand -> Bool
gameOver hand = value hand > 21

--A4--   
winner :: Hand -> Hand -> Player 
winner bank guest
        | gameOver bank = Bank 
        | gameOver guest = Guest 
        | value bank <= value guest = Guest 
        | otherwise = Bank


