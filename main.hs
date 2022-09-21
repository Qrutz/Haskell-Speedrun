module Sudoku where

import Test.QuickCheck
import Data.Char
import Data.List(nub,elemIndices)
import Data.Maybe

------------------------------------------------------------------------------v

-- | Representation of sudoku puzzles (allows some junk)
type Cell = Maybe Int -- a single cell
type Row  = [Cell]    -- a row is a list of cells

data Sudoku = Sudoku [Row]
 deriving ( Show, Eq )

rows :: Sudoku -> [Row]
rows (Sudoku ms) = ms

-- | A sample sudoku puzzle
example :: Sudoku
example =
    Sudoku
      [ [j 3,j 6,n  ,n  ,j 7,j 1,j 2,n  ,n  ]
      , [n  ,j 5,n  ,n  ,n  ,n  ,j 1,j 8,n  ]
      , [n  ,n  ,j 9,j 2,n  ,j 4,j 7,n  ,n  ]
      , [n  ,n  ,n  ,n  ,j 1,j 3,n  ,j 2,j 8]
      , [j 4,n  ,n  ,j 5,n  ,j 2,n  ,n  ,j 9]
      , [j 2,j 7,n  ,j 4,j 6,n  ,n  ,n  ,n  ]
      , [n  ,n  ,j 5,j 3,n  ,j 8,j 9,n  ,n  ]
      , [n  ,j 8,j 3,n  ,n  ,n  ,n  ,j 6,n  ]
      , [n  ,n  ,j 7,j 6,j 9,n  ,n  ,j 4,j 3]
      ]
  where
    n = Nothing
    j = Just



--example2 = Sudoku $ replicate 9 $ replicate 9 (Just 3)


-- * A1

-- | allBlankSudoku is a sudoku with just blanks
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku $ replicate 9 $ replicate 9 Nothing

-- * A2

-- | isSudoku sud checks if sud is really a valid representation of a sudoku
-- puzzle
-- Checks number of rows, cell per row and the cell values
isSudoku :: Sudoku -> Bool
isSudoku sud = length (rows sud) == 9 && nub (map length $ rows sud) == [9] && not (any (`notElem` validInputs) (concatMap (take 9) $ rows sud))
  where validInputs = Nothing:[Just m | m <- [ 1..9]]

-- * A3

-- | isFilled sud checks if sud is completely filled in,
-- i.e. there are no blanks
isFilled :: Sudoku -> Bool
isFilled sud = all isJust (concat (rows sud))

------------------------------------------------------------------------------

-- * B1

-- | printSudoku sud prints a nice representation of the sudoku sud on
-- the screen
printSudoku :: Sudoku -> IO ()
printSudoku s = mapM_ (putStrLn . map converter) (rows s )

converter :: Maybe Int -> Char
converter Nothing = '.'
converter (Just n) = intToDigit n

-- * B2

-- | readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku file = do
                     str <- readFile file
                     let sud = Sudoku $ map (map charToCell) $ lines str
                     if isSudoku sud
                       then return sud
                       else error "Not a Sudoku!"

charToCell :: Char -> Cell
charToCell '.' = Nothing
charToCell c = Just (digitToInt c)

------------------------------------------------------------------------------

-- * C1

-- | cell generates an arbitrary cell in a Sudoku
cell :: Gen Cell
cell = frequency [(9,return Nothing),
                  (1, do n <- choose (1,9)
                         return (Just n))]

-- * C2

-- | an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary = do
          s <-vectorOf 9 $ vectorOf 9 cell
          return (Sudoku s)

 -- hint: get to know the QuickCheck function vectorOf

-- * C3

prop_Sudoku :: Sudoku -> Bool
prop_Sudoku = isSudoku
  -- hint: this definition is simple!

------------------------------------------------------------------------------

type Block = [Cell] -- a Row is also a Cell

-- * D1

-- Checks Block for repetitive numbers
isOkayBlock :: Block -> Bool
isOkayBlock b = filter (/= Nothing) b == nub ( filter (/= Nothing) b)

-- * D2

blocks :: Sudoku -> [Block]
blocks sud = rows sud ++ rowsToColumns  ++ rowsToSquares
  where rowsToColumns = [[  rows sud !! n !! m | n <- [0..8]] | m <-[0..8]]
        rowsToSquares = [concatMap (take 3 . drop i) $ take 3 $ drop j $  rows sud | j <- [0,3,6], i<- [0,3,6]]

-- Tests the function 'blocks'
prop_blocks_lengths :: Sudoku -> Bool
prop_blocks_lengths sud = length (blocks sud) == 3*9 && nub (map length $ blocks sud) == [9]

-- * D3

-- Checks all blocks in a Sudoku for repetitive numbers in rows, columns and squares
isOkay :: Sudoku -> Bool
isOkay sud = all isOkayBlock (blocks sud)

---- Part A ends here --------------------------------------------------------
------------------------------------------------------------------------------
---- Part B starts here ------------------------------------------------------


-- | Positions are pairs (row,column),
-- (0,0) is top left corner, (8,8) is bottom left corner
type Pos = (Int,Int)

-- * E1

blanks :: Sudoku -> [Pos]
blanks sud =  concat (zipWith (\a b -> [(a, c) |c <- b]) [0..8] rws)
  where
      rws = map (elemIndices Nothing) (rows sud)

prop_blanks_allBlanks :: Bool
prop_blanks_allBlanks = blanks allBlankSudoku == [(n,m)| n <-[0..8], m <- [0..8]]

-- * E2
(!!=) :: [a] -> (Int,a) -> [a]
xs !!= (i,y) | i > length xs -1 || i < 0 = xs
             | otherwise = take i xs ++ y : drop (i+1) xs

-- First checks size then checks the input value
prop_bangBangEquals_correct :: [Int] -> (Int,Int) -> Bool
prop_bangBangEquals_correct xs (i,y)  = lengthCheck && inputCheck
  where lengthCheck = length (xs !!= (i,y)) == length xs
        inputCheck = null xs || xs !!= (i',y) !! i' == y
        i' = abs $ i `mod` (length xs)


-- * E3
update :: Sudoku -> Pos -> Cell -> Sudoku
update sud (c, r) newVal = Sudoku newRowVal
  where newRowVal = rows sud !!= (c, (rows sud !! c) !!= (r, newVal))


prop_update_updated :: Sudoku -> Pos -> Cell -> Bool 
prop_update_updated sud (c,r) newCell = updatedCell  == newCell 
      where updatedCell = rows (update sud (c', r') newCell) !! c' !! r'
            (c',r') = (abs $ c `mod` 9, abs $ r `mod` 9)  

------------------------------------------------------------------------------



-- * F1

solve :: Sudoku -> Maybe Sudoku
solve sud | not (isSudoku sud && isOkay sud) = Nothing
          | otherwise = solveHelper sud (blanks sud)

solveHelper :: Sudoku -> [Pos] -> Maybe Sudoku
solveHelper sud [] = Just sud
solveHelper sud (x:xs)
        | null (possibleVals sud x) = Nothing
        | otherwise = (listToMaybe . catMaybes) [solveHelper (update sud x (Just n)) xs | n <- possibleVals sud x]

-- Get all possible values that chosen (i, y) postion can have in current state of game
possibleVals :: Sudoku -> Pos -> [Int]
possibleVals sud (i,y) = let updatedSudoku x = update sud (i, y) (Just x)
                          in  [x | x <- [1..9], isOkay (updatedSudoku x)]

-- * F2
readAndSolve :: FilePath -> IO ()
readAndSolve file = do
    sud <- readSudoku file
    maybe (putStrLn "no solution") printSudoku (solve sud)

-- * F3
isSolutionOf :: Sudoku -> Sudoku -> Bool
isSolutionOf sud1 sud2 | not (isOkay sud1 && isSudoku sud1 && isFilled sud1) = False
                       | otherwise = and $ zipWith isEqual ((concat . rows)  sud1) ((concat . rows) sud2)
                       where
                         isEqual _ Nothing = True
                         isEqual r r2 = fromJust r == fromJust r2
-- * F4
prop_SolveSound :: Sudoku -> Property
prop_SolveSound sud = let solution = fromJust $ solve sud
                          in isOkay sud ==> (solution `isSolutionOf` sud)
