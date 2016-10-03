{-----------------  Project Euler  -  Su Doku  -  Problem 96  ------------------
  
  Su Doku (Japanese meaning number place) is the name given to a popular
  puzzle concept. Its origin is unclear, but credit must be attributed to
  Leonhard Euler who invented a similar, and much more difficult, puzzle idea
  called Latin Squares. The objective of Su Doku puzzles, however, is to
  replace the blanks (or zeros) in a 9 by 9 grid in such that each row,
  column, and 3 by 3 box contains each of the digits 1 to 9. Below is an
  example of a typical starting puzzle grid and its solution grid.
    
    <snip>
    
  A well constructed Su Doku puzzle has a unique solution and can be solved by
  logic, although it may be necessary to employ "guess and test" methods in
  order to eliminate options (there is much contested opinion over this). The
  complexity of the search determines the difficulty of the puzzle; the example
  above is considered easy because it can be solved by straight forward direct
  deduction.
  
  The 6K text file, euler96_sudoku.txt contains fifty different Su Doku puzzles
  ranging in difficulty, but all with unique solutions (the first puzzle in the
  file is the example above).
  
  By solving all fifty puzzles find the sum of the 3-digit numbers found in the
  top left corner of each solution grid; for example, 483 is the 3-digit number
  found in the top left corner of the solution grid above.
  
  https://projecteuler.net/problem=96
  
-------------------------------------------------------------------------------}

import Data.Array
import Data.List (intersperse, nub, (\\), unfoldr)
import Data.Maybe (catMaybes, fromJust, listToMaybe)
import EulerUtils.IO (timeit)

{-----------------------------------  Data  -----------------------------------}

type Sudoku = Array (Int, Int) (Maybe Int)

value s = 100*a + 10*b + c
    where [a,b,c] = catMaybes [ s!(1,j) | j<-[1,2,3] ]

{--------------------------------  Algorithm  ---------------------------------}

-- the possible digits at the given index
options s (i,j) = case s!(i,j) of
        Just d   ->  [d]
        Nothing  ->  ([1..9]\\) . catMaybes . concat $ [h, v, b]
    where
    h = [ s!(x,j) | x<-[1..9] ]      -- extant numbers in the same row
    v = [ s!(i,y) | y<-[1..9] ]      --   "       "    "   "   "   column
    [i0,j0] = map (\n -> 3* div (n-1) 3) [i,j]
    b = [ s!(i0+x,j0+y) | x<-[1..3]  --   "       "    "   "   "   3x3 block
                        , y<-[1..3] ]

-- Standard backtraking algorithm
branch :: Sudoku -> [(Int, Int)] -> [Sudoku]
branch s [] = [s]
branch s (ij:ijs) = case options s ij of
        []    -> []
        [d]   -> branch (s // [(ij, Just d)]) ijs
        ds    -> concat [branch (s // [(ij,Just d)]) ijs | d <- ds]

solve s = head . branch s $ (indices s)

{------------------------------------  IO  ------------------------------------}

readSudoku  :: [String] -> Array (Int, Int) (Maybe Int)
readSudoku ls = listArray ((1,1),(9,9)) mb_digits
    where
    digits = concatMap (map (read . (:""))) ls
    mb_digits = map (\d-> if d==0 then Nothing else Just d) digits

showSudoku s = unlines [ intersperse ' ' [ show' (s!(i,j))
                                         | j <- [1..9] ]
                       | i <- [1..9] ]
    where show' Nothing = '_'
          show' (Just digit) = toEnum (48+digit)

getPuzzles = do
    raw_text <- readFile "euler96_sudoku.txt"
    let ls = lines raw_text
    let raw_puzzles = unfoldr (\xs -> if null xs then Nothing 
                                else Just (drop 1 . take 10 $ xs,  drop 10 xs)
                          ) ls
    let puzzles = map readSudoku raw_puzzles
    return puzzles
    
main = do
    ps <- getPuzzles
    let values = map (value . solve) ps
    let solution = sum values
    timeit $ print solution