--
-- Haskell Projekt  -  Minesweeper
-- author: fadi dokmak | id003964
--
import System.Environment
import System.Random


-- minesweeper field containing all cells
type Field = [[Cell]]
-- a cell, each field has n*m cells
-- contains the number of mines in nearby cells and a visiblity flag to represent if the cell has been clicked on (will be shown)
type Cell = (Index, Integer, Bool)
type Index = (Integer, Integer)


--
-- -- returns specific cell of field at position n module Main where
-- -- parameters: n - row of element, m - position of element in row n, field - minesweeper field
-- -- returns cell at position n m
-- getCell :: Integer -> Integer -> Field -> Maybe Cell
-- getCell _ _ [] = Nothing
--
--
--
--
-- -- is called when a specific cell is clicked
-- -- the cell get the visiblity flag = true
-- -- if the clicked cell is a mine  => Game Over
-- clickCell ::  Integer -> Integer -> Field -> Field
-- clickCell n m (x:xs) | n > 1              = x:(clickCell (n-1) m (xs))
-- --                     | x == []            = [[(-1111, False)]] -- TODO Monad Nothing
--                      | n == 1 && m > 1    = (head x):(clickCell 0 (m-1) ((tail x):xs))
--                      | n == 1 && m == 1   = ((fst x), True)
--
--
--
-- -- generates the field of the Minesweeper Field
-- -- parameters: n - row count, m - column count
-- -- returns: two-dimensional array initialized with (0,false)
-- genField :: Integer -> Integer -> Field
-- genField n m =
--   let stepY arr n m = case n of
--                           0 -> arr
--                           otherwise ->
--                             stepY ((let stepX arr m = case m of
--                                                   0 -> arr
--                                                   otherwise -> stepX (arr ++ [(m, False)]) (m-1)
--                             in stepX [] m) : arr) (n-1) m
--   in stepY [] n m

--genCells colNum = ((0,0), 0, False) : zipWith (\) range

-- endless list of minesweeper cells (unclicked)
-- mines are spawned randomly
cells = [(0, False)] ++ cells

genMine = do
  num <- randomIO :: IO Float


-- endless list of index keys for Field
-- param: c   -  limits value of second index value
range c =
  let step c i j = if (c < j)
                    then [(i,j)] ++ (step c (i+1) 0)
                    else [(i,j)] ++ (step c i (j+1))
  in step c 0 0



  -- main :: IO ()
  -- main = putStrLn "Hello World"
