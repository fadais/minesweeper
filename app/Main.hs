--
-- Haskell Projekt  -  Minesweeper
-- author: fadi dokmak | id003964
--
module Main where


import System.Environment
import System.Random
import Data.Ix

version = 1.0


-- minesweeper field containing all cells
type Field = [Cell]
-- a cell, each field has n*m cells
-- contains the number of mines in nearby cells and a visiblity flag to represent if the cell has been clicked on (will be shown)
type Cell = (Index, Integer, Bool, Bool)
type Index = (Integer, Integer)

-- the nearer to 10, the fewer mines
difficulty = 10

rowNum = 10
colNum = 10

mine = -1


-- debug flag: show all cells
initBool = False

-- MAIN

-- generates Value
main :: IO ()
main = do
 -- putStrLn "show f"
 c' <- (genCells rowNum colNum [])
 cl <- return $ genIntValues c'
 minesweeper (return cl)
--  printField cl

--  printField $ click (get (1,1) cl) cl
 --putStrLn $ show $ getAdjacentCells (head c) c


 -- game actions

-- Spielablauf:
--  1. zeige aktuelles Feld
--  2. nehme nächste Aktion des Spielers
--  3. werte Spielfeld entsprechend der Spieleraktion aus
--  4. checke Feld auf Gewinn / Niederlage
--  5. wiederhole mit aktuellem Feld
minesweeper :: IO Field -> IO ()
minesweeper field = do
  f <- field
  -- TODO putstrln in printField rein
  putStrLn "      0  1  2  3  4  5  6  7  8  9 \n"
  printField f
  -- game over check
  if won f
    then putStrLn "Herzlichen Glückwunsch" -- TODO show all cells
    else
      if lost f
        then putStrLn "Du hast verloren" -- TODO show all cells
        else do
          o <- getOption
          if (o == 1)
            then minesweeper (clickAction f)
            else minesweeper (markAction f)


-- perform action of clicking on a cell - get Index of cell and "click" it
clickAction :: Field -> IO Field
clickAction [] = return []
clickAction cl = do
  i <- getIndex
  return $ click (get i cl) cl


-- perform action of clicking on a cell - get Index of cell and mark it
markAction :: Field -> IO Field
markAction cl = do
  i <- getIndex
  return $ editCell (get i cl) (mark (get i cl)) cl


getOption :: IO Integer
getOption = do
  putStrLn "Bitte gib deine nächste Aktion ein: \n(1) - Zelle aufdecken\n(2) - Zelle (de)markieren"
  o <- getLine
  return $ read o

getIndex :: IO Index
getIndex = do
  putStrLn "Gebe die Nummer der Reihe an"
  r <- getLine
  putStrLn "Gebe die Nummer der Spalte an"
  c <- getLine
  return ((read r), (read c))

click :: Cell -> Field -> Field
click _ [] = []
click (i, d, v, m) cl
          | v == True || m == True       = cl
          | otherwise         = if d == 0
                                      -- setze Feldsichtbarkeit True + prüfe alle adjazenten Zellen -> falls sie auch 0 sind, dann aufdecken
                                      then let step adj cl = case adj of
                                                                    [] -> cl
                                                                    (x:xs) -> if (isVisible x) -- sichtbare Zellen ignorieren
                                                                                then step xs cl
                                                                                else
                                                                                  if (not ((getValue x) == 0)) -- Werte != 0 -> aufdecken
                                                                                    then step xs (editCell x (setVisible x) cl)
                                                                                    else step xs (click x cl)  -- Werte == 0 -> aufdecken + adjazente Zellen aufdecken
                                            in step (getAdjacentCells (i,d,v, m) cl) (editCell (i,d,v, m) (i,d,True, m) cl)
                                      else (editCell (i,d,v, m) (i,d,True,m) cl)


-- Field
printField :: Field -> IO ()
printField [] = putStrLn ""
printField f = do
    putStr $ convertCell $ head f
    printField $ tail f

get :: Index -> Field -> Cell
get (i,j) [] = ((-1,-1), 0, False, False) -- Todo Nothing
get i (x:xs)
        | (index' x) == i     = x
        | otherwise               = get i xs

-- true if game is lost (mine has been clicked on)
lost :: Field -> Bool
lost [] = False
lost (x:xs) = if (isMine x) && (isVisible x)
                then True
                else lost xs

--
won :: Field -> Bool
won [] = True
won (x:xs) = if isMarked x || (not $ isMine x)
              then won xs
              else False
-- Cell
editCell :: Cell -> Cell -> [Cell] -> [Cell]
editCell _ _ [] = []
editCell cOld cNew (x:xs)
                | cOld == x       = cNew:xs
                | otherwise       = x:(editCell cOld cNew xs)

-- Cell Value
setCellVal :: Integer -> Cell -> Cell
setCellVal x (i, d, v, m)
                                | d == mine       = (i,d,v,m)
                                | otherwise     = (i, x, v,m)

getValue :: Cell -> Integer
getValue (ix, d, v,m) = d

-- visibility flag
setVisible :: Cell -> Cell
setVisible (i, d, v, m) = (i, d, True,m)

isVisible :: Cell -> Bool
isVisible (i, d, v, m) = v

-- mark flag
isMarked :: Cell -> Bool
isMarked (i,d,v,m) = m

mark :: Cell -> Cell
mark (i,d,v,True) = (i,d,v,False)
mark (i,d,v,False) = (i,d,v,True)

-- generator
genIntValues :: [Cell] -> [Cell]
genIntValues [] = []
genIntValues cl =
    let step cr cl = case cr of
                            [] -> []
                            (x:xs) -> (setCellVal (mineCount (getAdjacentCells x cl)) x) : (step xs cl)
    in step cl cl


genCellPart :: (Integer, Integer) -> IO Cell
genCellPart (a,b) = do
    num <- getRandNum 10
    if num >= difficulty
        then return ((a,b), mine, initBool, False)
        else return ((a,b), 0, initBool, False)


genCells :: Integer -> Integer -> [Cell] -> IO [Cell]
--genCells 0 0 _=
genCells w h l = do
  c <- genCellPart (w',h)
  if w < 1
    then return l
    else
      if h < 1
        then genCells (w-1) 9 (c:l)
        else genCells w (h-1) (c:l)
  where
    w' = w - 1


-- return ajacent cell -> (i,j) -> [(i-1,j-1),(i,j-1), (i+1,j-1), (i-1,j), (i+1,j),(i-1,j+1), (i,j+1), (i+1,j+1)]
getAdjacentCells :: Cell -> [Cell] -> [Cell]
getAdjacentCells c [] = []
getAdjacentCells c (x:xs) | isAdj c x       = x:(getAdjacentCells c xs)
                          | otherwise       = getAdjacentCells c xs

isAdj :: Cell -> Cell -> Bool
isAdj ((i0,j0),d0,b0,m0) ((i1,j1), d1, b1,m1)
                                | ( elem i0 (range (i1-1,i1+1)) ) && ( elem j0 (range (j1-1,j1+1)) )    = True
                                | otherwise                                                             = False

-- get index of cell
index' :: Cell -> Index
index' ((i,j), d, v,m) = (i,j)


-- returns number of mines in cell list
mineCount :: [Cell] -> Integer
mineCount [] = 0
mineCount (x:xs) | (getValue x) == mine     = 1 + mineCount xs
                 | otherwise                = mineCount xs

-- mine checker
isMine :: Cell -> Bool
isMine (i, d, v, m) = (d == -1)

-- show Cells on CLI
convertCell:: Cell -> String
convertCell ((i,j), c, v,m) = if m == True
                                then if j == 0
                                              then if i > 9
                                                    then (show i) ++ " |  # "
                                                    else (show i) ++ "  |  # "
                                              else
                                                if j == 9
                                                  then " # \n"
                                                  else " # "
                                else
                                  case v of
                                        False     -> if j == 0
                                                      then if i > 9
                                                            then (show i) ++ " |  * "
                                                            else (show i) ++ "  |  * "
                                                      else
                                                        if j == 9
                                                          then " * \n"
                                                          else " * "
                                        otherwise -> if j == 0
                                                      then (show i) ++ " |  " ++ (show c) ++ " "
                                                      else
                                                        if j >= (colNum - 1)
                                                          then " " ++ (show c) ++ " \n"
                                                          else " " ++ (show c) ++ " "

-------- Helper

getRandNum :: Integer -> IO Integer
getRandNum max = do
    num <- randomRIO (0, max)
    return num
