import Data.List
import Data.Char
import System.IO

-- You can change the grid size here
size :: Int
size = 3

-- we represent the grid as list of lists of player values [[player]]
type Grid = [[Player]]

data Player = O | B | X deriving (Eq, Ord, Show)

-- example of winning grid
-- [[B,O,O]]
-- [[O,X,O]]
-- [[X,X,X]]

next :: Player -> Player
next O = X
next B = B
next X = O

empty :: Grid
empty = replicate size (replicate size B)

-- [[B,B,B]]
-- [[B,B,B]]
-- [[B,B,B]]

full :: Grid -> Bool
full = all (/=B) . concat
-- concat will flatten the list to single list, otherwise "all" will fail

-- all duplicate functions are me just practicing
full1 :: Grid -> Bool
full1 xs = all (/=B) $ concat xs

full2 :: Grid -> Bool
full2 xs = all (==B) [y | y <- concat xs]

turn :: Grid -> Player
turn g = if os <= xs then O else X
         where
           os = length (filter (==O) ps)
           xs = length (filter (==X) ps)
           ps = concat g

wins :: Player -> Grid -> Bool
wins p g = any line (rows ++ cols ++ dias)
           where
             line = all (==p)
             rows = g
             cols = transpose g -- transpose is a function from Data.List, converts grid cols into rows
             dias = [diag g, diag (map reverse g)]

diag :: Grid -> [Player]
diag g = [g !! n !! n | n <- [0..size-1]]
--eg.
-- > testList =
-- [[1,2,3]]
-- [[4,5,6]]
-- [[7,8,9]]
-- > diag testList = [testList !! n !! n | n <- [0..size-1]]
--                 = [testList !! 0 !! 0 | 0 <- [0..(2)]] = [1]    -- which is (0,0)
--                 = [testList !! 1 !! 1 | 1 <- [0..(2)]] = [1,5]  -- which is (1,1)
--                 = [testList !! 2 !! 2 | 2 <- [0..(2)]] = [1,5,9]-- which is (2,2)
-- > [1,5,9]
-- > diag (map reverse testList)
-- map reverse testList
-- [[3,2,1]]
-- [[6,5,4]]
-- [[9,8,7]]
-- diag (the new list above)
-- [3,5,7]
-- resutl of [diag testList, diag (map reverse testList)]
-- > [[1,5,9],[3,5,7]]

won :: Grid -> Bool
won g = wins O g || wins X g

putGrid :: Grid -> IO ()
putGrid = putStrLn . unlines . concat . interleave bar . map showRow
          where bar = [replicate ((size*4)-1) '-']

-- convert each row to a list of strings
showRow :: [Player] -> [String]
showRow = beside . interleave bar . map showPlayer
          where
            beside = foldr1 (zipWith (++))
            bar = replicate 3 "|"

-- convert a player value to a list of strings
showPlayer :: Player -> [String]
showPlayer O = ["   ", " O ", "  "]
showPlayer B = ["   ", "   ", "  "]
showPlayer X = ["   ", " X ", "  "]

-- interleave a value between each element in the list
interleave :: a -> [a] -> [a]
interleave x []     = []
interleave x [y]    = [y]
interleave x (y:ys) = y : x : interleave x ys

valid :: Grid -> Int -> Bool
valid g i = O <= i && < size^2 && concat g !! == B

move :: Grid -> Int -> Player [Grid]
move g i p = if valid g i then [chop size (xs ++ [p] ys)] else []
             where (xs,B:ys) = splitAt i (concat g)

chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)

getNat :: String -> IO Int
getNat prompt = do putStr prompt
                   xs <- getLine
                   if xs /= [] && all isDigit xs then -- isDigit provided in Data.Char
                      return (read xs)
                   else
                      do putStrLn "Error: Invalid number"
                         getNat prompt

tictactoe :: IO ()
tictactoe = run empty O

run :: Grid -> Player -> IO ()
run g p = do cls
             goto(1,1)
             putGrid g
             run' g p

type Pos = (Int, Int)

cls :: IO ()
cls = putStr "\ESC[2J"

goto :: Pos -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

prompt :: Player -> String
prompt p = "Player " ++ show p ++ ", enter your move: "

run’ :: Grid -> Player -> IO ()
run’ g p | wins O g = putStrLn "Player O wins!\n"
         | wins X g = putStrLn "Player X wins!\n"
         | full g = putStrLn "It’s a draw!\n"
         | otherwise =
              do i <- getNat (prompt p)
              case move g i p of
              [] -> do putStrLn "ERROR: Invalid move"
                       run’ g p
              [g’] -> run g’ (next p)

data Tree a = Node a [Tree a]
              deriving Show

gametree :: Grid -> Player -> Tree Grid
gametree g p = Node g [gametree g’ (next p) | g’ <- moves g p]

moves :: Grid -> Player -> [Grid]
moves g p
   | won g = []
   | full g = []
   | otherwise = concat [move g i p | i <- [0..((size^2)-1)]]

prune :: Int -> Tree a -> Tree a
prune 0 (Node x _) = Node x []
prune n (Node x ts) = Node x [prune (n-1) t | t <- ts]

depth :: Int
depth = 9

minimax :: Tree Grid -> Tree (Grid,Player)
minimax (Node g [])
   | wins O g = Node (g,O) []
   | wins X g = Node (g,X) []
   | otherwise = Node (g,B) []
minimax (Node g ts)
   | turn g == O = Node (g, minimum ps) ts’
   | turn g == X = Node (g, maximum ps) ts’
                   where
                      ts’ = map minimax ts
                      ps = [p | Node (_,p) _ <- ts’]

bestmove :: Grid -> Player -> Grid
bestmove g p = head [g’ | Node (g’,p’) _ <- ts, p’ == best]
               where
                  tree = prune depth (gametree g p)
                  Node (_,best) ts = minimax tree

main :: IO ()
main = do hSetBuffering stdout NoBuffering
          play empty O

play :: Grid -> Player -> IO ()
play g p = do cls
              goto (1,1)
              putGrid g
              play’ g p

play’ :: Grid -> Player -> IO ()
play’ g p
   | wins O g = putStrLn "Player O wins!\n"
   | wins X g = putStrLn "Player X wins!\n"
   | full g = putStrLn "It’s a draw!\n"
   | p == O = do i <- getNat (prompt p)
                 case move g i p of
                   [] -> do putStrLn "ERROR: Invalid move"
                            play’ g p
                   [g’] -> play g’ (next p)
   | p == X = do putStr "Player X is thinking... "
                 (play $! (bestmove g p)) (next p)
