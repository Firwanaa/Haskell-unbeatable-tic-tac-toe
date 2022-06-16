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
--eg. testList =
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










