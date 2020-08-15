--Yansong Li B00755354, Yichen Zhao B00775330, Le Wang B00761974
--CSCI 3136 - Haskell Project - Dr.Norbert Zeh - Due on: 19th Feb 2020
--Since we are using a bounch of resources, most functions are found 
--in http://hackage.haskell.org/ and the lab notes. We just put the other resources here:
--https://stackoverflow.com/questions/32253948/haskell-read-write-binary-files-complete-working-example
--https://hackage.haskell.org/package/split-0.2.3.3/docs/Data-List-Split.html
--https://stackoverflow.com/questions/28522395/converting-an-int-to-a-list-of-bits-represented-by-bool?rq=1
--https://lettier.github.io/posts/2016-04-29-breadth-first-search-in-haskell.html
module Main where

import qualified Data.ByteString.Lazy as BS
import Data.Bits
import Data.Word
import Data.List
import System.Environment

type MazeGraph = [[Int]]
type Maze = ((Int, Int), MazeGraph)
type Path = [(Int, Int)]

--it is a function that decode a maze to a matrix
decodeMaze :: BS.ByteString -> Maybe Maze
decodeMaze bs 
   | remain_length == length remain * 8 = Just ((h, w), toMazeGraph (readMaze remain) (h, w))
   | otherwise = Nothing
  where
    word8s = BS.unpack bs
    h = decodeBytes (take 4 word8s) 
    w = decodeBytes (take 4 (drop 4 word8s)) 
    remain = drop 8 word8s 
    remain_length = ((h*w*2 `div` 8) + if h*w*2 `rem` 8 == 0 then 0 else 1) * 8
--it is a function that find the path of a maze
--findPath using the breath first search algorithm below to find the results
findPath :: Maze -> Maybe Path
findPath maze@((_,w),_) = case bfs maze [(0, [])] of 
    Nothing -> Nothing
    Just p  -> Just $ map (\x -> index2RC x w) (reverse p)
--it is a function that does a breath first search algorithm
bfs ::  Maze -> [(Int, [Int])] -> Maybe [Int]
bfs ((h,w), m) lst = case lst of 
    [] -> Nothing
    (x,p):t -> let rd = m !! x 
                   ux = m !! (x - w)
                   lx = m !! (x - 1)
                   u = if x - w < 0 then [] else if x `elem` ux && head p /= x-w then [x-w] else []
                   l = if x - 1 < 0 then [] else if x `elem` lx && head p /= x-1 then [x-1] else []
                   extends = delete x (rd ++ u ++ l) \\ p
                in 
               if x == h * w - 1 then Just (x:p)
               else bfs ((h, w), m) (map (\y -> (y, x : p)) extends ++ t)

--it is a function that read a maze to boolean vlaues
readMaze :: [Word8] -> [[Bool]]
readMaze maze = zipList2 $ concatMap word8ToBits maze
--it is a function that conver boolean vaue to int 
toMazeGraph :: [[Bool]] -> (Int, Int) -> [[Int]]
toMazeGraph m (h, w) = map (\(x, [a, b]) -> updateCell [a,b] x (h,w)) (zip [0..h*w-1] m)

--it is a function that update the 2 bits number of a cell from 0,0 if there are walls
updateCell :: [Bool] -> Int -> (Int, Int) -> [Int]
updateCell [a,b] x (h,w)
  | not a     = if not b then x_r x ++ x_u x else x_r x
  | otherwise = if not b then x_u x else []
  where
     x_r x = if (x + 1) `rem` w == 0 then [] else [x+1]
     x_u x = if x + w >= h*w         then [] else [x+w]                              
updateCell _ _ _ = error ""

--it is a function that split a list
zipList2 :: [a] -> [[a]]
zipList2 [] = []
zipList2 [x] = [[x]]
zipList2 (x:y:t) = [x,y] : zipList2 t

--it is a function convert word8 to boolean values
word8ToBits :: Word8 -> [Bool]
word8ToBits x = map (testBit x) [0..7]

--it is a function decode word8 to int
decodeBytes :: [Word8] -> Int
decodeBytes lst = foldl (\acc x -> 256 * acc + (fromInteger.toInteger) x) 0 (reverse lst) 

--it is a function convert index to row and column
index2RC :: Int -> Int -> (Int, Int)
index2RC x w = (x `div` w, x `rem` w)

--it is a function convert the path to strings
pathToString :: Path -> String
pathToString [] = ""
pathToString ((x,y):h) = show x ++ " " ++ show y ++ "\n" ++ pathToString h

-- main function runs the code with the user input:
-- ./Solve <input file name> <output file name>
main :: IO ()
main = do
    args <- getArgs
    case args of
        [inputFile, outputFile] -> 
            do 
                input <- (BS.readFile inputFile)
                case decodeMaze input of 
                   Just maze -> case findPath maze of 
                                    Just path -> writeFile outputFile (pathToString path)
                                    _         -> return ()
                   _         -> return ()
        _  -> error "To Solve the maze: ./Solve <input file name> <output file name>\n"
