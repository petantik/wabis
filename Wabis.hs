module Wabis where
import Control.Monad (liftM2)

import Data.Maybe (fromJust)


data Direction = North | East | South | West 

type WorldMap = [String] 

thisWorld :: WorldMap
thisWorld =  ["..$....."
             ,"...*..--"
             ,"--....?."
             ,"!...#---"
             ,"..@....."]


type Pos = (Int, Int) 

type CharItemPair = (Char,WorldItems)

charItemPairs:: [CharItemPair]
charItemPairs = [('.',Clear)
             ,('-',Wall)
             ,('#',Friend)
             ,('!',Foe)
             ,('*',Neutral)
             ,('?',Mystery)
             ,('@',Player)
             ,('$',Exit)
             ]
data WorldItems = Clear
                | Wall
                | Friend
                | Foe
                | Neutral
                | Mystery
                | Player
                | Exit
                | Unknown deriving (Show, Eq)

type ItemPos = (WorldItems, Pos)

worldW = length (head thisWorld)
worldH = length thisWorld
worldCurrent = worldToItemPos thisWorld

worldToItemPos :: WorldMap -> [ItemPos]
worldToItemPos  = liftM2 zip (concat . worldToItems) (concat . worldToPos) 

worldToItems :: WorldMap -> [[WorldItems]]
worldToItems =(map . map) charToItem 

worldToPos :: WorldMap -> [[Pos]]
worldToPos  w = makeIndexedArray (x,y)
         where y = length w
               x = length (w!!1)

makeIndexedArray :: (Int,Int) -> [[Pos]]
makeIndexedArray (x,y) = zipWith indexHelper ys xs 
                where xs = replicate (fromIntegral y) [1..x]
                      ys = [1..y] 

indexHelper :: Int -> [Int] -> [Pos]
indexHelper y  = map (\x -> (x,y)) 

findItem :: WorldItems -> [ItemPos]-> [Pos]
findItem wi  = map snd . filter (\x -> fst x == wi) 

charToItem :: Char ->  WorldItems
charToItem c = fromJust $ lookup c charItemPairs 

main :: IO ()
main = do
    let playerPos = show $ findItem Player worldCurrent
    let currentWorld =  unlines thisWorld
    putStrLn "Hello, Player"
    putStrLn "Enter Game"
    putStrLn ("You are @, standing at "++playerPos++"; exit via $")
    putStrLn currentWorld

    
