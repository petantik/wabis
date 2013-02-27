module Wabis where
import Control.Monad (liftM2)


data Direction = North | East | South | West 

type WorldMap = [String] 

thisWorld :: WorldMap
thisWorld =  ["..$....."
             ,"...*..--"
             ,"--....?."
             ,"!...#---"
             ,"..@....."]


type Pos = (Int, Int) 

charItemPairs:: [(Char,WorldItems)]
charItemPairs = [('.',Clear)
             ,('-',Wall)
             ,('#',Friend)
             ,('!',Foe)
             ,('*',Neutral)
             ,('?',Mystery)
             ,('@',Wall)
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
findItem _ [] = []
findItem i (ip:ips)
            | fst ip == i = snd ip: findItem i ips
            | otherwise   = findItem i ips


charToItem :: Char -> [(Char,WorldItems)] -> WorldItems
charToItem c (cip:cips)
            | c == fst cip = snd cip
            | otherwise    = charToItem c cips 

main :: IO ()
main = do
    let playerPos = show $ findItem Player worldCurrent
    let currentWorld =  unlines thisWorld
    putStrLn "Hello, Player"
    putStrLn "Enter Game"
    putStrLn ("You are @, standing at "++playerPos++"; exit via $")
    putStrLn currentWorld

    
