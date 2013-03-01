module Wabis where
import Control.Monad (liftM2)
import Data.Tuple (swap)
import Data.Maybe (fromJust)


data Direction = North | East | South | West 

type WorldCharMap = [String] 
type Pos = (Int, Int) 
type ItemPos = (WorldItems, Pos)
type CharItemPair = (Char,WorldItems)
type ItemCharPair = (WorldItems,Char)
type World = [ItemPos]

thisWorld :: WorldCharMap
thisWorld =  ["..$....."
             ,"...*..--"
             ,"--....?."
             ,"!...#---"
             ,"..@....."]

data WorldItems = Clear
                | Wall
                | Friend
                | Foe
                | Neutral
                | Mystery
                | Player
                | Exit
                | Unknown deriving (Show, Eq)


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


itemCharPairs :: [ItemCharPair]
itemCharPairs = map swap charItemPairs


worldDim :: WorldCharMap -> (Int,Int)
worldDim w = (length . head $ w,length w)

worldCMToWorld :: WorldCharMap -> World
worldCMToWorld  = liftM2 zip (concat . worldCMToItems) (concat . worldCMToPos) 

worldCMToItems :: WorldCharMap -> [[WorldItems]]
worldCMToItems =(map . map) charToItem 

worldCMToPos :: WorldCharMap -> [[Pos]]
worldCMToPos  w = makeIndexedArray . worldDim $ w
{-|
makeIndexedArray' :: (Int,Int) -> [[Pos]]
makeIndexedArray' (x,y) = zipWith indexHelper ys xs 
                where xs = replicate (fromIntegral y) [1..x]
                      ys = [1..y] 
indexHelper :: Int -> [Int] -> [Pos]
indexHelper y  = map (\x -> (x,y)) 

-}
makeIndexedArray :: (Int,Int) -> [[Pos]]
makeIndexedArray (x,y) = zipUp ysxs 
                    where xs = [1..x]
                          ys = [1..y]
                          xxs = replicate y xs
                          ysxs = zip ys xxs
                          zipUp = map (\t -> zip (snd t)(replicate (length (snd t))(fst t))  )

findItem :: WorldItems -> World-> [Pos]
findItem wi  = map snd . filter (\x -> fst x == wi) 

charToItem :: Char ->  WorldItems
charToItem c = fromJust $ lookup c charItemPairs 

itemToChar :: WorldItems -> Char 
itemToChar wi = fromJust $ lookup wi itemCharPairs 

main :: IO ()
main = do
    let playerPos = show . findItem Player . worldCMToWorld $ thisWorld
    let currentWorld =  unlines thisWorld
    putStrLn "Hello, Player"
    putStrLn "Enter Game"
    putStrLn ("You are @, standing at "++playerPos++"; exit via $")
    putStrLn currentWorld


playerMove :: World -> Direction -> World
playerMove world d = undefined
