module Main where
import Control.Monad (liftM2)


data Direction = North | East | South | West 

type WorldMap = [String] 

thisWorld :: WorldMap
thisWorld =  ["........"
             ,"......--"
             ,"--......"
             ,"....----"
             ,"........"]


type Pos = (Int, Int) 

data WorldItems = Clear | Wall | Foe | Unknown deriving Show

type ItemPos = (WorldItems, Pos)

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

charToItem :: Char -> WorldItems
charToItem c = case c of 
                    '.' -> Clear
                    '-' -> Wall
                    '#' -> Foe
                    _ -> Unknown

main :: IO ()
main = do
    putStrLn "Enter Game"
    
