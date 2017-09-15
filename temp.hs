--unfinished

import Control.Monad
import Data.List hiding (insert)

testChart = ["+-++++++++","+-++++++++","+-------++","+-++++++++","+-++++++++","+------+++","+-+++-++++","+++++-++++","+++++-++++","++++++++++"]
testWords = ["AGRA","NORWAY","ENGLAND","GWALIOR"]


preChart = [[(1,1),(1,2),(1,3),(1,4),(1,5),(1,6),(1,7),(1,8),(1,9),(1,10)],
            [(2,1),(2,2),(2,3),(2,4),(2,5),(2,6),(2,7),(2,8),(2,9),(2,10)],
            [(3,1),(3,2),(3,3),(3,4),(3,5),(3,6),(3,7),(3,8),(3,9),(3,10)],
            [(4,1),(4,2),(4,3),(4,4),(4,5),(4,6),(4,7),(4,8),(4,9),(4,10)],
            [(5,1),(5,2),(5,3),(5,4),(5,5),(5,6),(5,7),(5,8),(5,9),(5,10)],
            [(6,1),(6,2),(6,3),(6,4),(6,5),(6,6),(6,7),(6,8),(6,9),(6,10)],
            [(7,1),(7,2),(7,3),(7,4),(7,5),(7,6),(7,7),(7,8),(7,9),(7,10)],
            [(8,1),(8,2),(8,3),(8,4),(8,5),(8,6),(8,7),(8,8),(8,9),(8,10)],
            [(9,1),(9,2),(9,3),(9,4),(9,5),(9,6),(9,7),(9,8),(9,9),(9,10)],
            [(10,1),(10,2),(10,3),(10,4),(10,5),(10,6),(10,7),(10,8),(10,9),(10,10)]]

data Info = Info {
    coordinate :: (Int, Int),
    status     :: Int,
    content    :: Char
  } deriving(Show, Eq)
-- Info (1,1) 1 'a'

getMultipleLines :: Int -> IO [String]
getMultipleLines n
    | n <= 0 = return []
    | otherwise = do
        x <- getLine
        xs <- getMultipleLines (n-1)
        let ret = (x:xs)
        return ret

transfer :: [String] -> [[Info]]
transfer xs = map (map (\(co, ch) -> if ch == '+' then Info co 1 '+' else Info co 0 ' ')) xs'
        where xs' = zipWith (zip) preChart xs

showChart xs = do
        forM_ xs $ \s -> putStrLn $ (map content) s

vacancy :: [[Info]] -> [[Info]]
vacancy chart =  ( foldl (\acc x -> pickup x ++ acc) [] chart ) ++ ( foldl (\acc x -> pickup x ++ acc) [] (transpose chart) )

pickup :: [Info] -> [[Info]]
pickup [] = []
pickup [a] = []
pickup all@(x:xs) = if status x == 1 then pickup xs 
                    else if status (head xs) == 1 then pickup (tail xs)
                        else [takeWhile (\t -> status t /= 1) all] ++ (pickup (dropWhile (\t -> status t /= 1) all))        

markRep :: [[Info]] -> [[Info]]
markRep a = map (map (\x -> if (coordinate x) `elem` reps then Info (coordinate x) (-1) (content x) else x)) a 
                where coos = concat (map (map coordinate) a)
                      reps = snd $ repetition (coos, [])

repetition :: ([(Int, Int)], [(Int, Int)]) -> ([(Int, Int)],[(Int, Int)])
repetition ([],r) = ([],r)
repetition ((x:xs), reserve) = if x`elem`xs then repetition ((delete x xs), (x:reserve)) else repetition (xs, reserve)            
                        
                        
fillIn :: [[Info]] -> String -> [Info] -> [[Info]]
fillIn chart word coo = if row then take (rowN-1) chart ++ [take (colSta-1) (chart!!(rowN-1)) ++ combine ++ drop colEnd (chart!!(rowN-1))] ++ drop rowN chart   
                            else transpose ( take (rowN-1) chart ++ [take (colSta-1) (chart!!(rowN-1)) ++ combine ++ drop colEnd (chart!!(rowN-1))] ++ drop rowN chart )
                        where row = (fst $ coordinate $ coo!!0 )== (fst $ coordinate $ coo!!1 )
                              rowN = if row then fst $ coordinate $ coo!!0 else snd $ coordinate $ coo!!0
                              colSta = if row then snd $ coordinate $ coo!!0 else fst $ coordinate $ coo!!0
                              colEnd = colSta + length coo - 1
                              combine = zipWith (\ch info -> Info (coordinate info) (if status info == 0 then 1 else (-1)) ch) word coo 


iPermutations :: [String] -> [[String]]
iPermutations [] = []
iPermutations xs = takeWhile (\x -> length x == length (head xs)) ss : (iPermutations $ dropWhile (\x -> length x == length (head xs)) ss)
        where ss = iSort [] xs
                              
                              

insert x [] = [x]
insert x (y:ys) | length x < length y = (x:y:ys)
                | otherwise = (y:(insert x ys))
iSort xs [] = xs
iSort xs (y:ys) = iSort (insert y xs) ys

                              
                              
                              
{-
main = do
    temp <- getMultipleLines 10
    word' <- getLine
    let
      word = words $ map (\c -> if c == ';' then ' ' else c) word'
      emptyChart = transfer temp
      out = fill word emptyChart
    showChart emptyChart--out

















-}
