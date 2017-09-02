
positions :: Int -> Int -> [[Int]]
positions 0 n = [[]]
positions k n  = [x:xs | x <- [1..n], xs <- positions (k-1) n]

noSameRow :: [Int] -> Bool
noSameRow [] = True
noSameRow (x:xs) = (not $ elem x xs) && noSameRow xs

noSameDiag :: [Int] -> Bool
noSameDiag [] = True
noSameDiag xs@(x:xs') = and [abs(i1 - i) /= abs(p1 - p) | (i,p) <- ip] && noSameDiag xs'
        where (i1,p1):ip = zip [1..] xs

{-极慢的一个方法-}
queen1 :: Int -> [[Int]]
queen1 n = [xs | xs <- positions n n, noSameRow xs, noSameDiag xs]



positions' :: Int -> Int -> [[Int]]
positions' 0 k = [[]]
positions' k n = [p:ps | ps <- positions' (k - 1) n, p <- [1..n], isSafe p ps]

isSafe :: Int -> [Int] -> Bool
isSafe p ps = not ((elem p ps) || (sameDiag p ps))
        where sameDiag p ps = any (\(dist, q) -> abs(p - q) == dist) $ zip [1..] ps

queen2 :: Int -> [[Int]]
queen2 n = positions' n n
