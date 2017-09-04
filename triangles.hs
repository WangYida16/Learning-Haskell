import Control.Monad

main = do
    numS <- getLine
    let num = read numS :: Int
    forM_ (lists num) $ \s -> putStrLn s

lists :: Int -> [String]
lists 0 = map (\n -> (replicate ((64 - 2 * n)`div`2) '_') ++ (replicate (2 * n - 1) '1') ++ (replicate ((64 - 2 * n)`div`2) '_') ) [1..32]
lists n = zipWith (\s neg -> zipWith (\cs cn -> if cs=='1'&&cn=='~' then '_' else cs) s neg) (lists(n-1)) (rEVERSE n h negs)
            where h = 32 `div` (2^n)
                  negs = zipWith (\s c -> if c=='~' then map (\ch -> if ch=='1' then '~' else ch ) s else s ) (lists(n-1)) $
                          concat $ repeat (replicate h '~' ++ replicate h '@')

rEVERSE :: Int -> Int -> [String] -> [String]
rEVERSE n h ss = if ss == [] then [] else ((reverse (take (2 * h) ss)) ++ rEVERSE n h (drop (2 * h) ss))
