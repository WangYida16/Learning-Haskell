import Control.Monad

lists :: Int -> [String]
lists 0 = replicate 62 (replicate 100 '_') ++ [(replicate 49 '_' ++ "1" ++ replicate 50 '_')]
lists n =  extendBranches n (extendTrunk n (lists (n-1)))

extendTrunk :: Int -> [String] -> [String]
extendTrunk n list = zipWith (\n s -> if n<=end && n>=start then last else s) [1..63] (lists (n-1))
                        where end = 2^(7-n) - 1
                              start = 2^(6-n) + 2^(5-n)
                              last = if n==1 then list!!62 else list!!end

createBranches :: String -> String
createBranches ('_':'1':'_':xs) = ('1':'_':'1':(createBranches xs))
createBranches ('_':'_':xs) = ('_':(createBranches ('_':xs)))
createBranches s = s

extendBranches :: Int -> [String] -> [String]
extendBranches n list = zipWith (\m s -> if m<=end && m>=start then dealBranch end m list else s) [1..63] list
                        where end = 2^(6-n) + 2^(5-n) - 1
                              start = 2^(6-n)

dealBranch :: Int -> Int -> [String] -> String
dealBranch end m list
        | end==m = createBranches (list!!m)
        | otherwise = toLeft(dealBranch end (m+1) list)
toLeft :: String -> String
toLeft ('_':'1':'_':xs) = ('1':'_':(toRight ('_':xs)))
toLeft ('_':'_':xs) = ('_':toLeft('_':xs))
toLeft s = s
toRight :: String -> String
toRight ('_':'1':'_':xs) = ('_':'_':'1':(toLeft xs))
toRight ('_':'_':xs) = ('_':toRight('_':xs))

main = do
  numS <- getLine
  let num = read numS ::Int
  forM_ (lists num) $ \s -> putStrLn s
