Haskell ʵ�ֵļ��������㷨

> module Sort (insertionSort,bubbleSort,selectionSort,quickSort,mergeSort) where

0.��������

> insert :: Ord a => a -> [a] -> [a]
> insert x [] = [x]
> insert x (y:ys) | x < y = (x:y:ys)
>                 | otherwise = (y:(insert x ys))

> insertionSort :: Ord a => [a] -> [a] -> [a]
> insertionSort xs [] = xs
> insertionSort xs (y:ys) = insertionSort (insert y xs) ys

1.ð������

> swaps :: Ord a => [a] -> [a]
> swaps [] = []
> swaps [x] = [x]
> swaps (x:y:s) | x > y = y:(swaps $ x:s)
>             | otherwise = x:(swaps $ y:s)

> fix :: Eq a => (a -> a) -> a -> a
> fix f x = if x == x' then x else fix f x'
>       where x' = f x

> bubbleSort :: Ord a => [a] -> [a]
> bubbleSort xs = fix swaps xs

> bubbleSort' :: Ord a => [a] -> [a]
> bubbleSort' [] = []
> bubbleSort' xs = bubbleSort' (init(swaps xs)) ++ [last(swaps xs)]

2.ѡ������

> delete :: Ord a => a -> [a] -> [a]
> delete _ [] = []
> delete x (l:ls) = if x == l then ls else l:(delete x ls)

> selectionSort :: Ord a => [a] -> [a]
> selectionSort [] = []
> selectionSort xs = mini:(selectionSort rest)
>               where mini = minimum xs
>                     rest = delete mini xs

3.��������

> filterSplit :: (a -> Bool) -> [a] -> ([a], [a])
> filterSplit _ [] = ([],[])
> filterSplit f (x:xs) | f x = ((x:l),r)
>                      | otherwise = (l,(x:r))
>                   where (l,r) = filterSplit f xs

> quickSort :: Ord a => [a] -> [a]
> quickSort [] = []
> quickSort [x] = [x]
> quickSort (x:xs) = quickSort l ++ [x] ++ quickSort r
>           where (l,r) = filterSplit (<x) xs

4.�鲢����

> merge :: Ord a => [a] -> [a] -> [a]
> merge xs [] = xs
> merge [] ys = ys
> merge (x:xs) (y:ys) | x > y = y:(merge (x:xs) ys)
>                     | otherwise = x:(merge xs (y:ys))

> mergeSort :: Ord a => [a] -> [a]
> mergeSort [] = []
> mergeSort [x] = [x]
> mergeSort xs = merge (mergeSort l) (mergeSort r)
>           where (l,r) = (take le xs, drop le xs)
>                 le = (length xs) `div` 2



































