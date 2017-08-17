Haskell趣学指南--代码熟悉

第三章

3.1 模式匹配

(1)检查传给它的数是不是7

> lucky :: Int -> String
> lucky 7 = "Lucky Number Seven!"
> lucky x = "Sorry, you're out of luck, pal!"

(2)展开列表的前几项

> tell :: (Show a) => [a] -> String
> tell [] = "This list is empty"
> tell (x:[]) = "This list has one element: " ++ show x
> tell(x:y:[]) = "This list has two elements: " ++ show x ++ "and" ++ show y
> tell(x:y:_) = "This list is long. The first two elements are: " ++ show x ++ "and" ++ show y

(3)使用As模式, 返回字符串的第一个字符

> firstletter :: String -> String
> firstletter "" = "Empty string, whoops!"
> firstletter all@(x:xs) = "The first letter of " ++ all ++ " is " ++[x]

3.2 哨卫

(1)BMI-1

> bmiTell1 :: Double -> String
> bmiTell1 bmi
>     | bmi <= 18.5 = "Underweight"
>     | bmi <= 25.0 = "Normal"
> 	  | bmi <= 30.0 = "Fat"
> 	  | otherwise = "Whale!!!"

3.3 where
应用where的BMI-2 

> bmiTell2 :: Double -> Double -> String
> bmiTell2 weight height
>     | bmi <= skinny = "Underweight"
>     | bmi <= normal = "Normal"
> 	  | bmi <= fat = "Fat"
> 	  | otherwise = "Whale!!!"
>     where bmi = weight / height ^ 2
>           skinny = 18.5
>           normal = 25.0
>           fat = 30.0

3.4 let表达式

> cylinder :: Double -> Double -> Double
> cylinder r h = 
>     let sideArea = 2 * pi *r * h
>         topArea = pi * r ^2
>     in sideArea + 2 * topArea

3.5 case表达式

> describeList :: [a] -> String
> describeList ls = "The list is " ++ case ls of [] -> "empty."
>                                                [x] -> "a singleton list."
>                                                xs -> "a longer list."



第四章 递归

一些函数的实现

(1)maximum

> maximum' :: (Ord a) => [a] -> a
> maximum' [] = error "maximum of empty list"
> maximum' [x] = x
> maximum' (x:xs) = max x (maximum' xs)

(2)replicate

> replicate' :: Int -> a -> [a]
> replicate' n x
>     | n <= 0 = []
>     | otherwise = x:replicate' (n-1) x

(3)take

> take' :: (Num i, Ord i) => i -> [a] -> [a]
> take' n _
>     | n <= 0 = []   --没有指定otherwise, 意味着若 n > 0, 则转入下一个模式
> take' _ []   = []
> take' n (x:xs) = x : take' (n-1) xs

(4)reverse

> reverse' :: [a] -> [a]
> reverse' [] = []
> reverse' (x:xs) = reverse' xs ++ [x]

(5)repeat

> repeat' :: a -> [a]
> repeat' x = x : repeat' x

(6)zip

> zip' :: [a] -> [b] -> [(a,b)]
> zip' _ [] = []
> zip' [] _ = []
> zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

(7)elem

> elem' :: (Eq a) => a -> [a] -> Bool
> elem' _ [] = False
> elem' a (x:xs)
>     | x == a = True
>     | otherwise = elem' a xs

(8)quicksort

> quicksort' :: (Ord a) => [a] -> [a]
> quicksort' [] = []
> quicksort' (x:xs) = 
>     let smallerOrEqual = [a | a <- xs, a <= x]
>         larger = [a | a <- xs, a > x]
>     in quicksort' smallerOrEqual ++ [x] ++ quicksort' larger


第五章 高阶函数

	我们接着实现

(1)zipWith

> zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
> zipWith' _ [] _ = []
> zipWith' _ _ [] = []
> zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

(2)flip

> flip1 :: (a -> b -> c) -> b -> a -> c
> flip1 f = g
>     where g x y = f y x

> flip2 :: (a -> b -> c) -> b -> a -> c
> flip2 f x y = f y x

(3)map

> map' :: (a -> b) -> [a] -> [b]
> map' _ [] = []
> map' f (x:xs) = f x : map' f xs

(4)filter

> filter' :: (a -> Bool) -> [a] -> [a]
> filter' _ [] = []
> filter' p (x:xs)
>     | p x = x : filter' p xs
>     | otherwise = filter' p xs

(5)Collatz sequence

> createChain :: Integer -> [Integer]
> createChain 1 = [1]
> createChain n
>     | odd n = n : createChain (3 * n + 1)
>     | even n = n : createChain (n `div` 2)

> numLongChains :: Integer -> Int
> numLongChains n = length (filter isLong (map createChain [1..n]))--注意length返回值为Int
>     where isLong xs = length xs > 15


(6)lambda
	
  几个例子

ghci> zipWith (\a b -> (a * 30 + 3) / b) [5,4,3,2,1] [1,2,3,4,5]
[153.0,61.5,31.0,15.75,6.6]

addThree :: Int -> Int -> Int -> Int
addThree = \x -> \y -> \z -> x + y + z

flip'' :: (a -> b -> c) -> b -> a -> c
flip'' f = \x y -> f y x

(7)fold函数
  取一个 二元函数 一个初始值 一个待折叠的列表
 fold函数取初始值和列表的起始元素来应用二元函数,得到返回值作为新的累加值,以此类推...
 
(8)foldl左折叠的例子

foldl 的二元函数 第一个参数为 累加值 第二个参数为 列表

> sum' :: (Num a) => [a] -> a
> sum' xs = foldl (\acc x -> acc + x) 0 xs --或写成sum' = foldl (+) 0

> map'' :: (a -> b) -> [a] -> [b]
> map'' f xs = foldl (\acc x -> acc ++ [f x]) [] xs

(9)foldr右折叠的例子

foldr 的二元函数 第一个参数为 列表 第二个参数为 累加值

> map''' :: (a -> b) -> [a] -> [b]
> map''' f xs = foldr (\x acc -> f x : acc) [] xs

> elem'' ::(Eq a) => a -> [a] -> Bool
> elem'' y ys = foldr (\x acc -> if x == y then True else acc) False ys

(10)foldl1与foldr1(最后一个字符是数字1)
不需要明确提供初始值,直接以首\尾元素为初始值,并从旁边的元素开始折叠

> maximum'' :: (Ord a) => [a] -> a
> maximum'' = foldl1 max


再来一些折叠的例子
(11)reverse, product, filter, last

> reverse'' :: [a] -> [a]
> reverse''  = foldl (\acc x -> x : acc) []--或者reverse = foldl (flip (:)) []

> product'' :: (Num a) => [a] -> a
> product'' = foldl (*) 1

> filter'' :: (a -> Bool) -> [a] -> [a]
> filter'' p = foldr (\x acc -> if p x then x : acc else acc) []

> last' :: [a] -> a
> last' = foldl1 (\_ x -> x)


(12)and

> and' :: [Bool] -> Bool
> and' = foldr (&&) True

(13)扫描
  scanl和scanr
  取一个 二元函数 一个初始值 一个待扫描的列表
  取列表的起始元素(首\尾)和初始值应用二元函数,返回值作为新的累加值,记录包括初始值在内的所有累加值
  scanl将累加值在列表中从左到右排列,scanr则从右到左排列

  也有scanl1和scanr1
  
  例子
ghci> scanl (+) 0 [3,5,2,1]
[0,3,8,10,11]

ghci> scanr (+) 0 [3,5,2,1]
{11,8,3,1,0]

ghci> scanl (flip (:)) [] [3,2,1]
[[],[3],[2,3], [1,2,3]]


(14)将自然数的平方和相加,会在何处超过1000?

sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1


(15) $ : 函数应用符
($) :: (a -> b) -> a -> b
f $ x = f x
$优先级最低,且为右结合
作用是

*减少括号
如 sqrt (3+4+9) 可以写成 sqrt % 3+4+9
   sum (filter (>10) (map (*2) [2..10])) 写成 sum $ filter (>10) $ map (*2) [2..10]

*将函数应用转为函数
如 
ghci> map ($ 3) [(4+),(10*),(^2),sqrt] 
[7.0,30.0,9.0,1.7320508075688772]

(16)函数组合

(.) :: (b -> c) -> (a -> b) -> a -> c
f.g x = \x -> f (g x)

例子
sum (replicate 5 (max 6.7 8.9))
(sum.replicate 5)(max 6.7 8.9)
sum.replicate5 $ max 6.7 8.9

replicate 2 (product (map (*3) (zipWith max [1,2] [4,5])))
replicate2 .product.map (*3) $ zipWith max [1,2] [4,5]



(17)Point-Free风格

fn x = ceiling (negate (tan (cos (max 50 x))))
fn = ceiling.negate.tan.cos.max 50

> oddSquareSum1 :: Integer
> oddSquareSum1 = sum (takeWhile (< 10000) (filter odd (map (^2) [1..])))

> oddSquareSum2 :: Integer
> oddSquareSum2 = sum.takeWhile (<10000).filter odd $ map (^2) [1..]






























