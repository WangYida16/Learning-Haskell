HaskellȤѧָ��--������Ϥ

������

3.1 ģʽƥ��

(1)��鴫���������ǲ���7

> lucky :: Int -> String
> lucky 7 = "Lucky Number Seven!"
> lucky x = "Sorry, you're out of luck, pal!"

(2)չ���б��ǰ����

> tell :: (Show a) => [a] -> String
> tell [] = "This list is empty"
> tell (x:[]) = "This list has one element: " ++ show x
> tell(x:y:[]) = "This list has two elements: " ++ show x ++ "and" ++ show y
> tell(x:y:_) = "This list is long. The first two elements are: " ++ show x ++ "and" ++ show y

(3)ʹ��Asģʽ, �����ַ����ĵ�һ���ַ�

> firstletter :: String -> String
> firstletter "" = "Empty string, whoops!"
> firstletter all@(x:xs) = "The first letter of " ++ all ++ " is " ++[x]

3.2 ����

(1)BMI-1

> bmiTell1 :: Double -> String
> bmiTell1 bmi
>     | bmi <= 18.5 = "Underweight"
>     | bmi <= 25.0 = "Normal"
> 	  | bmi <= 30.0 = "Fat"
> 	  | otherwise = "Whale!!!"

3.3 where
Ӧ��where��BMI-2 

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

3.4 let���ʽ

> cylinder :: Double -> Double -> Double
> cylinder r h = 
>     let sideArea = 2 * pi *r * h
>         topArea = pi * r ^2
>     in sideArea + 2 * topArea

3.5 case���ʽ

> describeList :: [a] -> String
> describeList ls = "The list is " ++ case ls of [] -> "empty."
>                                                [x] -> "a singleton list."
>                                                xs -> "a longer list."



������ �ݹ�

һЩ������ʵ��

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
>     | n <= 0 = []   --û��ָ��otherwise, ��ζ���� n > 0, ��ת����һ��ģʽ
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


������ �߽׺���

	���ǽ���ʵ��

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
> numLongChains n = length (filter isLong (map createChain [1..n]))--ע��length����ֵΪInt
>     where isLong xs = length xs > 15


(6)lambda
	
  ��������

ghci> zipWith (\a b -> (a * 30 + 3) / b) [5,4,3,2,1] [1,2,3,4,5]
[153.0,61.5,31.0,15.75,6.6]

addThree :: Int -> Int -> Int -> Int
addThree = \x -> \y -> \z -> x + y + z

flip'' :: (a -> b -> c) -> b -> a -> c
flip'' f = \x y -> f y x

(7)fold����
  ȡһ�� ��Ԫ���� һ����ʼֵ һ�����۵����б�
 fold����ȡ��ʼֵ���б����ʼԪ����Ӧ�ö�Ԫ����,�õ�����ֵ��Ϊ�µ��ۼ�ֵ,�Դ�����...
 
(8)foldl���۵�������

foldl �Ķ�Ԫ���� ��һ������Ϊ �ۼ�ֵ �ڶ�������Ϊ �б�

> sum' :: (Num a) => [a] -> a
> sum' xs = foldl (\acc x -> acc + x) 0 xs --��д��sum' = foldl (+) 0

> map'' :: (a -> b) -> [a] -> [b]
> map'' f xs = foldl (\acc x -> acc ++ [f x]) [] xs

(9)foldr���۵�������

foldr �Ķ�Ԫ���� ��һ������Ϊ �б� �ڶ�������Ϊ �ۼ�ֵ

> map''' :: (a -> b) -> [a] -> [b]
> map''' f xs = foldr (\x acc -> f x : acc) [] xs

> elem'' ::(Eq a) => a -> [a] -> Bool
> elem'' y ys = foldr (\x acc -> if x == y then True else acc) False ys

(10)foldl1��foldr1(���һ���ַ�������1)
����Ҫ��ȷ�ṩ��ʼֵ,ֱ������\βԪ��Ϊ��ʼֵ,�����Աߵ�Ԫ�ؿ�ʼ�۵�

> maximum'' :: (Ord a) => [a] -> a
> maximum'' = foldl1 max


����һЩ�۵�������
(11)reverse, product, filter, last

> reverse'' :: [a] -> [a]
> reverse''  = foldl (\acc x -> x : acc) []--����reverse = foldl (flip (:)) []

> product'' :: (Num a) => [a] -> a
> product'' = foldl (*) 1

> filter'' :: (a -> Bool) -> [a] -> [a]
> filter'' p = foldr (\x acc -> if p x then x : acc else acc) []

> last' :: [a] -> a
> last' = foldl1 (\_ x -> x)


(12)and

> and' :: [Bool] -> Bool
> and' = foldr (&&) True

(13)ɨ��
  scanl��scanr
  ȡһ�� ��Ԫ���� һ����ʼֵ һ����ɨ����б�
  ȡ�б����ʼԪ��(��\β)�ͳ�ʼֵӦ�ö�Ԫ����,����ֵ��Ϊ�µ��ۼ�ֵ,��¼������ʼֵ���ڵ������ۼ�ֵ
  scanl���ۼ�ֵ���б��д���������,scanr����ҵ�������

  Ҳ��scanl1��scanr1
  
  ����
ghci> scanl (+) 0 [3,5,2,1]
[0,3,8,10,11]

ghci> scanr (+) 0 [3,5,2,1]
{11,8,3,1,0]

ghci> scanl (flip (:)) [] [3,2,1]
[[],[3],[2,3], [1,2,3]]


(14)����Ȼ����ƽ�������,���ںδ�����1000?

sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1


(15) $ : ����Ӧ�÷�
($) :: (a -> b) -> a -> b
f $ x = f x
$���ȼ����,��Ϊ�ҽ��
������

*��������
�� sqrt (3+4+9) ����д�� sqrt % 3+4+9
   sum (filter (>10) (map (*2) [2..10])) д�� sum $ filter (>10) $ map (*2) [2..10]

*������Ӧ��תΪ����
�� 
ghci> map ($ 3) [(4+),(10*),(^2),sqrt] 
[7.0,30.0,9.0,1.7320508075688772]

(16)�������

(.) :: (b -> c) -> (a -> b) -> a -> c
f.g x = \x -> f (g x)

����
sum (replicate 5 (max 6.7 8.9))
(sum.replicate 5)(max 6.7 8.9)
sum.replicate5 $ max 6.7 8.9

replicate 2 (product (map (*3) (zipWith max [1,2] [4,5])))
replicate2 .product.map (*3) $ zipWith max [1,2] [4,5]



(17)Point-Free���

fn x = ceiling (negate (tan (cos (max 50 x))))
fn = ceiling.negate.tan.cos.max 50

> oddSquareSum1 :: Integer
> oddSquareSum1 = sum (takeWhile (< 10000) (filter odd (map (^2) [1..])))

> oddSquareSum2 :: Integer
> oddSquareSum2 = sum.takeWhile (<10000).filter odd $ map (^2) [1..]






























