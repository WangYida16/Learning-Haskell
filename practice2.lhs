������ ģ��
Haskell��������:  http://www.haskell.org/google/

> import Data.Char
> import Data.List
> import qualified Data.Map as Map

1.ģ�����

����ģ��
import modouleName
ghci> :m + modouleName

ֻ����ģ���еĲ��ֺ���
import modouleName (functionName1, functionName2..)

�����ĳ���������������
import modouleName hiding (functionName1,functionName2..)

�޶�����
import qualified modouleName( as self-DefinedName)
���ø�ģ��ĺ���
modouleName.functionName(self-DefinedName.functionName)

2.  Data.List�еļ�������

2.1 words
ghci> words "hey these are the words in this sentence"
["hey","these","are","the","words","in","this","sentence"]
�෴�Ĳ���: unwords ����
���ƵĲ���: lines , unlines ����


2.2 group
ghci> [1,1,1,2,2,3,3,3,4,4,5,6,7,7]
[[1,1,1],[2,2],[3,3,3],[4,4],[5],[6],[7,7]]
ghci> [1,1,1,2,2,3,3,3,4,4,5,6,7,7,1,1]
[[1,1,1],[2,2],[3,3,3],[4,4],[5],[6],[7,7],[1,1]]

2.3 sort
sort xs
���б�xs�е�Ԫ�ذ��ֵ�˳������

����

> wordNum :: String -> [(String, Int)]
> wordNum = map (\ws -> (head ws, length ws)) . group . sort . words

2.4 tails (ע����s)
ghci> tails [1,2,3]
[[1,2,3],[2,3],[3],[]]
ghci> tails "party"
["party", "arty","rty","ty","y"]

2.5 isPrefixOf
isPrefixOf xsh xsl
ȡ��������, �жϵڶ����б�xsl�Ƿ��Ե�һ���б�xsh��ͷ

2.6 any
any p xs : �ж� �б�xs ���Ƿ������� ��������p ��Ԫ��

����(������2.7��ͬ)

> isIn :: (Eq a) => [a] -> [a] -> Bool
> needle `isIn` haystack = any (needle `isPrefixOf`) (tails haystack) 

2.7 isInfixOf 
isInfixOf xss xsl
�жϵ�һ���б�xss�Ƿ�����ڵڶ����б�xsl֮��

2.8 foldl' �� foldl1'
"�ϸ����۵�",���ܲ���, �����ö��Լ���, ������ֵ,û��"�ϸ����۵�"

2.9 find :: (a -> Bool) -> [a] -> Maybe a  -- ע��Maybe\Just\Nothing��ʹ��
ȡһ������������һ���б�, �����б��е�һ����������������Ԫ��

2.10 delete :: Eq a => a -> [a] -> [a]
ɾ���б��е�һ�γ��ֵ�Ԫ��

3.  Data.Char�еļ�������

3.1 ord :: Char -> Int
ord Char :���ַ�ת��Ϊ��ֵ(Unicode)

3.2 chr :: Int -> Char
chr Int :����ֵת��Ϊ�ַ�(Unicode)

����

> encode :: Int -> String -> String
> encode offset msg = map (\x -> chr $ ord x + offset) msg -- or : map (chr.(+offset).ord) msg  ע�����Ҫ����������Ǹ�����

> decode :: Int -> String -> String
> decode offset msg = map (chr.(subtract offset).ord) msg -- or : encode (negate offset) msg 

3.3 digitToInt :: Char -> Int
���ַ�תΪʮ����������

����

> digitSum :: Int -> Int
> digitSum  = sum . map digitToInt . show

> firstTo :: Int -> Maybe Int
> firstTo n = find (\x -> digitSum x == n) [1..]

3.4 isDigit :: Char -> Bool



4.ӳ�����ֵ

> findKey1 :: (Eq k) => k -> [(k, b)] -> Maybe b
> findKey1 _ [] = Nothing
> findKey1 key ((k,v):xs)
>     | k == key = Just v
>     | otherwise = findKey1 key xs


> findKey2 :: (Eq k) => k -> [(k, b)] ->  b
> findKey2 key = foldr (\(k,v) acc -> if k == key then v else acc) Nothing 

��������������5.1 lookup�Ĺ���

5.  Data.Map�е�һЩ����

5.1 lookup :: Ord k => k -> Map k a -> Maybe a
���ݼ���ֵ

5.2 fromList :: Ord k => [(k, a)] -> Map k a
��һ�������б�ת��Ϊ�ȼ۵�ӳ��
	��ԭ�б��д�����ͬ�ļ�,��ֻȡ���һ��,����ǰ������

5.3 insert :: Ord k => k -> a -> Map k a -> Map k a

5.4 size :: Map k a -> Int

5.5 fromListWith  :: Ord k => (a -> a -> a) -> [(k, a)] -> containers-0.5.7.1:Data.Map.Base.Map k a
����

> phonebookToMap1 :: (Ord k) => [(k, String)] -> Map.Map k String
> phonebookToMap1  = Map.fromListWith add
>     where add x y  = x ++ "," ++ y

> phonebookToMap2 :: (Ord k) => [(k, a)] -> Map.Map k [a]
> phonebookToMap2 xs = Map.fromListWith (++) $ map (\(k,v) -> (k,[v])) xs -- �ƺ����ܿ��ﻯOrz




