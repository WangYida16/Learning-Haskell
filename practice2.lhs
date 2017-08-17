第六章 模块
Haskell函数搜索:  http://www.haskell.org/google/

> import Data.Char
> import Data.List
> import qualified Data.Map as Map

1.模块操作

导入模块
import modouleName
ghci> :m + modouleName

只导入模块中的部分函数
import modouleName (functionName1, functionName2..)

导入除某函数外的其他函数
import modouleName hiding (functionName1,functionName2..)

限定导入
import qualified modouleName( as self-DefinedName)
调用该模块的函数
modouleName.functionName(self-DefinedName.functionName)

2.  Data.List中的几个函数

2.1 words
ghci> words "hey these are the words in this sentence"
["hey","these","are","the","words","in","this","sentence"]
相反的操作: unwords 函数
类似的操作: lines , unlines 函数


2.2 group
ghci> [1,1,1,2,2,3,3,3,4,4,5,6,7,7]
[[1,1,1],[2,2],[3,3,3],[4,4],[5],[6],[7,7]]
ghci> [1,1,1,2,2,3,3,3,4,4,5,6,7,7,1,1]
[[1,1,1],[2,2],[3,3,3],[4,4],[5],[6],[7,7],[1,1]]

2.3 sort
sort xs
将列表xs中的元素按字典顺序排序

例子

> wordNum :: String -> [(String, Int)]
> wordNum = map (\ws -> (head ws, length ws)) . group . sort . words

2.4 tails (注意有s)
ghci> tails [1,2,3]
[[1,2,3],[2,3],[3],[]]
ghci> tails "party"
["party", "arty","rty","ty","y"]

2.5 isPrefixOf
isPrefixOf xsh xsl
取两个参数, 判断第二个列表xsl是否以第一个列表xsh开头

2.6 any
any p xs : 判断 列表xs 中是否有满足 限制条件p 的元素

例子(功能与2.7相同)

> isIn :: (Eq a) => [a] -> [a] -> Bool
> needle `isIn` haystack = any (needle `isPrefixOf`) (tails haystack) 

2.7 isInfixOf 
isInfixOf xss xsl
判断第一个列表xss是否包含在第二个列表xsl之中

2.8 foldl' 和 foldl1'
"严格左折叠",功能不变, 不采用惰性计算, 立即求值,没有"严格右折叠"

2.9 find :: (a -> Bool) -> [a] -> Maybe a  -- 注意Maybe\Just\Nothing的使用
取一个限制条件和一个列表, 返回列表中第一个满足限制条件的元素

2.10 delete :: Eq a => a -> [a] -> [a]
删除列表中第一次出现的元素

3.  Data.Char中的几个函数

3.1 ord :: Char -> Int
ord Char :将字符转换为数值(Unicode)

3.2 chr :: Int -> Char
chr Int :将数值转换为字符(Unicode)

例子

> encode :: Int -> String -> String
> encode offset msg = map (\x -> chr $ ord x + offset) msg -- or : map (chr.(+offset).ord) msg  注意必须要有最外面的那个括号

> decode :: Int -> String -> String
> decode offset msg = map (chr.(subtract offset).ord) msg -- or : encode (negate offset) msg 

3.3 digitToInt :: Char -> Int
将字符转为十六进制数字

例子

> digitSum :: Int -> Int
> digitSum  = sum . map digitToInt . show

> firstTo :: Int -> Maybe Int
> firstTo n = find (\x -> digitSum x == n) [1..]

3.4 isDigit :: Char -> Bool



4.映射键和值

> findKey1 :: (Eq k) => k -> [(k, b)] -> Maybe b
> findKey1 _ [] = Nothing
> findKey1 key ((k,v):xs)
>     | k == key = Just v
>     | otherwise = findKey1 key xs


> findKey2 :: (Eq k) => k -> [(k, b)] ->  b
> findKey2 key = foldr (\(k,v) acc -> if k == key then v else acc) Nothing 

以上两个函数是5.1 lookup的功能

5.  Data.Map中的一些函数

5.1 lookup :: Ord k => k -> Map k a -> Maybe a
根据键求值

5.2 fromList :: Ord k => [(k, a)] -> Map k a
将一个关联列表转换为等价的映射
	若原列表中存在相同的键,则只取最后一个,忽略前面所有

5.3 insert :: Ord k => k -> a -> Map k a -> Map k a

5.4 size :: Map k a -> Int

5.5 fromListWith  :: Ord k => (a -> a -> a) -> [(k, a)] -> containers-0.5.7.1:Data.Map.Base.Map k a
例子

> phonebookToMap1 :: (Ord k) => [(k, String)] -> Map.Map k String
> phonebookToMap1  = Map.fromListWith add
>     where add x y  = x ++ "," ++ y

> phonebookToMap2 :: (Ord k) => [(k, a)] -> Map.Map k [a]
> phonebookToMap2 xs = Map.fromListWith (++) $ map (\(k,v) -> (k,[v])) xs -- 似乎不能柯里化Orz




