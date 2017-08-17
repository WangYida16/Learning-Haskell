> import System.IO

第八章 输入与输出

0.do代码块可将多个I/O操作合成为一个I/O操作

1. putStrLn :: String -> IO ()
 显示字符串，之后换行

2. getLine :: IO String
eg. name <- getLine
 将用户的输入表示为一个字符串

3. <-
 "<-"用于执行I/O操作,并为操作的结果绑定名字

4. return
 基于一个纯的值来构造I/O操作, 功能与 <- 相反
 return取一个值作为参数装入容器, <- 取一个容器,从中取出值并进行绑定
 return构造的I/O操作实际上不会做任何事情,也不会中断I/O do代码块的执行

5. putStr
 显示字符串，不换行
 
6. putChar
 取一个字符做参数,返回一个在终端返回字符的I/O操作
 
7. print
 取一个Show的实例类型的值作为参数,对它应用show使之字符串化,然后将字符串输出到终端
 相当于 putStrLn.show
 
8. when
位置: Control.Monad
作用: 取 一个布尔值 和 一个I/O操作 , 如果 布尔值为真,则将I/O操作原样返回, 否则,返回一个return ()

9. sequence
取一组I/O操作组成的列表作为参数,返回一个将列表中I/O操作依次执行的I/O操作. 最后的返回值为 列表中所有I/O操作执行后的结果组成的列表.

10. mapM 与 mapM_
 mapM取一个函数和一个列表, 将函数映射到列表上, 然后对结果应用sequence
 mapM_ 不保留最终结果

> testmapM = mapM print [1,2,3]

结果如下
1
2
3
[(),(),()]

> testmapM_ = mapM_ print [1,2,3]

结果如下
1
2
3

11. forever
位置-Control.Monad
 取一个I/O操作作为参数, 返回一个永远重复执行该I/O操作的I/O操作

12. forM
位置-Control.Monad
 与mapM相似, 不过第一个参数是列表, 第二个参数是函数, 最后将I/O操作排为序列







第九章  更多的输入输出操作

1. getContents :: IO String
从标准输入中读取所有内容,直到遇到一个end-of-line 字符
!!!惰性的

2. interact
取一个类型为Stirng->String的函数为参数, 返回一个I/O操作: 接受一个输入, 把函数作用在输入, 输出结果

> egInteract = interact respondPalindromes
> respondPalindromes :: String -> String
> respondPalindromes = unlines.
>                      map (\xs -> if isPal xs then "palindrome" else "not a palindrome") .
>                      lines
> isPal :: String -> Bool
> isPal xs = xs == reverse xs

3. openFile :: FilePath -> IOMode ->IO Handle
 type FilePath = String
 data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode
位置:System.IO

4. hGetContents :: Handle -> IO String
也是惰性的
位置:System.IO

5. hClose :: Handle -> IO ()
位置:System.IO

6. withFile :: FilePath -> IOMode -> (Handle -> IO r) -> IO r
接受一个文件路径、一个IOMode、一个函数(接受一个句柄作为参数, 返回一个I/O操作),
然后返回一个I/O操作: 打开文件, 对文件应用函数, 关闭文件.
如果出错了,withFile会确保文件句柄被关闭.
位置:System.IO

> myDisplay = do 
>     withFile "target.txt" ReadMode (\handle -> do
>         contents <- hGetContents handle
>         putStrLn contents)

7. bracket :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
位置:Control.Exception
第一个参数为 一个请求资源的I/O操作
第二个参数为 一个释放资源的函数, bracket确保即使出现异常也会调用它
第三个函数为 主要操作,如读写

8. hPutStr, hPutStrLn, hGetLine, hGetChar

9. readFile, writeFile, appendFile

10. openTempFile :: FilePath -> String -> IO (FilePath, Handle)

11. removeFile, renameFile
位置: System.Directory
接收文件路径作为参数

12. bracketOnError :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
只有当异常产生时才会执行清理操作

13.getArgs :: IO [String]
获取伴随程序运行的命令行参数列表

14.getProgName :: IO String
获取程序名









