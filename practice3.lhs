> import System.IO

�ڰ��� ���������

0.do�����ɽ����I/O�����ϳ�Ϊһ��I/O����

1. putStrLn :: String -> IO ()
 ��ʾ�ַ�����֮����

2. getLine :: IO String
eg. name <- getLine
 ���û��������ʾΪһ���ַ���

3. <-
 "<-"����ִ��I/O����,��Ϊ�����Ľ��������

4. return
 ����һ������ֵ������I/O����, ������ <- �෴
 returnȡһ��ֵ��Ϊ����װ������, <- ȡһ������,����ȡ��ֵ�����а�
 return�����I/O����ʵ���ϲ������κ�����,Ҳ�����ж�I/O do������ִ��

5. putStr
 ��ʾ�ַ�����������
 
6. putChar
 ȡһ���ַ�������,����һ�����ն˷����ַ���I/O����
 
7. print
 ȡһ��Show��ʵ�����͵�ֵ��Ϊ����,����Ӧ��showʹ֮�ַ�����,Ȼ���ַ���������ն�
 �൱�� putStrLn.show
 
8. when
λ��: Control.Monad
����: ȡ һ������ֵ �� һ��I/O���� , ��� ����ֵΪ��,��I/O����ԭ������, ����,����һ��return ()

9. sequence
ȡһ��I/O������ɵ��б���Ϊ����,����һ�����б���I/O��������ִ�е�I/O����. ���ķ���ֵΪ �б�������I/O����ִ�к�Ľ����ɵ��б�.

10. mapM �� mapM_
 mapMȡһ��������һ���б�, ������ӳ�䵽�б���, Ȼ��Խ��Ӧ��sequence
 mapM_ ���������ս��

> testmapM = mapM print [1,2,3]

�������
1
2
3
[(),(),()]

> testmapM_ = mapM_ print [1,2,3]

�������
1
2
3

11. forever
λ��-Control.Monad
 ȡһ��I/O������Ϊ����, ����һ����Զ�ظ�ִ�и�I/O������I/O����

12. forM
λ��-Control.Monad
 ��mapM����, ������һ���������б�, �ڶ��������Ǻ���, ���I/O������Ϊ����







�ھ���  ����������������

1. getContents :: IO String
�ӱ�׼�����ж�ȡ��������,ֱ������һ��end-of-line �ַ�
!!!���Ե�

2. interact
ȡһ������ΪStirng->String�ĺ���Ϊ����, ����һ��I/O����: ����һ������, �Ѻ�������������, ������

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
λ��:System.IO

4. hGetContents :: Handle -> IO String
Ҳ�Ƕ��Ե�
λ��:System.IO

5. hClose :: Handle -> IO ()
λ��:System.IO

6. withFile :: FilePath -> IOMode -> (Handle -> IO r) -> IO r
����һ���ļ�·����һ��IOMode��һ������(����һ�������Ϊ����, ����һ��I/O����),
Ȼ�󷵻�һ��I/O����: ���ļ�, ���ļ�Ӧ�ú���, �ر��ļ�.
���������,withFile��ȷ���ļ�������ر�.
λ��:System.IO

> myDisplay = do 
>     withFile "target.txt" ReadMode (\handle -> do
>         contents <- hGetContents handle
>         putStrLn contents)

7. bracket :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
λ��:Control.Exception
��һ������Ϊ һ��������Դ��I/O����
�ڶ�������Ϊ һ���ͷ���Դ�ĺ���, bracketȷ����ʹ�����쳣Ҳ�������
����������Ϊ ��Ҫ����,���д

8. hPutStr, hPutStrLn, hGetLine, hGetChar

9. readFile, writeFile, appendFile

10. openTempFile :: FilePath -> String -> IO (FilePath, Handle)

11. removeFile, renameFile
λ��: System.Directory
�����ļ�·����Ϊ����

12. bracketOnError :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
ֻ�е��쳣����ʱ�Ż�ִ���������

13.getArgs :: IO [String]
��ȡ����������е������в����б�

14.getProgName :: IO String
��ȡ������









