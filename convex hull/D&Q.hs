import Text.Printf
import Data.List
--接收点集,返回凸包周长
solve :: [(Int, Int)] -> Double
solve points = comp $ hull points
        where comp xs@(y:ys) = if length xs <= 1 then 0 else dist y (head ys) + comp ys -- 计算周长
--主函数
main :: IO ()
main = do
  n <- readLn :: IO Int
  content <- getContents
  let
    points = map (\[x, y] -> (x, y)). map (map (read::String->Int)). map words. lines $ content
    ans = solve points
  printf "%.1f\n" ans
--接收点集,求凸包
hull :: [(Int, Int)] -> [(Int, Int)]
hull xs = [m] ++ dQ (up, m, n) ++ [n] ++ dQ (down, n, m) ++ [m] -- 初步分治 , 首尾相连
           where (up, down) = divide (delete m $ delete n xs) m m n --初步划分为上下两个区域
                 m = foldl (\acc x -> if snd acc < snd x then acc --左下点
                                        else if snd acc > snd x then x
                                          else if fst acc > fst x then x else acc) (head xs) (tail xs)
                 n = foldl (\acc x -> if snd acc < snd x then x --右上点
                                        else if snd acc > snd x then acc
                                          else if fst acc > fst x then acc else x) (head xs) (tail xs)
--分治法
dQ :: ([(Int, Int)], (Int, Int), (Int, Int)) -> [(Int, Int)]
dQ ([], _, _) = []
dQ (points, m@(xm, ym), n@(xn, yn)) = dQ(points1, m, p) ++ [p] ++ dQ(points2, p, n) --分治
          where (points1, points2) = divide (delete p points) m n p --分割
                p = foldl (\acc x -> if triangle m n acc > triangle m n x then acc else x) (head points) (tail points) --面积最大点
--分割出两个新的待处理区域
divide :: [(Int, Int)] -> (Int, Int) -> (Int, Int) -> (Int, Int) -> ([(Int, Int)], [(Int, Int)])
divide [] _ _ _ = ([], [])
divide points@(x:xs) m n p = if isLeft m x p then (x:left, right) else if isRight x n p then (left, x:right) else (left, right) --递归分类存放
              where isLeft m x p = cross (v m p) (v m x) > 0 --待处理区域1
                    isRight x n p = cross (v n p) (v n x) < 0  --待处理区域2
                    v x y = (fst y - fst x, snd y - snd x) --向量构造
                    cross (x1, y1) (x2, y2) = x1 * y2 - x2 * y1 --叉积
                    (left, right) = divide xs m n p
--求三角形面积
triangle :: (Int, Int) -> (Int, Int) -> (Int, Int) -> Double
triangle (x1, y1) (x2, y2) (x3, y3) = fromIntegral(x2*y3-x3*y2-x1*y3+x3*y1+x1*y2-x2*y1)/2
--求两点间距离
dist :: (Int, Int) -> (Int, Int) -> Double
dist x y = sqrt $ (read :: String -> Double) $ show((fst x - fst y)^2 + (snd x - snd y)^2)
