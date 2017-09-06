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

--接受点集,返回凸包,初始调用jarvis函数
hull :: [(Int, Int)] -> [(Int, Int)]
hull xs = cut $ init:get(jarvis (init, xs, []) )
      where get (x1,x2,x3) = x3 --只从返回值中取出堆栈
            cut (x:xs) = [x] ++ (takeWhile (/=x) xs) ++ [x] --jarvis步进法会产生多余的包裹部分,在此将它们除去
            init = foldl (\acc x -> if snd acc < snd x then acc --根据坐标挑选步进初始点
                                      else if snd acc > snd x then x
                                        else if fst x > fst acc then acc
                                          else x) (head xs) (tail xs)

--jarvis步进法求凸包, 接收: 基准点, 余下点集, 凸包点集, 返回: 下一个基准点(即这一步求出的凸包点), 余下点集, 可能重叠的凸包点集
jarvis :: ((Int, Int), [(Int, Int)], [(Int, Int)]) -> ((Int, Int), [(Int, Int)], [(Int, Int)])
jarvis z@(i, [], r) = z
jarvis (init0, points0, results0) = jarvis (init1, points1, results1)
                        where init1 = it
                              points1 = delete it points0 -- 新的待选点集
                              results1 = results0++[it] -- 新的堆栈
                              v a = (fst a - fst init0, snd a - snd init0) -- 待选点 与 当前基准点 构成的向量
                              cross (a, b) (c, d) = a * d - b * c --计算叉积
                              it = foldl (\acc x -> if cross (v acc) (v x) > 0 then acc --步进得到下一个点
                                                    else if cross (v acc) (v x) < 0 then x
                                                      else if dist init0 acc > dist init0 x then acc
                                                        else x )  (head points0) (tail points0)

--求两点间距离
dist :: (Int, Int) -> (Int, Int) -> Double
dist x y = sqrt $ (read :: String -> Double) $ show((fst x - fst y)^2 + (snd x - snd y)^2)
