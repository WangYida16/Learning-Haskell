import Text.Printf
import Data.List
--主函数
main :: IO ()
main = do
  n <- readLn :: IO Int
  content <- getContents
  let
    points = map (\[x, y] -> (x, y)). map (map (read::String->Int)). map words. lines $ content
    ans = comp $ hull points
  printf "%.1f\n" ans

--接收点集, 初始调用graham, 返回凸包,
hull :: [(Int, Int)] -> [(Int, Int)]
hull xs = (head it) : (fst $ graham (stack,points))
      where it = initSort xs
            stack = reverse $ take 3 it
            points = drop 3 it

--graham扫描法
graham :: ([(Int, Int)], [(Int, Int)]) -> ([(Int, Int)], [(Int, Int)])
graham (stack,[]) = (stack,[])
graham (stack, points@(p:rest)) = graham (stack', points')
          where stack' = if con then p:stack else tail stack --更新 凸包点集堆栈
                points' = if con then rest else points -- 更新 待选点集
                con = judge v1 v2 == 1 || judge v1 v2 == 0 -- 非顺时针偏转
                judge (x1, y1) (x2, y2) = if x1 * y2 - x2 * y1 /= 0 then (x1 * y2 - x2 * y1) `div` abs(x1 * y2 - x2 * y1) else 0 --两个向量的偏转方向
                v1 = (fst p2 - fst p1, snd p2 - snd p1)
                v2 = (fst p - fst p2, snd p - snd p2)
                p1 = stack!!1
                p2 = stack!!0

--初始排序
initSort :: [(Int, Int)] -> [(Int, Int)]
initSort xs = get(findNext(init, xs, []))
        where get (x1,x2,x3) = x3 -- 得到堆栈, 即排好序的点集
              init = foldl (\acc x -> if snd acc < snd x then acc --根据坐标选取P0
                                            else if snd acc > snd x then x
                                                else if fst x > fst acc then acc
                                                    else x) (head xs) (tail xs)

--排序内部函数, 按顺序找到下一个点
findNext :: ((Int, Int), [(Int, Int)], [(Int, Int)]) -> ((Int, Int), [(Int, Int)], [(Int, Int)])
findNext z@(i, [], r) = z
findNext (init, points0, results0) = findNext (init, points1, results1)
                        where points1 = delete it points0 --更新待选点集
                              results1 = results0++[it] --更新堆栈
                              v a = (fst a - fst init, snd a - snd init) --构造向量
                              cross (a, b) (c, d) = a * d - b * c --叉积
                              it = foldl (\acc x -> if cross (v acc) (v x) > 0 then acc --下一个点
                                                    else if cross (v acc) (v x) < 0 then x
                                                      else if dist init acc > dist init x then x
                                                        else acc )  (head points0) (tail points0)

--计算点集周长
comp :: [(Int,Int)] -> Double
comp xs@(y:ys) = if length xs <= 1 then 0 else dist y (head ys) + comp ys

--求两点间距离
dist :: (Int, Int) -> (Int, Int) -> Double
dist x y = sqrt ( (read :: String -> Double) $ show ((fst x - fst y)^2 + (snd x - snd y)^2))
