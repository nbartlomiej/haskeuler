import Data.List

next :: Int -> Int
next 1 = 1
next a
  | even a = a `div` 2
  | otherwise = 3*a + 1

unique :: [Int] -> Int
unique list =
  let startData :: ([(Int, Int)],[Int])
      startData = ([(x, x) | x <- list], [])
      unique' :: ([(Int, Int)],[Int]) -> Int
      unique' (pairs,cache) =
        let (pairs', x) = next' (pairs,cache)
        in if (all (==1) $ map snd pairs')
            then head $ filter (/= 1) $ map fst pairs
            else unique' $ remove $ next' (pairs,cache)
  in unique' startData

next' :: ([(Int, Int)],[Int]) -> ([(Int, Int)],[Int])
next' (pairs, cache) = (map (\(a,b) -> (a, next b) ) pairs, cache)


remove :: ([(Int, Int)],[Int]) -> ([(Int, Int)],[Int])
remove (pairs, cache) =
  let pairs' = filter ((/=1).snd) pairs
      cache' = nub $ sort $ cache ++ (map snd $ filter ((==1).snd) pairs)
  in  (filter' pairs' cache', cache')

filter' :: [(Int, Int)] -> [Int] -> [(Int, Int)]
filter' ((a,b):pairs) (c:filtered)
  | b == 1 = filter' pairs (c:filtered)
  | a == c = filter' pairs (c:filtered)
  | a < c = (a,b):(filter' pairs (c:filtered))
  | a > c = filter' ((a,b):pairs) (filtered)
filter' [] _ = []
filter' pairs [] = pairs

main = putStrLn $ show(unique [1..1000000])
