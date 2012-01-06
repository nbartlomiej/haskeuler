import Data.List

fibs :: [Int]
fibs = unfoldr (\(a,b) -> Just (a, (b,a+b)) ) (0,1)

result :: Int
result = sum $ filter (\x -> odd x) $ takeWhile (\x -> x < 4000000) fibs

main = putStrLn $ show(result)
