-- Pascal triangle function adapted from:
-- http://www.haskell.org/haskellwiki/Blow_your_mind
pascal :: [[Int]]
pascal = (iterate generateRow [1])
  where
    generateRow :: [Int] -> [Int]
    generateRow row = zipWith (+) ([0] ++ row) (row ++ [0])

result :: Int -> Int
result n = pascal !! (n*2) !! n

main = putStrLn $ show $ result 20
