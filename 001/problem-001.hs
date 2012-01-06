result :: Int
result = sum [ x | x <- [0..1000], (x `mod` 3 == 0) || (x `mod` 5 == 0)]

main = putStrLn $ show(result)
