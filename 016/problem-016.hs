import Char

sumDigits :: Integer -> Int
sumDigits n = sum $ map digitToInt (show n)

result :: Int
result = sumDigits (2^1000)

main = putStrLn $ show $ result
