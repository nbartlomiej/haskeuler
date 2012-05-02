import Char

sumDigits :: Integer -> Int
sumDigits n = sum $ map digitToInt (show n)

result :: Int
result = sumDigits $ product [1..100]

main = putStrLn $ show $ result
