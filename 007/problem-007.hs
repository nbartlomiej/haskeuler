import Data.List
import Data.Numbers.Primes

result :: Int
result = primes !! 10000 -- Not 10001 as we start counting from zero.

main = putStrLn $ show(result)
