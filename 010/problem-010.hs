import Data.List
import Data.Numbers.Primes

result :: Int
result = sum $ takeWhile (<2000000) primes

main = putStrLn $ show(result)
