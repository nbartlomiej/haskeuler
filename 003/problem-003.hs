import Data.List
import Data.Maybe

primes :: [Integer]
primes = [2] ++ unfoldr ( \primes ->
    let newPrime = fromJust $ find (\candidate ->
            all (\x -> candidate `mod` x /= 0 ) primes
          ) [last primes + 1 ..]
    in  Just(newPrime, primes++[newPrime])
  ) [2]

factors :: Integer -> [Integer]
factors 1 = []
factors x =
  let newFactor = fromJust $ find (\candidate -> x `mod` candidate == 0) primes
  in [newFactor] ++ factors(x `div` newFactor)

result :: Integer
result = last $ factors 600851475143

main = putStrLn $ show(result)
