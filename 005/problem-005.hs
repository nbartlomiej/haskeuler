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

-- A list containing sets of factors for all given numbers.
allFactors :: [Integer] -> [Integer]
allFactors list = foldl (\acc el -> acc ++ (el\\acc) ) [] (map factors list)

result :: Integer
result = product $ allFactors [1..20]

main = putStrLn $ show(result)
