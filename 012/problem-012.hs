import Data.List
import Data.Maybe
import Data.Numbers.Primes

triangles :: [Int]
triangles = unfoldr (\(x,sum) -> Just (sum, (x+1,sum+x+1))) (1,1)

divisors :: [(Int, Int)]
divisors = map divisors' triangles
  where divisors' x = (x, product $ map ((+1).length) $ group $ primeFactors x)

result :: Int
result = fst.fromJust $ find ((>500).snd) divisors

main = putStrLn $ show(result)
