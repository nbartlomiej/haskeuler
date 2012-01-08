import Data.List

palindrome :: Integer -> Bool
palindrome num = all (==True) (zipWith (==) (show num) (reverse.show $ num))

palindromes :: [Integer]
palindromes = [x*y | x<-[999,998..100],y<-[999,998..100], x>y, palindrome(x*y)]

result :: Integer
result = head $ reverse $ sort $ palindromes

main = putStrLn $ show(result)
