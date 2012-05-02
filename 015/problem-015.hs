import Data.List

-- Let's not use the concept of paths and consider fields instead. Fields on a
-- board can be either free or occupied. Assume that path always sticks to the
-- top right borders of free fields. E.g. the examples in ProjectEuler's
-- problem description correspond to the following "field representation":
--   0 0   0 1   0 1   1 1   1 1   1 1
--   0 0 , 0 0 , 0 1 , 0 0 , 0 1 , 1 1
--
-- Let's reduce that to a count of occupied fields per column (starting from
-- top):
--   [0.0], [0,1], [0,2], [1,1] ,[1,2], [2,2]
--
-- The following function will return number of possible field permutations for
-- a board of given length (this number of field permutations is equal to
-- number of paths).
fieldsCount :: Int -> Int
fieldsCount width = length $ fields width

fields :: Int -> [[Int]]
fields width = (unfoldr next' (take width $ repeat 0)) ++ full
  where
    next' :: [Int] -> Maybe( [Int], [Int])
    next' fields = if (last fields == width)
      then Nothing
      else Just (fields, increment fields width)
    full = [take width $ repeat width]

increment :: [Int] -> Int -> [Int]
increment [] base = []
increment (x:xs) base = if x < base
    then (x+1):xs
    else
      let incremented = increment xs base
          duplicateFirst (y:ys) = (y:y:ys)
      in duplicateFirst incremented

main = putStrLn $ show $ fieldsCount 20
