partition :: [a] -> Int -> [[a]] -> [[a]]
partition [] _ ys = ys
partition xs n ys = partition (drop n xs) n (ys ++ [take n xs])

reverseNested :: [[a]] -> [[a]]
reverseNested xs = map reverse xs

flatten :: [[a]] -> [a] -> [a]
flatten [] ys = ys
flatten (x:xs) ys = flatten xs (ys ++ x)

reverseNestedPartition :: [a] -> Int -> [a]
reverseNestedPartition xs n = (flatten (reverseNested (partition xs n [[]])) [])
