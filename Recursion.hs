-- Recursion.hs

helloRecursion = do
  let maximum' :: (Ord a) => [a] -> a
      maximum' [] = error "No maximum for empty lists."
      maximum' [x] = x
      maximum' (x:xs)
        | x > maxTail = x
        | otherwise = maxTail
        where maxTail = maximum' xs

  print (maximum' [1,5,2,3,4])

  let maximum'' :: (Ord a) => [a] -> a
      maximum'' [] = error "No maximum for empty lists."
      maximum'' [x] = x
      maximum'' (x:xs) = max x (maximum'' xs)

  print (maximum'' [1,5,2,3,4])

  let replicate' :: (Num i, Ord i) => i -> a -> [a]
      replicate' n x
        | n <= 0 = []
        | otherwise = x:replicate' (n-1) x

  print (replicate' 5 1)

  let take' :: (Num i, Ord i) => i -> [a] -> [a]
      take' n _
        | n <= 0 = []
      take' _ [] = []
      take' n (x:xs) = x : take' (n-1) xs

  print (take' 5 [1,2,3,4,5,6,7,8,9])

  let reverse' :: [a] -> [a]
      reverse' [] = []
      reverse' (x:xs) = reverse' xs ++ [x]

  print (reverse' [5,4,3,2,1])

  let repeat' :: a -> [a]
      repeat' x = x : repeat' x

  print (take' 5 (repeat 1))

  let zip' :: [a] -> [b] -> [(a,b)]
      zip' _ [] = []
      zip' [] _ = []
      zip' (x:xs) (y:ys) = (x,y) : (zip' xs ys)

  print (zip' ['A','B'] ['a','b'])

  let elem' :: (Eq a) => a -> [a] -> Bool
      elem' a [] = False
      elem' a (x:xs)
        | a == x = True
        | otherwise = elem' a xs

  print (elem' 1 [1,2])
  print (elem' 2 [1,2])
  print (elem' 3 [1,2])

quickSort = do
  let quicksort :: (Ord a) => [a] -> [a]
      quicksort [] = []
      quicksort (x:xs) =
        let smallerSorted = quicksort [a | a <- xs, a <= x]
            biggerSorted = quicksort [a | a <- xs, a > x]
        in smallerSorted ++ [x] ++ biggerSorted

  print (quicksort [1,9,2,8,3,7,4,6,5])

main = do
  helloRecursion
  quickSort
