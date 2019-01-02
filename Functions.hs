-- Functions.hs

currying = do
  let multThree :: (Num a) => a -> a -> a -> a
      multThree x y z = x * y * z

  let multTwoWithNine :: (Num a) => a -> a -> a
      multTwoWithNine = multThree 9

  let multWithEighteen :: (Num a) => a -> a
      multWithEighteen = multTwoWithNine 2

  print (multWithEighteen 10)

  let compareWithHundred :: (Num a, Ord a) => a -> Ordering
      compareWithHundred x = compare 100 x

  print (compareWithHundred 99)
  print (compareWithHundred 100)
  print (compareWithHundred 101)

  let divideByTen :: (Floating a) => a -> a
      divideByTen = (/10)

  print (divideByTen 100)

  let isUpperAlphanum :: Char -> Bool
      isUpperAlphanum = (`elem` ['A'..'Z'])

  print (isUpperAlphanum 'A')
  print (isUpperAlphanum 'a')

higherOrderism = do
  let applyTwice :: (a -> a) -> a -> a
      applyTwice f x = f (f x)

  print (applyTwice (+1) 1)
  print (applyTwice (*2) 1)
  print (applyTwice (1:) [1])
  print (applyTwice ("HEY " ++) "HEY")

  let zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
      zipWith' _ [] _ = []
      zipWith' _ _ [] = []
      zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

  print (zipWith' (+) [4,2,5,6] [2,6,2,3])
  print (zipWith' max [6,3,2,1] [7,3,1,5])
  print (zipWith' (++) ["foo ", "bar ", "baz "] ["fighters", "hoppers", "aldrin"])
  print (zipWith' (*) (replicate 5 2) [1..])
  print (zipWith' (zipWith' (*)) [[1,2,3],[3,5,6],[2,3,4]] [[3,2,2],[3,4,5],[5,4,3]])

  let flip' :: (a -> b -> c) -> (b -> a -> c)
      flip' f = g
        where g x y = f y x

  let flip'' :: (a -> b -> c) -> (b -> a -> c)
      flip'' f x y = f y x

  print (flip' zip [1,2,3,4,5] "hello")
  print (zipWith' (flip' div) [2,2..] [10,8,6,4,2])

  let map' :: (a -> b) -> [a] -> [b]
      map' _ [] = []
      map' f (x:xs) = (f x) : map' f xs

  print (map' (+3) [1,5,3,16])
  print (map' (++ "!") ["BIFF", "BANG", "POW"])
  print (map' (replicate 3) [3..6])
  print (map' fst [(1,2),(3,5),(6,3),(2,6),(2,5)])

  let filter' :: (a -> Bool) -> [a] -> [a]
      filter' _ [] = []
      filter' p (x:xs)
        | p x = x : filter' p xs
        | otherwise = filter' p xs

  print (filter' (>3) [1,5,3,2,1,6,4,3,2,1])
  print (filter' (==3) [1,2,3,4,5])
  print (filter' even [1..10])
  print (let notNull x = not (null x) in filter' notNull [[1,2,3],[],[3,4,5],[2,2],[],[],[]])
  print (filter' (`elem` ['a'..'z']) "AaBbCc")
  print (filter' (`elem` ['A'..'Z']) "AaBbCc")

  let quicksort :: (Ord a) => [a] -> [a]
      quicksort [] = []
      quicksort (x:xs) =
        let smallerSorted = quicksort (filter (<=x) xs)
            biggerSorted = quicksort (filter (>x) xs)
        in smallerSorted ++ [x] ++ biggerSorted

  print (quicksort [1,7,2,8,3,9,6,4,5])

  let largestDivisible :: (Integral a) => a
      largestDivisible = head (filter p [100000,99999..])
        where p x = x `mod` 2839 == 0

  print (largestDivisible)

  print (sum (takeWhile (<10000) (filter odd (map (^2) [1..]))))
  print (sum (takeWhile (<10000) [n^2 | n <- [1..], odd (n^2)]))

  let listOfFuns = map (*) [0..]

  print ((listOfFuns !! 4) 5)

lambdas = do
  let chain :: (Integral a) => a -> [a]
      chain 1 = [1]
      chain n
        | even n = n : chain (n `div` 2)
        | odd n = n : chain (n*3 + 1)

  print (chain 1)
  print (chain 10)
  print (chain 30)

  let numLongChains :: Int
      numLongChains = length (filter isLong (map chain [1..100]))
        where isLong xs = length xs > 15

  print (numLongChains)

  print (length (filter (\xs -> length xs > 15) (map chain [1..100])))

  print (zipWith (\a b -> (a * 30 + 3) / b) [5,4,3,2,1] [1,2,3,4,5])

  print (map (\(a,b) -> a + b) [(1,2),(3,5),(6,3),(2,6),(2,5)])

  print ((\x -> \y -> \z -> x + y + z) 1 2 3)

  let flip' :: (a -> b -> c) -> b -> a -> c
      flip' f = \x y -> f y x

  print (flip' zip [1,2,3,4,5] "hello")

folds = do
  let sum' :: (Num a) => [a] -> a
      sum' xs = foldl (\acc x -> acc + x) 0 xs

  print (sum' [3,5,2,1])

  let sum'' :: (Num a) => [a] -> a
      sum'' = foldl (+) 0

  print (sum'' [3,5,2,1])

  let elem' :: (Eq a) => a -> [a] -> Bool
      elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys

  print (1 `elem` [1,2])
  print (2 `elem` [1,2])
  print (3 `elem` [1,2])

  let map' :: (a -> b) -> [a] -> [b]
      map' f xs = foldr (\x acc -> f x : acc) [] xs

  print (map' (+1) [1,2,3])

  let maximum' :: (Ord a) => [a] -> a
      maximum' = foldr1 (\x acc -> if x > acc then x else acc)

  print (maximum' [1,2,3])

  let reverse' :: [a] -> [a]
      reverse' = foldl (\acc x -> x : acc) []

  print (reverse' [1,2,3])

  let product' :: (Num a) => [a] -> a
      product' = foldr1 (*)

  print (product' [1,2,3])

  let filter' :: (a -> Bool) -> [a] -> [a]
      filter' p = foldr (\x acc -> if p x then x : acc else acc) []

  print (filter' (>1) [1,2,3])

  let head' :: [a] -> a
      head' = foldr1 (\x _ -> x)

  print (head' [1,2,3])

  let last' :: [a] -> a
      last' = foldl1 (\_ x -> x)

  print (last' [1,2,3])

  print (scanl (+) 0 [3,5,2,1])
  print (scanr (+) 0 [3,5,2,1])
  print (scanl1 (\acc x -> if x > acc then x else acc) [3,4,5,3,7,9.2,1])
  print (scanl (flip (:)) [] [3,2,1])

  let sqrtSums :: Int
      sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1

  print (sqrtSums)
  print (sum (map sqrt [1..131]))
  print (sum (map sqrt [1..130]))

applyCompose = do
  print (sqrt 1+3+5+7)
  print (sqrt (1+3+5+7))
  print (sqrt $ 1+3+5+7)

  print (map (\x -> negate (abs x)) [5,-3,-6,7,-3,2,-19,24])
  print (map (negate . abs) [5,-3,-6,7,-3,2,-19,24])
  print (map (\xs -> negate (sum (tail xs))) [[1..5],[3..6],[1..7]])
  print (map (negate . sum . tail) [[1..5],[3..6],[1..7]])

  let pointfulFn :: (RealFrac a, Floating a, Integral b) => a -> b
      pointfulFn x = ceiling (negate (tan (cos (max 50 x))))

  let pointlessFn :: (RealFrac a, Floating a, Integral b) => (a -> b)
      pointlessFn = ceiling . negate . tan . cos . max 50

  print (pointfulFn 30)
  print (pointlessFn 30)

  let oddSquareSum :: Integer
      oddSquareSum = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))

  let oddSquareSum' :: Integer
      oddSquareSum' = sum . takeWhile (<10000) . filter odd . map (^2) $ [1..]

  let oddSquareSum'' :: Integer
      oddSquareSum'' =
        let oddSquares = filter odd $ map (^2) [1..]
            belowLimit = takeWhile (<10000) oddSquares
        in sum belowLimit

  print (oddSquareSum)
  print (oddSquareSum')
  print (oddSquareSum'')

main = do
  currying
  higherOrderism
  lambdas
  folds
  applyCompose
