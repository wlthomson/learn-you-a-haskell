-- Syntax.hs

patternMatching = do
  let lucky :: (Integral a) => a -> String
      lucky 7 = "LUCKY NUMBER SEVEN!"
      lucky x = "Sorry, you're out of luck, pal!"

  print (lucky 5)
  print (lucky 7)

  let sayMe :: (Integral a) => a -> String
      sayMe 1 = "One!"
      sayMe 2 = "Two!"
      sayMe 3 = "Three!"
      sayMe 4 = "Four!"
      sayMe 5 = "Five!"
      sayMe x = "Not between 1 and 5"

  print (sayMe 1)
  print (sayMe 2)
  print (sayMe 3)
  print (sayMe 4)
  print (sayMe 5)
  print (sayMe 6)

  let factorial :: (Integral a) => a -> a
      factorial 0 = 1
      factorial n = n * factorial (n-1)

  print (factorial 0)
  print (factorial 1)
  print (factorial 2)
  print (factorial 3)

  let charName :: Char -> String
      charName 'a' = "Albert"
      charName 'b' = "Broseph"
      charName 'c' = "Cecil"
      charName chr = "Unknown"

  print(charName 'a')
  print(charName 'b')
  print(charName 'c')
  print(charName 'd')

  let addVectors :: (Num a) => (a, a) -> (a, a) -> (a,a)
      addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

  let first :: (a, b, c) -> a
      first (x, _, _) = x

  let second :: (a, b, c) -> b
      second (_, y, _) = y

  let third :: (a, b, c) -> c
      third (_, _, z) = z

  let head' :: [a] -> a
      head' [] = error "Can't call head on an empty list, dummy!"
      head' (x:_) = x

  print (head' [4,5,6])
  print (head' "Hello")

  let tell :: (Show a) => [a] -> String
      tell [] = "The list is empty"
      tell (x:[]) = "The list has one element: " ++ (show x)
      tell (x:y:[]) = "The list has two elements: " ++ (show x) ++ " and " ++ (show y)
      tell (x:y:_) = "This list is long. The first two elements are: " ++ (show x) ++ " and " ++ (show y)


  let listEmpty :: (Num a) => [a]
      listEmpty = []
  let listOne = [1]
  let listOneTwo = [1,2]
  let listOneTwoThree = [1,2,3]

  print (tell listEmpty)
  print (tell listOne)
  print (tell listOneTwo)
  print (tell listOneTwoThree)

  let length' :: (Num b) => [a] -> b
      length' [] = 0
      length' (_:xs) = 1 + length' xs

  print(length' "ham")

  let sum' :: (Num a) => [a] -> a
      sum' [] = 0
      sum' (x:xs) = x + sum' xs

  print(sum' [1,2,3,4,5])

  let capital :: String -> String
      capital "" = "Empty string, whoops!"
      capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

  print(capital "Dracula")

guards = do
  let bmiTell :: (RealFloat a) => a -> a -> String
      bmiTell weight height
        | weight / height ^ 2 <= 18.5 = "You're underweight!"
        | weight / height ^ 2 <= 25.0 = "You're normal."
        | weight / height ^ 2 <= 30.0 = "You're fat!"
        | otherwise   = "You're a whale!"

  let bmiSkinny = (70, 2)
  let bmiNormal = (90, 2)
  let bmiFat = (110, 2)
  let bmiWhale = (130, 2)

  print (bmiTell (fst bmiSkinny) (snd bmiSkinny))
  print (bmiTell (fst bmiNormal) (snd bmiNormal))
  print (bmiTell (fst bmiFat) (snd bmiFat))
  print (bmiTell (fst bmiWhale) (snd bmiWhale))

  let max' :: (Ord a) => a -> a -> a
      max' a b
        | a > b     = a
        | otherwise = b

  print (max' 1 2)
  print (max' 2 1)

  let myCompare :: (Ord a) => a -> a -> Ordering
      myCompare a b
        | a > b     = GT
        | a == b    = EQ
        | otherwise = LT

  print (myCompare 1 2)
  print (myCompare 1 1)
  print (myCompare 2 1)

wheres = do
  let bmiTell :: (RealFloat a) => a -> a -> String
      bmiTell weight height
        | bmi <= skinny = "You're underweight!"
        | bmi <= normal = "You're normal."
        | bmi <= fat    = "You're fat!"
        | otherwise     = "You're a whale!"
        where bmi = weight / height ^ 2
              (skinny, normal, fat) = (18.5, 25.0, 30.0)

  let bmiSkinny = (70, 2)
  let bmiNormal = (90, 2)
  let bmiFat = (110, 2)
  let bmiWhale = (130, 2)

  print (bmiTell (fst bmiSkinny) (snd bmiSkinny))
  print (bmiTell (fst bmiNormal) (snd bmiNormal))
  print (bmiTell (fst bmiFat) (snd bmiFat))
  print (bmiTell (fst bmiWhale) (snd bmiWhale))

  let initials :: String -> String -> String
      initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
        where (f:_) = firstname
              (l:_) = lastname

  let calcBmis :: (RealFloat a) => [(a, a)] -> [a]
      calcBmis xs = [bmi w h | (w, h) <- xs]
        where bmi weight height = weight / height ^ 2

  print (calcBmis [bmiSkinny, bmiNormal, bmiFat, bmiWhale])

letItBe = do
  let cylinder :: (RealFloat a) => a -> a -> a
      cylinder r h =
        let sideArea = 2 * pi * r * h
            topArea = pi * r ^ 2
        in sideArea + 2 * topArea

  print (cylinder 3 5)

  let calcBmis :: (RealFloat a) => [(a, a)] -> [a]
      calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]

  let bmiSkinny = (70, 2)
  let bmiNormal = (90, 2)
  let bmiFat = (110, 2)
  let bmiWhale = (130, 2)

  print (calcBmis [bmiSkinny, bmiNormal, bmiFat, bmiWhale])

  print (let mult x y z = x * y * z in mult 2 3 4)

caseExpressions = do
  let head' :: [a] -> a
      head' xs = case xs of [] -> error "No head for empty lists!"
                            (x:_) -> x

  print (head' ["head", "mid", "tail"])

  let describeList :: [a] -> String
      describeList xs = "The list is " ++ case xs of [] -> "empty."
                                                     [x] -> "a singleton list."
                                                     xs -> "a longer list."

  print (describeList [])
  print (describeList [1])
  print (describeList [1,2])

main = do
  patternMatching
  guards
  wheres
  letItBe
  caseExpressions
