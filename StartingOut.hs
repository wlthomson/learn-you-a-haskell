-- StartingOut.hs

readySetGo = do
  print (2 + 15)
  print (49 * 100)
  print (1892 - 1472)
  print (5/2)

  print ((50 * 100) - 4999)
  print (50 * 100 - 4999)
  print (50 * (100 - 4999))

  print (True && False)
  print (True && True)
  print (False || True)
  print (not False)
  print (not (True && True))

  print (5 == 5)
  print (1 == 0)
  print (5 /= 5)
  print (5 /= 4)
  print ("hello" == "hello")

  print (succ 8)
  print (min 9 10)
  print (min 3.4 3.2)
  print (max 100 101)
  print (succ 9 + max 5 4 + 1)
  print ((succ 9) + (max 5 4) + 1)

babysFirstFunctions = do
  let doubleMe x = x + x
  print (doubleMe 9)
  print (doubleMe 8.3)

  let doubleUs x y = doubleMe x + doubleMe y
  print (doubleUs 4 9)
  print (doubleUs 2.3 34.2)
  print (doubleUs 28 88 + doubleMe 123)

  let doubleSmallNumber x = if x > 100 then x else x*2
  let doubleSmallNumber' x = (if x > 100 then x else x*2) + 1
  print (doubleSmallNumber 100)
  print (doubleSmallNumber 101)
  print (doubleSmallNumber' 100)
  print (doubleSmallNumber' 101)

  let conanO'Brien = "It's a-me, Conan O'Brien!"
  print conanO'Brien

introToLists = do
  let lostNumbers = [4,8,15,16,23,42]
  print lostNumbers
  print ([1,2,3,4] ++ [9,10,11,12])

  print ("hello" ++ "world")
  print (['w','o'] ++ ['o','t'])

  print ('A' : " SMALL CAT")
  print (5 : [1,2,3,4,5])

  print ("Steve Buscemi" !! 6)
  print ([9.4,33.2,96.2,11.2,23.25] !! 1)

  let b = [[1,2,3,4],[5,3,3,3],[1,2,2,3,4],[1,2,3]]
  print b
  print (b ++ [[1,1,1,1]])
  print ([6,6,6]:b)
  print (b !! 2)

  print ([3,2,1] > [2,1,0])
  print ([3,2,1] > [2,1,0])
  print ([3,2,1] > [2,10,100])
  print ([3,4,2] > [3,4])
  print ([3,4,2] > [2,4])
  print ([3,4,2] == [3,4,2])

  print (head [5,4,3,2,1])
  print (tail [5,4,3,2,1])
  print (last [5,4.3,2,1])
  print (init [5,4,3,2,1])

  print (length [5,4,3,2,1])
  print (null [1,2,3])
  print (null [])
  print (reverse [5,4,3,2,1])

  print (take 3 [5,4,3,2,1])
  print (take 1 [3,9,3])
  print (take 5 [1,2])
  print (take 0 [6,6,6])
  print (drop 3 [8,4,2,1,5,6])
  print (drop 0 [1,2,3,4])
  print (drop 100 [1,2,3,4])

  print (minimum [8,4,2,1,5,6])
  print (maximum [1,9,2,3,4])
  print (sum [5,2,1,6,3,2,5,7])
  print (product [6,2,1,2])
  print (product [1,2,5,6,7,9,2,6])

  print (4 `elem` [3,4,5,6])
  print (10 `elem` [3,4,5,6])

texasRanges = do
  print ([1..20])
  print (['a'..'z'])
  print (['K'..'Z'])
  print ([2,4..20])
  print ([3,6..20])
  print ([0.1,0.3..1])

  print (take 10 (cycle [1,2,3]))
  print (take 12 (cycle "LOL "))
  print (take 10 (repeat 5))
  print (replicate 3 10)

imAListComprehension = do
  print ([x*2 | x <- [1..10]])
  print ([x*2 | x <- [1..10], x*2 >= 12])
  print ([x | x <- [50..100], x `mod` 7 == 3])

  let boomBangs xs = [if 10 < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]
  print (boomBangs [7..13])

  print ([x | x <- [10..20], x /= 13, x /= 15, x /= 19])
  print ([x*y | x <- [2,5,10], y <- [8,10,11]])
  print ([x*y | x <- [2,5,10], y <- [8,10,11], x*y > 50])

  let nouns = ["hobos","frog","pope"]
  let adjectives = ["lazy","grouchy","scheming"]
  print (take 5 [adjective ++ " " ++ noun | adjective <- adjectives, noun <- nouns])

  let length' xs = sum [1 | _ <- xs]
  print (length' [1,2,3])

  let removeNonUppercase st = [c | c <- st, c `elem` ['A'..'Z']]
  print (removeNonUppercase "Hahaha! Ahahaha!")
  print (removeNonUppercase "IdontLIKEFROGS")

  let xxs = [[1,3,5,2,3,1,2,4,5],[1,2,3,4,5,6,7,8,9],[1,2,4,2,1,6,3,1,3,2,3,6]]
  print ([[x | x <- xs, even x] | xs <- xxs])

tuples = do
  print (fst (8, 11))
  print (snd (8, 11))

  print (zip [1,2,3,4,5] [5,5,5,5,5])
  print (zip [1..5] ["one","two","three","four","five"])
  print (zip [5,3,2,6,2,7,2,5,4,6,6] ["im","a","turtle"])
  print (zip [1..] ["apple","orange","cherry","mango"])

  let triangles = [(a,b,c) | c <- [1..10], b <- [1..10], a <- [1..10]]
  let rightTriangles = [(a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2+b^2==c^2]
  let rightTriangles' = [(a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2+b^2==c^2, a+b+c==24]
  print (take 10 triangles)
  print rightTriangles
  print rightTriangles'

main = do
  readySetGo
  babysFirstFunctions
  introToLists
  texasRanges
  imAListComprehension
  tuples
