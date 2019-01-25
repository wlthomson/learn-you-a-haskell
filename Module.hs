-- Module.hs

import Data.List

dataList = do
  let numUniques :: (Eq a) => [a] -> Int
      numUniques = length . nub

  print (numUniques [1,2,3])
  print (numUniques [1,2,3,1,2,3])

  print (intersperse '.' "MONKEY")
  print (intersperse 0 [1,2,3,4,5,6])

  print (intercalate " " ["hey","there","guys"])
  print (intercalate [0,0,0] [[1,2,3],[4,5,6],[7,8,9]])

  print (transpose [[1,2,3],[4,5,6],[7,8,9]])
  print (transpose ["hey","there","guys"])

  print (map sum $ transpose [[0,3,5,9],[10,0,0,9],[8,5,1,-1]])

  print (concat ["foo","bar","car"])
  print (concat [[3,4,5],[2,3,4],[2,1,1]])

  print (concatMap (replicate 4) [1..3])

  print (and $ map (>4) [5,6,7,8])
  print (and $ map (==4) [4,4,4,3,4])

  print (or $ map (==4) [2,3,4,5,6,1])
  print (or $ map (>4) [1,2,3])

  print (any (==4) [2,3,5,6,1,4])
  print (all (>4) [6,9,10])

  print (all (`elem` ['A'..'Z']) "HEYGUYSwhatsup")
  print (any (`elem` ['A'..'Z']) "HEYGUYSwhatsup")

  print (take 10 $ iterate (*2) 1)
  print (take 3 $ iterate (++ "haha") "haha")

  print (splitAt 3 "heyman")
  print (splitAt 100 "heyman")
  print (splitAt (-3) "heyman")

  print (let (a,b) = splitAt 3 "foobar" in b ++ a)

  print (takeWhile (>3) [6,5,4,3,2,1,2,3,4,5,4,3,2,1])
  print (takeWhile (/=' ') "This is a sentence")
  print (sum $ takeWhile (<10000) $ map (^3) [1..])

  print (dropWhile (/=' ') "This is a sentence")
  print (dropWhile (<3) [1,2,2,2,3,4,5,4,3,2,1])

  print (let stock = [(99.4,2000,9,1),(955.2,2008,9,2), (999.2,2008,9,3),
                      (1001.4,2008,9,4), (998.3,2008,9,5)]
         in head (dropWhile (\(val,y,m,d) -> val < 1000) stock))

  print (let (fw, rest) = span (/=' ') "This is a sentence"
         in "First word:" ++ fw ++ ", the rest:" ++ rest)

  print (break (==4) [1,2,3,4,5,6,7])
  print (span (/=4) [1,2,3,4,5,6,7])

  print (sort [8,5,3,2,1,6,4,2])
  print (sort "This will be sorted soon")

  print (group [1,1,1,1,2,2,2,2,3,3,2,2,2,5,6,7])
  print (map (\l@(x:xs) -> (x,length l)) . group . sort $ [1,1,1,1,2,2,2,2,3,3,2,2,2,5,6,7])

  print (inits "w00t")
  print (tails "w00t")
  print (let w = "w00t" in zip (inits w) (tails w))

  let search :: (Eq a) => [a] -> [a] -> Bool
      search needle haystack =
        let nlen  = length needle
        in foldl (\acc x -> if take nlen x == needle then True else acc) False (tails haystack)

  print (search "bar" "foobarbaz")
  print (search "buzz" "foobarbaz")

  print ("cat" `isInfixOf` "im a cat burglar")
  print ("Cat" `isInfixOf` "im a cat burglar")
  print ("cats" `isInfixOf` "im a cat buglar")

  print ("hey" `isPrefixOf` "hey there!")
  print ("hey" `isPrefixOf` "oh hey there!")

  print ("there!" `isSuffixOf` "oh hey there!")
  print ("there!" `isSuffixOf` "oh hey there")

  print (partition (`elem` ['A'..'Z']) "BOBsidneyMORGANeddy")
  print (partition (>3) [1,3,5,6,3,2,1,0,3,7])

  print (span (`elem` ['A'..'Z']) ("BOBsidneyMORGANeddy"))

  print (find (>4) [1,2,3,4,5,6])
  print (find (>9) [1,2,3,4,5,6])

  print (4 `elemIndex` [1,2,3,4,5,6])
  print (10 `elemIndex` [1,2,3,4,5,6])

  print (' ' `elemIndices` "Where are the spaces?")

  print (findIndex (==4) [5,3,2,1,6,4])
  print (findIndex (==7) [5,3,2,1,6,4])

  print (findIndices (`elem` ['A'..'Z']) "Where Are The Caps?")

  print (zipWith3 (\x y z -> x + y + z) [1,2,3] [4,5,2,2] [2,2,3])

  print (zip4 [2,3,3] [2,2,2] [5,5,3] [2,2,2])

  print (lines "first line\nsecond line\nthird line")

  print (unlines ["first line", "second line", "third line"])

  print (words "hey these are the words in this sentence")

  print (unwords ["hey", "there", "mate"])

  print (nub [1,2,3,4,3,2,1,2,3,4,3,2,1])
  print (nub "Lots of words and stuff")

  print (delete 'h' "hey there ghang!")
  print (delete 'h' . delete 'h' $ "hey there ghang!")
  print (delete 'h' . delete 'h' . delete 'h' $ "hey there ghang!")

  print ([1..10] \\ [2,5,9])
  print ("Im a big baby" \\ "big")

  print ("hey man" `union` "man what's up")
  print ([1..7] `union`[5..10])

  print ([1..7] `intersect` [5..10])

  print (insert 4 [3,5,1,2,8,2])
  print (insert 4 [1,3,4,4,1])
  print (insert 4 [1,2,3,5,6,7])
  print (insert 'g' $ ['a'..'f'] ++ ['h'..'z'])
  print (insert 3 [1,2,4,3,2,1])

  let values = [-4.3, -2.4, -1.2, 0.4, 2.3, 5.9, 10.5, 29.1, 5.3, -2.4, -14.5, 2.9, 2.3]

  print (groupBy (\x y -> (x > 0) == (y > 0)) values)

  let on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
      f `on` g = \x y -> f (g x) (g y)

  print (groupBy ((==) `on` (> 0)) values)

  let xs = [[5,4,5,4,4],[1,2,3],[3,5,4,3],[],[2],[2,2]]

  print (sortBy (compare `on` length) xs)

main = do
  dataList
