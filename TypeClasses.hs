-- TypeClasses.hs

data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

shapes :: IO ()
shapes = do
  let surface :: Shape -> Float
      surface (Circle _ r) = pi * r ^ 2
      surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

  print (surface (Rectangle (Point 0 0) (Point 100 100)))
  print (surface (Circle (Point 0 0) 24))

  let nudge :: Shape -> Float -> Float -> Shape
      nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r
      nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))

  print (nudge (Circle (Point 34 34) 10) 5 10)

  let baseCircle :: Float -> Shape
      baseCircle r = Circle (Point 0 0) r

  let baseRect :: Float -> Float -> Shape
      baseRect w h = Rectangle (Point 0 0) (Point w h)

  print (nudge (baseCircle 10) 10 10)
  print (nudge (baseRect 40 100) 60 23)

data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     , height :: Float
                     , phoneNumber :: String
                     , flavour :: String
                     } deriving (Show)

data Car = Car { company :: String
               , model :: String
               , year :: Int
               } deriving (Show)

records :: IO()
records = do
  let somePerson = Person "Buddy" "Finklestein" 43 184.2 "526-2928" "Chocolate"

  print (firstName somePerson)
  print (lastName somePerson)
  print (age somePerson)
  print (height somePerson)
  print (phoneNumber somePerson)
  print (flavour somePerson)

  let someCar = Car { company="Ford", model="Mustang", year=1967 }

  print (company someCar)
  print (model someCar)
  print (year someCar)

main :: IO ()
main = do
  shapes
  records
