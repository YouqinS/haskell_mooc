polynomial :: Double -> Double
polynomial x = x^2 - x - 1
f1 p = polynomial (p) - 1
f2 y = y + 1


-- if ... then ... else
login user password = if user == "unicorn73"
                      then if password == "f4bulous!"
                           then "unicorn73 logged in"
                           else "wrong password"
                      else "unknown user"

login1 :: String -> String -> String
login1 "unicorn73" "f4bulous!" = "unicorn73 logged in"
login1 "unicorn73" _           = "wrong password"
login1 _           _           = "unknown user"


-- creating local definitions: 
-- let...in and where
circleArea :: Double -> Double
circleArea r = pi * rsquare
    where pi = 3.1415926
          rsquare = r * r

{- circleArea r = let pi = 3.1415926
                   square x = x * x
               in pi * square r -}


x :: Int
x = 5

f :: Int -> Int
f x = 2 * x

g :: Int -> Int
g y = x where x = 6

h :: Int -> Int
h x = x where x = 3

-- Recursion
factorial :: Int -> Int
factorial 1 = 1
factorial n = n * factorial (n-1)

{- factorial 3
  ==> 3 * factorial (3-1)
  ==> 3 * factorial 2
  ==> 3 * 2 * factorial 1
  ==> 3 * 2 * 1
  ==> 6 -}


-- compute the sum 1^2+2^2+3^2+...+n^2
squareSum 0 = 0
squareSum n = n^2 + squareSum (n-1)


-- Fibonacci numbers, slow version
fibonacci 1 = 1
fibonacci 2 = 1
fibonacci n = fibonacci (n-2) + fibonacci (n-1)