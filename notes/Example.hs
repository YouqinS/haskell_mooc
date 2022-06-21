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


-- factorial implemented with guards instead of pattern matching
factorial' n
  | n<0       = -1
  | n==0      = 1
  | otherwise = n * factorial' (n-1)


-- compute the sum 1^2+2^2+3^2+...+n^2
squareSum 0 = 0
squareSum n = n^2 + squareSum (n-1)


-- combine guards with pattern matching
guessAge :: String -> Int -> String
guessAge "Griselda" age
    | age < 47 = "Too low!"
    | age > 47 = "Too high!"
    | otherwise = "Correct!"
guessAge "Hansel" age
    | age < 12 = "Too low!"
    | age > 12 = "Too high!"
    | otherwise = "Correct!"
guessAge name age = "Wrong name!"



-- Fibonacci numbers, slow version
{- fibonacci 1 = 1
fibonacci 2 = 1
fibonacci n = fibonacci (n-2) + fibonacci (n-1) -}


-- fibonacci numbers, fast version
fibonacci :: Integer -> Integer
fibonacci n = fibonacci' 0 1 n

fibonacci' :: Integer -> Integer -> Integer -> Integer
fibonacci' a b 1 = b
fibonacci' a b n = fibonacci' b (a+b) (n-1)


repeatString n str = repeatHelper n str ""

-- repeatHelper n str result = if (n==0)
--                             then result
--                             else repeatHelper (n-1) str (result++str)

repeatHelper 0 _   result = result
repeatHelper n str result = repeatHelper (n-1) str (result++str)


-- Lists
{- 
  [True,True,False] :: [Bool]
  ["Moi","Hei"] :: [String]
  [] :: [a]                   -- more about this later
  [[1,2],[3,4]] :: [[Int]]    -- a list of lists
  [1..7] :: [Int]             -- range syntax, value [1,2,3,4,5,6,7] 
-}

{-
  head :: [a] -> a            -- returns the first element
  tail :: [a] -> [a]          -- returns everything except the first element
  init :: [a] -> [a]          -- returns everything except the last element
  take :: Int -> [a] -> [a]   -- returns the n first elements
  drop :: Int -> [a] -> [a]   -- returns everything except the n first elements
  (++) :: [a] -> [a] -> [a]   -- lists are catenated with the ++ operator
  (!!) :: [a] -> Int -> a     -- lists are indexed with the !! operator (get element at index of)
  reverse :: [a] -> [a]       -- reverse a list
  null :: [a] -> Bool         -- is this list empty?
  length :: [a] -> Int        -- the length of a list
-}


-- ghci> [7,10,4,5] !! 2
-- 4

-- ghci> f xs = take 2 xs ++ drop 4 xs  (take the first 2 and everything after the 4th)
-- ghci> f [1,2,3,4,5,6]
-- [1,2,5,6]


-- ghci> g xs = tail xs ++ [head xs]
-- ghci> g [1,2,3] 
-- [2,3,1]

-- ghci> reverse [1..4]
-- [4,3,2,1]

-- given a password, return (Just username) if login succeeds, Nothing otherwise
login2 :: String -> Maybe String
login2 "f4bulous!" = Just "unicorn73"
login2 "swordfish" = Just "megahacker"
login2 _           = Nothing

-- Multiply an Int with a Maybe Int. Nothing is treated as no multiplication at all.
perhapsMultiply :: Int -> Maybe Int -> Int
perhapsMultiply i Nothing = i
perhapsMultiply i (Just j) = i*j   -- Note how j denotes the value inside the Just

{- 
ghci> perhapsMultiply 1 Nothing 
1

ghci> perhapsMultiply 3 (Just 2)
6 

-}


intOrZero :: Maybe Int -> Int
intOrZero Nothing = 0
intOrZero (Just i) = i


-- ghci> intOrZero Nothing 
-- 0
-- ghci> intOrZero (Just 0)
-- 0
-- ghci> intOrZero (Just 1)
-- 1


safeHead :: [a] -> Maybe a
safeHead xs = if null xs then Nothing else Just (head xs)
-- ghci> safeHead []
-- Nothing
-- ghci> safeHead [1]
-- Just 1


headOrZero :: [Int] -> Int
headOrZero xs = intOrZero (safeHead xs)


readInt :: String -> Either String Int
readInt "0" = Right 0
readInt "1" = Right 1
readInt s = Left ("Unsupported string: " ++ s)

-- ghci> readInt "10"
-- Left "Unsupported string: 10"
-- ghci> readInt "1"
-- Right 1
-- ghci> readInt "0"
-- Right 0



iWantAString :: Either Int String -> String
iWantAString (Right str)   = str
iWantAString (Left number) = show number

-- ghci> iWantAString (Right "hi")
-- "hi"
-- ghci> iWantAString (Left 1)
-- "1"

lectureParticipants :: [Either String Int]
lectureParticipants = [Right 10, Right 13, Left "easter vacation", Right 17, Left "lecturer was sick", Right 3]

-- ghci> lectureParticipants
-- [Right 10,Right 13,Left "easter vacation",Right 17,Left "lecturer was sick",Right 3]


describe :: Integer -> String
describe n = case n of 0 -> "zero"
                       1 -> "one"
                       2 -> "an even prime"
                       n -> "the number " ++ show n



-- parse country code into country name, returns Nothing if code not recognized
parseCountry :: String -> Maybe String
parseCountry "FI" = Just "Finland"
parseCountry "SE" = Just "Sweden"
parseCountry _ = Nothing

flyTo :: String -> String
flyTo countryCode = case parseCountry countryCode of Just country -> "You're flying to " ++ country
                                                     Nothing -> "You're not flying anywhere"
-- ghci> flyTo "FI"
-- "You're flying to Finland"
-- ghci> flyTo "aa"
-- "You're not flying anywhere"


-- helper function for pattern matching instead of using the case-of expression:
flyTo1 :: String -> String
flyTo1 countryCode = handleResult (parseCountry countryCode)
  where handleResult (Just country) = "You're flying to " ++ country
        handleResult Nothing        = "You're not flying anywhere"


-- given a sentence, decide whether it is a statement, question or exclamation
sentenceType :: String -> String
sentenceType str = case last str of '.' -> "statement"
                                    '?' -> "question"
                                    '!' -> "exclamation"
                                    _   -> "not a sentence"

-- same function, helper function instead of case-of
sentenceType1 str = checkLastElement (last str)
  where checkLastElement "." = "statement"
        checkLastElement "?" = "question"
        checkLastElement "!" = "exclamation"
        checkLastElement _ = "not a sentence"


motivate :: String -> String
motivate "Monday"    = "Have a nice week at work!"
motivate "Tuesday"   = "You're one day closer to weekend!"
motivate "Wednesday" = "3 more day(s) until the weekend!"
motivate "Thursday"  = "2 more day(s) until the weekend!"
motivate "Friday"    = "1 more day(s) until the weekend!"
motivate _           = "Relax! You don't need to work today!"


motivate1 :: String -> String
motivate1 day = case distanceToSunday day of
  6 -> "Have a nice week at work!"
  5 -> "You're one day closer to weekend!"
  n -> if n > 1
       then show (n - 1) ++ " more day(s) until the weekend!"
       else "Relax! You don't need to work today!"


motivate2 :: String -> String
motivate2 day
  | n == 6 = "Have a nice week at work!"
  | n == 5 = "You're one day closer to weekend!"
  | n > 1 = show (n - 1) ++ " more day(s) until the weekend!"
  | otherwise = "Relax! You don't need to work today!"
  where n = distanceToSunday day - 1

distanceToSunday :: String -> Int
distanceToSunday d = case d of
  "Monday"    -> 6
  "Tuesday"   -> 5
  "Wednesday" -> 4
  "Thursday"  -> 3
  "Friday"    -> 2
  "Saturday"  -> 1
  "Sunday"    -> 0


area :: String -> Double -> Double
area shape x = case shape of
  "square" -> square x
  "circle" -> pi * square x
  where square x = x*x

-- getElement (Just i) gets the ith element (counting from zero) of a list, getElement Nothing gets the last element
getElement :: Maybe Int -> [a] -> a
getElement (Just i) xs = xs !! i
getElement Nothing xs = last xs

-- ghci> getElement Nothing [1,2]
-- 2
-- ghci> getElement (Just 1) [1,2]
-- 2
-- ghci> 

direction :: Either Int String -> String
direction (Left i) = "you should go left " ++ show i ++ " meters!"
direction (Right i) = "you should go right " ++ show i ++ " meters!"

-- ghci> direction (Left 1)
-- "you should go left 1 meters!"
-- ghci> direction (Right "1")
-- "you should go right \"1\" meters!"


