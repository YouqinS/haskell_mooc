module Gold where

-- The golden ratio
phi :: Double
phi = (sqrt 5 + 1) / 2 -- -> 1.618

polynomial :: Double -> Double
polynomial x = x^2 - x - 1 

f x = polynomial (polynomial x) 

main = do
  print (polynomial phi) -- -> => -0.0
  print (f phi) -- -> -1


