module Input (stringToInts) where

stringToInts :: String -> [Integer]
stringToInts input = map read (lines input)
