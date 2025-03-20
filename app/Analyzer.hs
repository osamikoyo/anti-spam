module Analyzer where

setToLenght :: String -> Int -> String
setToLenght text l = text ++ p "" l
  where
    p :: String -> Int -> String
    p t ln
      | ln == l = t
      | otherwise = p (t ++ " ") $ ln + 1

getProcentString :: String -> String -> Integer
getProcentString f s
  | length f < lens = r lenf $ p (setToLenght f lens) s 0
  | otherwise = r lenf $ p (setToLenght f lens) s 0
  where
    lens = length s
    lenf = length f
    p :: String -> String -> Integer -> Integer
    p ss [] _ = p s (setToLenght "" (length ss)) $ toInteger lens
    p [] ff _ = p f (setToLenght "" (length ff)) $ toInteger lenf
    p (x : xs) (y : ys) c
      | null xs && null ys = c
      | x == y = p xs ys $ c + 1
      | otherwise = p xs ys c
    r :: (Integral a, Integral b) => a -> b -> Integer
    r m c = toInteger c * 100 `div` toInteger m
