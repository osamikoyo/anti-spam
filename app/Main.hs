module Main where

import Data.Char (isAlpha, isSpace)
import Data.List (isPrefixOf)

cleanText :: [Char] -> [Char]
cleanText = filter (\c -> isAlpha c || isSpace c)

filterTextByKeyWord :: [String] -> [String] -> [String]
filterTextByKeyWord _ [] = []
filterTextByKeyWord filterWords (x : xs)
  | x `elem` filterWords = filterTextByKeyWord filterWords xs
  | otherwise = x : filterTextByKeyWord filterWords xs

arrayToString :: [String] -> String
arrayToString [] = ""
arrayToString (x : xs) = x ++ " " ++ arrayToString xs

filterSpamByKeyWord :: String -> [String] -> Bool
filterSpamByKeyWord "" _ = False
filterSpamByKeyWord _ [] = False
filterSpamByKeyWord text word = filtSpam word $ words text
  where
    filtSpam _ [] = False
    filtSpam ws (x : xs)
      | x `elem` ws = True
      | otherwise = filtSpam ws xs

itImgTag :: String -> Bool
itImgTag word
  | firstFourChars == "<img" = True
  | otherwise = False
  where
    firstFourChars = take 5 word

filterSpamByImages :: Integer -> String -> Bool
filterSpamByImages 0 _ = True
filterSpamByImages _ "" = False
filterSpamByImages mx text = filtSpam 0 mx $ words text
  where
    filtSpam :: Integer -> Integer -> [String] -> Bool
    filtSpam _ _ [] = False
    filtSpam c m (x : xs)
      | null xs && c < m = False
      | null xs && c >= m = True
      | itImgTag x = filtSpam (c + 1) m xs
      | otherwise = filtSpam c m xs

isLink :: String -> Bool
isLink str = "http://" `isPrefixOf` str || "https://" `isPrefixOf` str

filterSpamByLinks :: Integer -> String -> Bool
filterSpamByLinks 0 _ = True
filterSpamByLinks mx t = itSpam mx 0 $ words t
  where
    itSpam :: Integer -> Integer -> [String] -> Bool
    itSpam _ count [] = count >= mx
    itSpam m count (x : xs)
      | null xs = False
      | count >= m = True
      | isLink x = itSpam m (count + 1) xs
      | otherwise = itSpam m count xs

main :: IO ()
main = putStrLn "Hello, Haskell!"
