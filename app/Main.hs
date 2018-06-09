module Main where

import           Lib

main :: IO ()
main = do print "hi"
          demo

demo = do bigtext <- readFile "WizardOfOz.txt"
          print $ take 50 bigtext

smallbook = "this is a really small small book"

split :: Eq a => a -> [a] -> [[a]]
split d [] = []
split d s = x : split d (drop 1 y) where (x,y) = span (/= d) s

stringarray = split ' ' smallbook

count :: Eq a => Integral b => a -> [a] -> b
count e [] = 0
count e (a:xs) = (count e xs +) $ if a == e then 1 else 0

helloguys = "hello guys!"

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [y | y <- xs, y <= x] ++ [x] ++ quicksort [y | y <-xs, y > x]

words [] = []

isBlank :: Char -> Bool
isBlank ' ' = True
isBlank _ = False

--split the array into blankspaces, count the elements, how can i check for duplicates