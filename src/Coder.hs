module Coder where

import Data.List(group)
import qualified Data.ByteString.Lazy as BS

-- неповоторяющиеся
nonRecurring :: (Eq a1, Num a1) => [(a1, [a2])] -> [(a1, [a2])]
nonRecurring xs = nonRecurring' xs 0 [] where
    nonRecurring' [] i str | i == 0 = []
                           | otherwise = [(-i, str)]
    nonRecurring' ((a,b):xs) i str  | a == 1 = nonRecurring' xs (i + 1) (str ++ b)
                                    | a /= 1 && i == 0 = (a,b): nonRecurring' xs 0 []
                                    | otherwise = (-i, str): (a,b): nonRecurring' xs 0 []


-- защита от переполнения - нарезка на < 128               
cut :: (Ord t, Num t) => [a] -> [(t, [a])]
cut xs = cut' xs 0 []
cut' [] c str | c > 0 = [(-c, str)]
              | otherwise = []
cut' (x:xs) c str   | c == 128 = (-c, str) : cut' (x:xs) 0 []
                    | otherwise = cut' xs (c + 1) (str ++ [x])

bigNum :: (Ord a1, Num a1) => [(a1, [a2])] -> [(a1, [a2])]
bigNum [] = []
bigNum ((a,b):xs) | a > 127 = (127, b): bigNum ((a-127, b):xs)
                  | a < -128 = cut b ++ bigNum xs
                  | otherwise = (a, b):bigNum xs

encodeSeries :: [[a]] -> [(Int, [a])]
encodeSeries = map (\ x -> (length x, [head x]))

pairToStrings :: (Integral a1, Num a2) => [(a1, [a2])] -> [[a2]]
pairToStrings [] = []
pairToStrings ((a,b):xs) = [fromIntegral a]: b: pairToStrings xs

writeCode :: (Integral a1, Num a) => [(a1, [a])] -> [a]
writeCode [] = []
writeCode xs = concat (pairToStrings xs)

rle :: (Num a, Eq a) => [a] -> [a]
rle xs = writeCode $ bigNum $ nonRecurring $ encodeSeries $ group xs

coder finName foutName = do
    inp <- BS.readFile finName
    let inpUnp = BS.unpack inp
    let newStr = rle inpUnp
    let out = BS.pack newStr
    BS.writeFile foutName out
