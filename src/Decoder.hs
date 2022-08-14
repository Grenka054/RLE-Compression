module Decoder where

import qualified Data.ByteString.Lazy as BS

--"-2ab2a2b" -> ["-2","a","b","2","a","2","b"]
breakToStr :: [a] -> [[a]]
breakToStr = map (: [])

--["-2","a","b","2","a","2","b"] -> [(-2,"ab"), (2, "a"), (2, "b")]
toPairs :: (Integral a1, Num a2) => [[a1]] -> [(a2, [a1])]
toPairs xs = toPairs' xs 0 [] 0
toPairs' [] _ str _ | not (null str) = [(1, str)]
                    | otherwise = []
toPairs' (x:xs) f str c | f == 0 && head x > 127 = toPairs' xs 1 [] (head x) --negative (word8 unsigned)
                        | f == 0 && head x > 0 && head x < 128 = (fromIntegral (head x), head xs): toPairs' (tail xs) 0 [] 0 --posistive
                        | f == 1 && c /= 0 = toPairs' xs 1 (str ++ x) (c + 1)
                        | f == 1 && c == 0 = (1, str): toPairs' (x:xs) 0 [] 0
-- [(-2,"ab"), (2, "a"), (2, "b")] -> "abaabb"
decode :: [(Int, [a])] -> [a]
decode [] = []
decode ((a,b):xs) = concat(replicate a b) ++ decode xs

unrle :: Integral a => [a] -> [a]
unrle xs = decode $ toPairs $ breakToStr xs

decoder finName foutName = do
    inp <- BS.readFile finName
    let inpUnp = BS.unpack inp
    let newStr = unrle inpUnp
    let out = BS.pack newStr
    BS.writeFile foutName out