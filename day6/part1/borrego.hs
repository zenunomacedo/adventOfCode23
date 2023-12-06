import Data.List

main :: IO ()
main = readFile "input.txt" >>= print . product . map (length . (\[t :: Int, d] -> [tc * tp | tc <- [1..t-1], let tp = t - tc, (tc * tp) > d]) . map read) . transpose . map (words . tail . dropWhile (/= ':')) . lines