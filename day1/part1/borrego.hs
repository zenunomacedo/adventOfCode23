import Data.Char

main = readFile "input.txt" >>= print . sum . map (read . (\l -> [head l, last l]) . filter (isDigit)) . lines  
