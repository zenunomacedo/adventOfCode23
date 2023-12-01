import Data.Char
import Data.List
import Data.Maybe (fromJust, mapMaybe)

main = readFile "input.txt" >>= print . sum . map ( read . (\l -> [head l, last l]) . filter (isDigit) . processLine) . lines  

processLine [] = []
processLine l@(x:xs) = 
    let (x:xs) = if isDigit (head l)
        then l
        else foldr (\n acc -> if n `isPrefixOf` acc 
                                -- then (fromJust (lookup n lookups)) ++ (drop (length n) acc)
                                then (fromJust (lookup n lookups)) ++ (tail acc)
                                else acc) l numbers 
    in x : processLine xs

lookups = [
    ("one",   "1"),
    ("two",   "2"),
    ("three", "3"),
    ("four",  "4"), 
    ("five",  "5"), 
    ("six",   "6"), 
    ("seven", "7"), 
    ("eight", "8"), 
    ("nine",  "9"), 
    ("ten",  "10")
    ]

numbers = map fst lookups

main' = readFile "input.txt" >>= print . sum . map ( read . firstLast) . lines 
firstLast l = start l ++ end l
start l = if isDigit (head l) then [head l] else case mapMaybe (flip lookup lookups) (inits l) of 
            [] -> start (tail l)
            [v] -> v
end l = if isDigit (last l) then [last l] else case mapMaybe (flip lookup lookups) (tails l) of 
            [] -> end (init l)
            [v] -> v
