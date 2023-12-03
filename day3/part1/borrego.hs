
import Data.Char (isDigit)

type Matrix = [[Char]]
type Line = Int
type Col = Int

main = readFile "input.txt" >>= print . countParts 0 0 . lines

countParts :: Line -> Col -> Matrix -> Int
countParts l c m = case (l >= lmax, c >= cmax) of 
    (True, _) -> 0
    (_, True) -> countParts (l+1) 0 m
    _         -> if isPNumber m l c && verifyPN m l c
                    then (numberFrom m l c) + (skipNumber m l c)
                    else countParts l (c+1) m
 where cmax = length $ head m
       lmax = length m

verifyPN :: Matrix -> Line -> Col -> Bool 
verifyPN m l c = any (isPart m) [(ln,cn) | ln <- [l-1 .. l+1], cn <- [c-1 .. c+1], (ln,cn) /= (l,c)] 
              || (isPNumber m l (c+1) && verifyPN m l (c+1))

isPart :: Matrix -> (Line, Col) -> Bool 
isPart m (l, c) = not (l < 0 || c < 0 || l >= lmax || c >= cmax) && isPartSymbol (m!!l!!c) 
 where cmax = length $ head m
       lmax = length m
       isPartSymbol c = not (isDigit c) && c/= '.'

isPNumber :: Matrix -> Line -> Col -> Bool
isPNumber m l c = c < cmax && isDigit (m!!l!!c)
 where cmax = length $ head m

numberFrom :: Matrix -> Line -> Col -> Int
numberFrom m l c = read $ takeWhile isDigit $ drop c (m!!l)

skipNumber :: Matrix -> Line -> Col -> Int
skipNumber m l c = if isPNumber m l c 
                    then skipNumber m l (c+1)
                    else countParts l c m
  where cmax = length $ head m