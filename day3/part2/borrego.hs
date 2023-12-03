
import Data.Char (isDigit)
import Data.List (nub)

type Matrix = [[Char]]
type Line = Int
type Col = Int

main = readFile "input.txt" >>= print . countParts 0 0 . lines

countParts :: Line -> Col -> Matrix -> Int
countParts l c m = case (l >= lmax, c >= cmax) of 
    (True, _) -> 0
    (_, True) -> countParts (l+1) 0 m
    _         -> if isGSymbol m l c
                    then (verifyPSymbol m l c) + (countParts l (c+1) m)
                    else countParts l (c+1) m
 where cmax = length $ head m
       lmax = length m

isGSymbol :: Matrix -> Line -> Col -> Bool 
isGSymbol m l c = (m!!l!!c) == '*'

verifyPSymbol :: Matrix -> Line -> Col -> Int 
verifyPSymbol m l c = let 
                nearbies = filter (uncurry (isPNumber m)) [(ln,cn) | ln <- [l-1 .. l+1], cn <- [c-1 .. c+1], (ln,cn) /= (l,c)] 
                nearbyCoords = nub $ map (pullback m) nearbies
                in case nearbyCoords of 
                    [] -> 0
                    [_] -> 0
                    [c1, c2] -> (uncurry (numberFrom m) c1) * (uncurry (numberFrom m) c2)

isPNumber :: Matrix -> Line -> Col -> Bool
isPNumber m l c = not (l < 0 || c < 0 || l >= lmax || c >= cmax) && isDigit (m!!l!!c)
 where cmax = length $ head m
       lmax = length m

numberFrom :: Matrix -> Line -> Col -> Int
numberFrom m l c = read $ takeWhile isDigit $ drop c (m!!l)

pullback :: Matrix -> (Line, Col) -> (Line, Col)
pullback m (l,c) = if isPNumber m l (c-1) then pullback m (l,c-1) else (l,c)