

main = readFile "input.txt" >>= print . sum . map oneLine . lines 

oneLine :: String -> Int
oneLine s = if null ourWinners then 0 else 2^((length ourWinners) -1)
 where (':':s') = dropWhile (/= ':') s
       (win, '|':ours) =  span (/= '|') s'
       winI = words win
       oursI = words ours
       ourWinners = filter (`elem` winI) oursI