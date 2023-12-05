

main = readFile "input.txt" >>= print . process . lines

process :: [String] -> Int
process (sds:xs) =
 let seeds = map read $ words $ drop 6 sds
     s2s   = buildMap $ tail $ takeWhile (/= "") $ dropWhile (/= "seed-to-soil map:") xs
     s2f   = buildMap $ tail $ takeWhile (/= "") $ dropWhile (/= "soil-to-fertilizer map:") xs
     f2w   = buildMap $ tail $ takeWhile (/= "") $ dropWhile (/= "fertilizer-to-water map:") xs
     w2l   = buildMap $ tail $ takeWhile (/= "") $ dropWhile (/= "water-to-light map:") xs
     l2t   = buildMap $ tail $ takeWhile (/= "") $ dropWhile (/= "light-to-temperature map:") xs
     t2h   = buildMap $ tail $ takeWhile (/= "") $ dropWhile (/= "temperature-to-humidity map:") xs
     h2loc = buildMap $ tail $ takeWhile (/= "") $ dropWhile (/= "humidity-to-location map:") xs
     locations = map (h2loc . t2h . l2t . w2l . f2w . s2f . s2s) seeds
     minLoc = minimum locations
 in minLoc

buildMap :: [String] -> (Int -> Int)
buildMap [] = id
buildMap (x:xs) = let [final, initial, range] = map read $ words x
                  in \n -> if n >= initial && n < (initial + range)
                             then final + (n - initial)
                             else (buildMap xs) n