
import Debug.Trace

main = readFile "input.txt" >>= print . process . lines

process :: [String] -> Int
process (sds:xs) =
 let seeds = genSeeds $ words $ drop 6 sds
     s2s   = buildMap $ tail $ takeWhile (/= "") $ dropWhile (/= "seed-to-soil map:") xs
     s2f   = buildMap $ tail $ takeWhile (/= "") $ dropWhile (/= "soil-to-fertilizer map:") xs
     f2w   = buildMap $ tail $ takeWhile (/= "") $ dropWhile (/= "fertilizer-to-water map:") xs
     w2l   = buildMap $ tail $ takeWhile (/= "") $ dropWhile (/= "water-to-light map:") xs
     l2t   = buildMap $ tail $ takeWhile (/= "") $ dropWhile (/= "light-to-temperature map:") xs
     t2h   = buildMap $ tail $ takeWhile (/= "") $ dropWhile (/= "temperature-to-humidity map:") xs
     h2loc = buildMap $ tail $ takeWhile (/= "") $ dropWhile (/= "humidity-to-location map:") xs
     locations = concatMap h2loc $ concatMap t2h $ concatMap l2t $ concatMap w2l $ concatMap f2w $ concatMap s2f $ concatMap s2s seeds
     minLoc = minimum $ map fst locations
 in minLoc


buildMap :: [String] -> ((Int,Int) -> [(Int,Int)])
buildMap [] = return
buildMap (x:xs) = let [final, initial, range] = map read $ words x
                  in \(start, end) -> if start > (initial + range) || end < initial 
                                        then buildMap xs (start, end)
                                        else let prevRange = (start, rs-1)
                                                 postRange = (re+1, end)
                                                 (rs, re) = (max initial start, min (initial+range-1) (end))
                                                 displace = final - initial
                                             in (rs + displace, re + displace) : 
                                                            (if start > (rs -1) then [] else buildMap xs prevRange) ++ 
                                                            (if (re +1) > end   then [] else buildMap xs postRange)
                                            

genSeeds :: [String] -> [(Int,Int)]
genSeeds [] = []
genSeeds (s:r:xs) = (read s, (read s) + (read r) -1) : genSeeds xs