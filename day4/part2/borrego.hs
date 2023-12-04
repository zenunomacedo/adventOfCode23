
import Data.Array
import Control.Monad.State.Strict
import qualified Data.Map as Map
import Debug.Trace

type Card = (Int, Int)
type Card2 = (Int, Int, Int)

main = readFile "input.txt" >>= print . processCards3 . map oneLine . lines 

-- transform one line into card
oneLine :: String -> Card
oneLine s = (idI, numWins)
 where (iden, ':':s') = span (/= ':') $ drop 5 s
       (win, '|':ours) =  span (/= '|') s'
       idI = read iden
       winI = words win
       oursI = words ours
       wins = filter (`elem` winI) oursI
       numWins = length wins

-- get the total value of one card
grabById2 :: [Card2] -> Int -> Int
grabById2 (c@(i,w,v):t) n = if i == n then v else grabById2 t n


-- (from last to first card) count the value of a card, add it to list of computed cards, go to next
processCards3 :: [Card] -> Int
processCards3 c = processCards3' (reverse c) []
 where processCards3' [] _ = 0
       processCards3' ((i, w):t) l = let nextCardsIds = take w [i+1, i+2..]
                                         nextCardValues = sum $ map (grabById2 l) nextCardsIds
                                         thisCardValue = 1 + nextCardValues
                                     in thisCardValue + processCards3' t ((i,w,thisCardValue):l)




--
--
-- can we generate ALL the cards and count them? Nope. 
processCards :: [Card] -> [Card]
processCards c = processCards' (listArray (1, length c) c)
 where processCards' a = processCards'' c
        where processCards'' [] = []
              processCards'' ((i, w):t) =
                            let nextCardsIds = take w [i+1, i+2..]
                                nextCards = map (grabById a) nextCardsIds
                            in (i,w) : processCards'' (t ++ nextCards)

grabById :: Array Int Card -> Int -> Card
grabById a n = a!n


-- can we generate ALL the cards and count them? Nope, not even optimizing it. 
processCards2 :: [Card] -> Int
processCards2 c = processCards' (listArray (1, length c) c)
 where processCards' a = fst $ processCards'' c Map.empty
        where processCards'' :: [Card] -> (Map.Map Int Int) -> (Int, (Map.Map Int Int))
              processCards'' [] st = do 
                    trace (show $ Map.toList st) $ (0, st)
              processCards'' ((i, w):t) st = do
                                case Map.lookup i st of 
                                    Just v -> trace ("Add " ++ show i) $ (v, st) 
                                    Nothing -> let nextCardsIds = take w [i+1, i+2..]
                                                   nextCards = map (grabById a) nextCardsIds
                                                   (recurs, st') = processCards'' (nextCards ++ t) st
                                                   ret = 1 + recurs
                                                   st'' = Map.insert i ret st'
                                               in (ret, st'')



 