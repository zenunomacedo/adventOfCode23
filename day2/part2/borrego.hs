import Language.HaLex.Parser
import Data.Char
import Prelude hiding ((<$>), (<*>)) 

data Cubes = Cubes { red   :: Int, 
                     green :: Int, 
                     blue  :: Int
                 }
                deriving Show

isSmallerThan :: Cubes -> Cubes -> Bool
isSmallerThan (Cubes r1 g1 b1) (Cubes r2 g2 b2) = r1 <= r2 && g1 <= g2 && b1 <= b2

joinCubes :: Cubes -> Cubes -> Cubes
joinCubes (Cubes r1 g1 b1) (Cubes r2 g2 b2) = Cubes (r1+r2) (g1+g2) (b1+b2)

maxcubes :: Cubes -> Cubes -> Cubes
maxcubes (Cubes r1 g1 b1) (Cubes r2 g2 b2) = Cubes (max r1 r2) (max g1 g2) (max b1 b2)

ours = Cubes {red=12, green=13, blue=14}

main = readFile "input.txt" >>= print . sum . map oneLine . lines 

-- if not possible, we say 0
oneLine :: String -> Int
oneLine s = r * g * b
    where (gid :: Int, Cubes r g b) = parseLine s

parseLine s = let (r, []) = head $ parseLine' s
              in r
parseLine' = (\_ gid _ cubes -> (read gid, cubes)) <$> token' "Game" <*> num <*> symbol' ':' <*> parseGame
parseGame = 
            (\h _ t -> maxcubes h t) <$> parsePull <*> symbol' ';' <*> parseGame
        <|> (\v -> v)     <$> parsePull 
parsePull = 
        (\c1 _ cr -> joinCubes c1 cr) <$> parseOneCube <*> symbol' ',' <*> parsePull
        <|> parseOneCube 

parseOneCube =  (\n _ -> Cubes (read n) 0 0) <$> num <*> token' "red"
            <|> (\n _ -> Cubes 0 (read n) 0) <$> num <*> token' "green"
            <|> (\n _ -> Cubes 0 0 (read n)) <$> num <*> token' "blue"

num =   oneOrMore (satisfy isDigit) 
    <|> (\a _ -> a) <$> oneOrMore (satisfy isDigit) <*> oneOrMore (satisfy isSpace)

token' t = token t
        <|> (\a _ -> a) <$> token t <*> oneOrMore (satisfy isSpace)

symbol' s = symbol s 
        <|> (\a _ -> a) <$> symbol s <*> oneOrMore (satisfy isSpace)