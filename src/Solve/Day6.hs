module Solve.Day6 where

import Data.Functor.Foldable
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Parsing
import Relude.Extra.Foldable1
import Text.Megaparsec
import Text.Megaparsec.Char
import Prelude hiding (many)

type Object = Text

type Input = [(Object, Object)]

runParseInput :: IO (Maybe Input)
runParseInput = runParseFile "./data/6.input" parseInput

parseInput :: Parser Input
parseInput = sepBy1 parseOrbit newline <* eof

parseObject = toText <$> many (upperChar <|> digitChar)

parseOrbit = (,) <$> (parseObject <* char ')') <*> parseObject

parseAndSolve1 = do
  input <- runParseInput
  pure $ solve1 <$> input

makeDirectOrbitMap :: [(Object, Object)] -> Map.Map Object Object
makeDirectOrbitMap os = cata alg os
  where
    alg Nil = Map.empty
    alg (Cons (orbitee, orbiter) m) = Map.insert orbiter orbitee m

getOrbitsCount orbits o = case Map.lookup o orbits of
  Nothing -> 0
  Just o' -> 1 + getOrbitsCount orbits o'

solve1 os =
  let orbits = makeDirectOrbitMap os
      allObjects = Map.keys orbits
      orbitsCount = sum $ map (getOrbitsCount orbits) allObjects
   in orbitsCount

parseAndSolve2 = do
  input <- runParseInput
  pure $ do
    i <- input
    solve2 i

makeOrbitsPath :: Map.Map Object Object -> Object -> [Object]
makeOrbitsPath orbits o = case Map.lookup o orbits of
  Nothing -> []
  Just o' -> o : (makeOrbitsPath orbits o')

findLength p1 p2 o =
  let l1 = length $ takeWhile (/= o) p1
      l2 = length $ takeWhile (/= o) p2
   in (l1 + l2) - 2

solve2 os = do
  let orbits = makeDirectOrbitMap os
  let youPath = makeOrbitsPath orbits "YOU"
  let sanPath = makeOrbitsPath orbits "SAN"
  let commonDesendents = List.intersect youPath sanPath
  viaNonEmpty minimum1 $ map (findLength youPath sanPath) commonDesendents
