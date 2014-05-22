module TD2 where

import Data.Ratio
import Data.List
import Data.Maybe

data Prob a = Prob [(a, Rational)]
              deriving Show
                       
instance Functor Prob where
  fmap f (Prob p) = Prob $ fmap (\(a, r)->(f a, r)) p

instance Monad Prob where
  return a = Prob [(a, 1)]
  fail _ = Prob []
  Prob p >>= f = Prob $ concat $ map (\(a, r) ->
                               let Prob p' = f a
                                   in map (\(a',r')->(a', r*r')) p') p

sameProbability :: [a] -> Prob a
sameProbability l = let p = 1 % (fromIntegral $ length l)
                    in  Prob $ map(\a -> (a,p)) l

----------------------------------
-- Simplification et extraction --
----------------------------------

canonize :: (Eq a, Ord a) => Prob a -> Prob a
canonize (Prob p) = Prob $ let g = groupBy (\ a b -> (fst a == fst b)) $ sort p
                            in map (\l -> (fst $ head l, sum $ map snd l )) g

probability :: (Eq a, Ord a)  => a -> Prob a -> Rational
probability v p = let (Prob l) = canonize p 
                  in let prob = lookup v l
                           in fromMaybe 0 prob


----------
--Tests --
----------

dice :: Prob Int
dice = sameProbability [1, 2, 3, 4, 5, 6]

double :: Prob Bool
double = do
  x <- dice
  y <- dice
  return $ x == y

pair :: Prob Int
pair = do
  x <- dice
  y <- dice
  return $ x + y

----------------------------------
-- Probabilités conditionnelles --
----------------------------------

sick :: Prob Bool
sick = let prob = 1 % 100000
       in Prob [(True, prob), (False, 1 - prob)]

positive :: Bool -> Prob Bool
positive True = let prob = 1 % 1000
                in Prob [(True, 1 - prob), (False, prob)]
positive False = let prob = 1 % 1000
                in Prob [(True, prob), (False, 1 - prob)]

-- Probabilité que la personne soit malade ou non suivant le résultat du test
results :: Prob Bool
results = do
  s <-sick
  p <- positive s
  if p
     then return s
     else fail "result is negative"

renormalize :: Prob a -> Prob a
renormalize (Prob p) = let sumProb = sum $ map snd p
                           in Prob $ map (\(v, prob) -> (v, prob / sumProb)) p

---------------
-- Interlude --
---------------

cleave :: a -> [a->b] -> [b]
cleave a  = map ($a)

spread :: [a->b]->[a]->[b]
spread = zipWith ($)
