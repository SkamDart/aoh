module Solutions
    (
      dayOne
    , dayTwo
    ) where

import Data.Maybe
import qualified Data.Set as Set
import Debug.Trace

fromFile :: FilePath -> IO [Int]
fromFile fp = do
    f <- readFile fp
    return (fmap readLine (lines f))

readLine :: String -> Int
readLine l
    | head l == '-' = (-1) * ((read $ tail l) :: Int)
    | otherwise     = ((read $ tail l) :: Int)

dayOne :: FilePath -> IO Int
dayOne fp = do
    c <- fromFile fp
    return (foldl (+) 0 c)

cumsum :: [Int] -> [Int]
cumsum x = f x [] 0
    where f :: [Int] -> [Int] -> Int -> [Int]
          f [] ret acc = ret
          f (x:xs) ret acc = f xs (ret ++ [acc']) acc'
            where acc' = x + acc

dup :: Ord a => [a] -> Maybe a
dup [] = Nothing
dup x = dup' x Set.empty
    where dup' :: Ord b => [b] -> Set.Set b -> Maybe b
          dup' [] _     = Nothing
          dup' (x:xs) s = case (Set.member x s) of
                               True -> Just x
                               _    -> dup' xs (Set.insert x s)

dayTwo :: FilePath -> IO Int
dayTwo fp = do
    c <- fromFile fp
    let c' = trace ("values: " ++ show c) c
        cc = cumsum c
        cc' = trace ("cumsum: " ++ show cc) cc
        d = dup cc'
        --d  = (dup . cumsum) c'
        d' = trace (show d) d
    --return (fromMaybe (-1000) ((dup . cumsum) c'))
    return (fromMaybe (-10000) d')

