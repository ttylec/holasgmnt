module Main where
import Data.Random
import Sim
import qualified Data.Map as M
import Data.Foldable (foldl')
import qualified Data.Map.Merge.Strict as M
import Data.Map.Merge.Strict ( zipWithMatched, dropMissing )

queueAtArrival' :: [Event CustomerEvent] -> [(Customer, Int)]
queueAtArrival' = snd . foldl' go (0, []) . fmap event
  where
    go (l, ls) (NewCustomer c) = (l+1, (c,l):ls)
    go (l, ls) (ServedCustomer _ _ ) = (l-1, ls)

waitingTime' :: Customer -> Double -> (Customer, Double)
waitingTime' c started = (c, started - arriving c)

main :: IO ()
main = do
    clist <- sample (customerStream Red (1000*3600*24))

    let arr = M.fromList $ fmap (\c -> (c, arriving c)) clist
        qs = M.fromList $ queueAtArrival' $ allEvents clist
        wt = M.fromList . fmap event $ served waitingTime' clist

        r' = M.merge dropMissing dropMissing (zipWithMatched (\_ x y -> (x, y))) arr qs
        r = M.merge dropMissing dropMissing (zipWithMatched (\_ (x, y) z -> (x, y, z))) r' wt

    -- print . mean . V.fromList $ fmap processing clist
    putStrLn "arrival,qsize,wtime"
    putStr . unlines . fmap (\(x, y, z) -> show x <> "," <> show y <> "," <> show z) $ M.elems r