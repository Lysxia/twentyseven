{-# LANGUAGE ScopedTypeVariables #-}
import Rubik.Distances
import Control.Exception
import qualified Data.Vector.HalfByte as HB
import qualified Data.Vector.Generic.Mutable.Loops as MG
import Data.Time.Clock

main = do
  start <- getCurrentTime
  v :: HB.Vector' <- distancesM MG.iForM_ n root neighbors
  end <- getCurrentTime
  putStrLn $ "Distances: " ++ show (diffUTCTime end start)
  where
    n = 5^10 - 1 :: Int
    root = 0
    ops =
     [ (*2)
     , (*3)
     , (*5)
     , (*7)
     , (*11)
     , (+42)
     , (subtract 42)
     , (\x -> x * x)
     ]
    neighbors = \x -> [(f (x+1) `mod` (n+1)) `mod` n | f <- ops]
