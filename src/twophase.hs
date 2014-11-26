import Misc
import Coord
import Cubie
import Moves
import TwoPhase

import Control.Applicative
import Control.Monad
import Control.DeepSeq

import Data.List
import Data.Maybe

import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO

main :: IO ()
main = do

  twoPhaseTables `seq` putStrLn "Ready."
  forever $ do
    putStr "> "
    hFlush stdout
    s <- getLine
    case mapM decodeMove s of
      Nothing -> exitSuccess
      Just [] -> exitSuccess
      Just cs ->
        let c = foldl' (?) iden cs
        in putStrLn . intercalate " " . map snd . fromJust . twoPhase $ c

