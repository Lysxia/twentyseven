import Misc
import Cubie
import Moves
import TwoPhase

import Control.Applicative
import Control.Monad

import Data.List
import Data.Maybe

import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO

main :: IO ()
main = do
  args <- getArgs
  path <- case args of
        [] -> (</> ".tseven") <$> getHomeDirectory
        p : _ -> return p
  ph1 <- phase1Expand <$> decodeFile (path </> "phase1")
  ph2 <- phase2Expand <$> decodeFile (path </> "phase2")
  let solve = twoPhase (phase1 ph1) (phase2 ph2)
  ph1 `seq` ph2 `seq` putStrLn "Ready."
  forever $ do
    putStr "> "
    hFlush stdout
    s <- getLine
    case mapM decodeMove s of
      Nothing -> exitSuccess
      Just cs ->
        let c = foldl' (?) iden cs
        in putStrLn . intercalate " " . map snd . fromJust . solve $ c

