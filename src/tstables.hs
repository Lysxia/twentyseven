import TwoPhase

import Control.Applicative

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
  createDirectoryIfMissing
    True -- createParents
    path
  let p1 = path </> "phase1"
      p2 = path </> "phase2"
  fileExists <- and <$> mapM doesFileExist [p1,p2]
  case fileExists of
    True -> do
      hPutStrLn stderr $ "File(s) already exist(s) in '" ++ path ++"'."
      exitFailure
    False -> do
      putStrLn "Phase 1"
      encodeFile p1 phase1Compressed'
      putStrLn "Phase 2"
      encodeFile p2 phase2Compressed'
      exitSuccess

