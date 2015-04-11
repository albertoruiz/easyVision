module Util.Audio(
    loadAudio,
    savePlayAudio
) where

import Numeric.LinearAlgebra.HMatrix
import System.Process(system)

-- | load an audio file (currently using sox, TO DO: read sample rate)
loadAudio :: FilePath -> IO (Matrix Double)
loadAudio fpath = do
    let f = fpath ++ ".txt"
    _ <- system $ "sox "++fpath ++" -t dat - | sed '/;/d' - > "++f
    r <- loadMatrix f
    _ <- system $ "rm " ++ f
    return r

-- | save a matrix to an audio file and play it (using sox's play)
savePlayAudio :: Int -> Matrix Double -> FilePath -> IO ()
savePlayAudio rate m fpath = do
    saveMatrix fpath "%f" m
    _ <- system $ "play -r "++show rate++" -v 0.5 "++fpath
    return ()

