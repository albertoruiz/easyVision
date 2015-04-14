module Vision.LASBA(
    laSBA, laSBAg
) where

import Numeric.LinearAlgebra.HMatrix as LA
import Util.Misc(vec,Vec,Mat)

import Vision.Types
import Vision.IO

import System.IO
import System.Process
import System.IO.Unsafe(unsafePerformIO)

-- | Interface to sparse bundle adjustment implementation by M.I.A. Lourakis and A.A. Argyros
laSBA' :: [Proj]
       -> [Vec] -- ^ homog 3D points
       -> [Mat] -- ^ calibrated cameras
       -> Mat   -- ^ common calibration matrix
       -> String -- ^ additional arguments to the executable
       -> ([Vec],[Mat]) -- ^ refined points and cameras
laSBA' prjs pts cams kal epilog = unsafePerformIO $ do
   (name,h) <- openTempFile "/tmp" "bundle"   
   let names@[fc,fp,fk,rc,rp] = map (name++) $ words "cams.txt pts.txt kal.txt refcams.txt refpts.txt"
   saveQCams fc cams
   saveRawTracks fp (pts,prjs)
   saveMatrix fk "%f" kal
   putStr "\^[[2m"
   _ <- system $ "lasba "++unwords [fc,fp,fk,rc,rp] ++ epilog
   putStr "\^[[0m"
   refcams <- loadQCams rc
   refpts <- loadMatrix rp
   hClose h
   _ <- system $ "rm "++unwords (name:names)
   return (toRows (refpts¦1),refcams)

-- | Interface to sparse bundle adjustment implementation by M.I.A. Lourakis and A.A. Argyros
laSBAg :: String -- ^ additional arguments to the executable
       -> [Proj]
       -> [Vec] -- ^ homog 3D points
       -> [Mat] -- ^ calibrated cameras
       -> [Mat] -- ^ calibration matrices
       -> ([Vec],[Mat]) -- ^ refined points and cameras
laSBAg epilog prjs pts cams [k]  = laSBA' prjs pts cams k epilog
laSBAg epilog prjs pts cams kals = laSBA' kprjs pts cams k epilog
  where
    kprjs = calibrateProjections (map (<> inv k) kals) prjs
    k = diag (vec [1000,1000,1])

-- | laSBA \" 2> /dev/null\"
laSBA :: [Proj]
      -> [Vec] -- ^ homog 3D points
      -> [Mat] -- ^ calibrated cameras
      -> [Mat] -- ^ calibration matrices
      -> ([Vec],[Mat]) -- ^ refined points and cameras
laSBA  = laSBAg " "

