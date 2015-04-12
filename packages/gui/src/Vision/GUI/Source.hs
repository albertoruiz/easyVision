 {-# LANGUAGE RecordWildCards #-}

-----------------------------------------------------------------------------
{- |
Module      :  Vision.GUI.Source
Copyright   :  (c) Alberto Ruiz 2012-13
License     :  GPL

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional

-}
-----------------------------------------------------------------------------

module Vision.GUI.Source (
    run, camera,
    camG, cameraFolderIM
) where

import Graphics.UI.GLUT hiding (Point,Size,color)
import Vision.GUI.Types
import Vision.GUI.Interface
import Control.Arrow((***),(>>>))
import Util.Geometry
import Image
import Image.Devel
import Image.Capture
import Vision.GUI.Arrow--(ITrans, Trans,transUI,transUI2,runT_)
import Util.LazyIO(mkGenerator)
import Util.Options
import Control.Concurrent(threadDelay)
import Data.Colour.Names
import Vision.GUI.Util

someChannelsFromYUYV :: ImageYCbCr -> Channels
someChannelsFromYUYV x = CHIm {..}
  where
    yuv = no
    yCh = yuyv2gray x
    uCh = no
    vCh = no
    rgb = yuyv2rgb x
    rCh = no
    gCh = no
    bCh = no
    hsv = no
    hCh = no
    sCh = no
    fCh = gray2float yCh
    no = error "channel not available. Use hVision-ipp"

someChannelsFromRGB :: Image Word24 -> Channels
someChannelsFromRGB x = CHIm {..}
  where
    yuv = no
    yCh = rgb2gray x
    uCh = no
    vCh = no
    rgb = x
    rCh = no
    gCh = no
    bCh = no
    hsv = no
    hCh = no
    sCh = no
    fCh = gray2float yCh
    no = error "channel not available. Use hVision-ipp"


{- |
Returns the first image source given in the command line.

It also admits --photos=path/to/folder/ containing separate image files (.jpg or .png), currently read using imagemagick (slower, lazy),
and --photosmp=path/to/folder, to read images of the same type and size using mplayer (faster, strict).
-}
camera :: Generator Channels
camera = do
    f <- hasValue "--photos"
    g <- hasValue "--photosmp"
    h <- hasValue "--sphotos"
    if f then cameraFolderIM
         else if g then cameraFolderMP
                   else if h then cameraP
                             else cameraV


cameraP :: Generator Channels
cameraP = do
    hp <- optionString "--sphotos" "."
    g <- readFolderIM hp
    c <- mkGenerator g
    return (fmap (someChannelsFromRGB.fst) <$> c)

cameraV :: Generator Channels
cameraV = fmap (fmap (fmap someChannelsFromYUYV)) gcam

cameraFolderIM :: Generator Channels
cameraFolderIM = camG "--photos" r <*> dummy
  where
    r p _sz = map (someChannelsFromRGB *** id) <$> readFolderIM p

cameraFolderMP :: Generator Channels
cameraFolderMP = camG "--photosmp" rf <*> dummy
  where
    rf p sz = map (someChannelsFromYUYV *** id) <$> readFolderMP p sz


camG
    :: String
    -> (String -> Maybe Size -> IO [(Channels, [Char])])
    -> IO (IO (Maybe t) -> IO (Maybe Channels))
camG opt readf = do
    path <- optionString opt "."
    sz <- parseSize <$> optionString "--size" "640x480"
    imgs <- readf path (Just sz)
    interface (Size 240 320) "photos" (0,imgs) ft (keys imgs) [] r sh
  where
    keys xs = acts (length xs -1)
    acts n = [ (key (MouseButton WheelUp),   \_ _ (k,xs) -> (min (k+1) n,xs))
             , (key (SpecialKey  KeyUp),     \_ _ (k,xs) -> (min (k+1) n,xs))
             , (key (MouseButton WheelDown), \_ _ (k,xs) -> (max (k-1) 0,xs))
             , (key (SpecialKey  KeyDown),   \_ _ (k,xs) -> (max (k-1) 0,xs))]
    r _ (k,xs) _ = ((k,xs), fst $ xs!!k)
    sh _ (k,xs) _a x = Draw [Draw (rgb x), info (k,xs) ]
    -- ft w _ = evPrefSize w $= Just (Size 240 320)
    ft _ _ = return ()
    info (k,xs) = Draw [color black $ textF Helvetica12 (Point 0.9 0.6)
                        (show w ++ "x" ++ show h ++ " " ++snd ( xs!!k)) ]
      where
        Size h w = (size.rgb) (fst $ xs!!k)

dummy :: Generator ()
dummy = return (threadDelay 100000 >> return (Just ()))

run :: ITrans Channels b -> IO ()
run t = runT_ camera (t >>> optDo "--freq" freqMonitor)

