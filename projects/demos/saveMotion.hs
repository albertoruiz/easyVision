-- $ ./saveMotion --chan video
-- $ mencoder -o saved.avi saved.y4m -vc rawi420 -fps 15 -ovc lavc -lavcopts vcodec=mpeg4:mbd=2:trell:v4mv:turbo:vbitrate=2000
-- $ mplayer saved.avi

{-# LANGUAGE TemplateHaskell, Arrows, RecordWildCards #-}

import Vision.GUI
import ImagProc
import ImagProc.Camera
import Util.Misc(debug)

autoParam "SParam" ""  [  ("threshold","Float",realParam 20 0 200) ]

main = run $ arrL difs >>> g @@@ winParam >>> sMonitor "input" f >>> saveWin

difs imgs = zipWith dif imgs (tail imgs)
  where
    dif xa x = (round .(/10000) $ sum8u (sub8u 1 (grayscale xa) (grayscale x)), x)

g SParam{..} = (,) threshold

f roi (t,(d,x)) =  [ Draw [ Draw (rgb x) , g $ text (Point 0 0) (show d) ] ]
  where
      g | fromIntegral d > t = color red
        | otherwise = color white


saveWin = proc (t,(d, img)) -> do
    let y | fromIntegral d > t = Right (yuv img)
          | otherwise = Left (yuv img)
    z <- case y of
            Right x -> saver "saved.y4m" <<< observe "to be saved" id -< x
            Left  x -> returnA -< x
    returnA -< z
  where
    saver :: FilePath -> ITrans ImageYUV ImageYUV
    saver name = transUI $ do
        f <- yuvHeader 480 640 name
        return . adaptMb $ \cam -> do
                x <- cam
                saveYUV4Mpeg f x
                return x

