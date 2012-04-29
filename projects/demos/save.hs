-- $ mplayer saved.y4m -demuxer y4m
--             (or      -vc rawi420)
-- $ mencoder -o saved.avi saved.y4m -vc rawi420 -fps 30 -ovc lavc -lavcopts vcodec=mpeg4:mbd=2:trell:v4mv:turbo:vbitrate=2000

{-# LANGUAGE Arrows #-}

import Vision.GUI
import ImagProc
import ImagProc.Camera

main = run saveWin

main2 = run $   observe "Save" rgb
            >>> arr yuv >>> arrL (take 100) >>> saver "saved.y4m"


selectSave :: ITrans ImageYUV (Either ImageYUV ImageYUV)
selectSave = clickTag id id sh "Click to save"
  where
    sh (Right x) = Draw [ Draw x, color red $ text (Point 0 0) "RECORDING..."]
    sh (Left  x) = Draw x


saveWin = proc img -> do
    y <- selectSave -< yuv img
    z <- case y of
            Right x -> saver "saved.y4m" <<< observe "to be saved" id -< x
            Left  x -> returnA -< x
    returnA -< z

saver :: FilePath -> ITrans ImageYUV ImageYUV
saver name = transUI $ do
    f <- yuvHeader 480 640 name
    return $ \cam -> do
            x <- cam
            saveYUV4Mpeg f x
            return x

