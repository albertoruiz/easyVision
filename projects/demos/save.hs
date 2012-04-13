-- $ mplayer saved.y4m -demuxer y4m
--             (or      -vc rawi420)

{-# LANGUAGE Arrows #-}

import Vision.GUI
import ImagProc
import ImagProc.Camera

main = run saveWin2

main2 = run $   observe "Save" rgb
            >>> arr yuv >>> arrL (take 100) >>> saver "saved.y4m"


selectSave :: ITrans ImageYUV (Either ImageYUV ImageYUV)
selectSave = clickTag id id sh "Click to save"
  where
    sh (Right x) = Draw [ Draw x, color red $ text (Point 0 0) "RECORDING..."]
    sh (Left  x) = Draw x

-----------------------------------------------

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

------------------------------------------------

saveWin2 = arr yuv >>> selectSave >>> saver2 "saved.y4m"

saver2 :: FilePath -> ITrans (Either ImageYUV ImageYUV) ImageYUV
saver2 name = transUI $ do
    f <- yuvHeader 480 640 name
    return $ \cam -> do
            x <- cam
            case x of
                Right y -> do saveYUV4Mpeg f y
                              return y
                Left y -> return y

