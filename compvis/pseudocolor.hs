import EasyVision
import Foreign

work' = unsafePerformIO . hsvToRGB . putChannels . boost

boost im = (hCh im, constImage 255 (size $ rgb im), gray im)

th t bool = unsafePerformIO . binarize8u t bool

work = red

black k im = th k False (gray im)
white ks kg im = th ks True (sCh im) `andI` th kg True (gray im)
color im = notI (black 40 im)  `andI` notI (white 40 100 im)
red kg ks im = th kg True (gray im)  `andI`
         th ks True (sCh im)  `andI`
         (th 235 True (hCh im) `orI` th 20 False (hCh im))

-----------------------------------------------------------------

main = do
    sz <- findSize
    prepare

    (cam,ctrl) <- getCam 0 sz
               >>= withChannels
               >>= monitorizeIn "video" (mpSize 10) rgb 
               >>= withPause

    hsvPalette

    o <- createParameters [("kg",intParam 45 0 255),
                           ("ks",intParam 160 0 255)]

    e <- evWindow () "pseudocolor" sz Nothing (const (kbdcam ctrl))

    launch $ inWin e $ do
        kg <- fromIntegral `fmap` (getParam o "kg" :: IO Int)
        ks <- fromIntegral `fmap` (getParam o "ks" :: IO Int)
        drawImage . red kg ks =<< cam
