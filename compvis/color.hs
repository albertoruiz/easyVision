import EasyVision
import Foreign

work' = unsafePerformIO . hsvToRGB . putChannels . boost

boost im = (hCh im, constImage 255 (size $ rgb im), gray im)

-----------------------------------------------------------------

main = do
    sz <- findSize
    prepare

    (cam,ctrl) <- getCam 0 sz
               >>= withChannels
               >>= monitorizeIn "video" (mpSize 10) rgb 
               >>= withPause

    hsvPalette

    e <- evWindow () "pseudocolor" sz Nothing (const (kbdcam ctrl))

    launch $ inWin e $ drawImage . work' =<< cam
