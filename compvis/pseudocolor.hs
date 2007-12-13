import EasyVision

main = do
    sz <- findSize
    prepare

    (cam,ctrl) <- getCam 0 sz
               >>= withChannels
               >>= monitorizeIn "video" (mpSize 10) rgb
               >>= withPause

    hsvPalette

    --o <- createParameters [("kg",intParam 45 0 255),
    --                       ("ks",intParam 160 0 255)]

    e <- evWindow () "pseudocolor" sz Nothing (const (kbdcam ctrl))

    launch $ inWin e $ do
        --kg <- fromIntegral `fmap` (getParam o "kg" :: IO Int)
        --ks <- fromIntegral `fmap` (getParam o "ks" :: IO Int)
        img <- cam >>= rgbToHSV . rgb
        hsvCode img
        drawImage =<< hsvToRGB img

