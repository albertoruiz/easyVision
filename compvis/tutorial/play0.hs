import EasyVision

main = do
    prepare
    sz <- findSize
    cam <- getCam 0 sz >>= monitor "Video" sz drawImage
    launch $ do
        cam  -- do nothing with the grabbed image
        return ()
