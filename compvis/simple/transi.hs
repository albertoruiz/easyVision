import EasyVision


save sz cam = do
    sv <- optionalSaver sz
    return $ do {x <- cam; sv (toYUV x); return x}

main = run $ camera ~> rgb ~~> concatMap f
       >>= save (mpSize 20) >>= observe "trans" id

f x = [resize (mpSize 20) (modifyROI (const (ROI k 479 k 639)) x) | k <- [200,198 ..0]]
      ++ replicate 50 x