import EasyVision

main = run $ camera ~> grayscale >>= getBackground 

getBackground = clickStatusWindow "background difference" (mpSize 10) Nothing f g h
  where
    f x _ = Just x
    h _ _ = return ()
    g x Nothing = drawImage' x
    g x (Just bg) = drawImage' $ absDiff8u x bg

