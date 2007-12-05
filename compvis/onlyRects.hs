-- average of all rectangles found

module Main where

import EasyVision
import Text.Printf(printf)

drift alpha = virtualCamera (return . drifter)
    where drifter (a:b:rest) = a : drifter ((alpha .* a |+| (1-alpha).* b):rest)

main = do
    sz <- findSize
    ratio <- getOption "--ratio" (sqrt 2)
    alpha <- getOption "--alpha" 0.9
    let k = height sz `div` 32
    let szR = Size (32*k) (round (32*fromIntegral k*ratio))
        nm = "ratio " ++ printf "%.2f" ratio
    prepare

    (cam,ctrl) <- getCam 0 (mpSize 20)
               >>= onlyRectangles szR ratio
               >>= virtualCamera (return . concat)
--               >>= drift alpha
               >>= withPause

    w <- evWindow () nm sz Nothing (const $ kbdcam ctrl)

    launch $ do
        inWin w $ (cam :: IO ImageRGB) >>= drawImage
