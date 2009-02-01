import EasyVision
import Graphics.UI.GLUT

f = resize (mpSize 15) . gray . channels

g = notI . canny (0.1,0.3) . gradients . gaussS 2 . float

main = do
    prepare
    xs <- map f `fmap` readFrames 0
    let ys = map g $ xs
        zs = zipWith (\a b -> blockImage [[a,b]]) ys xs
    watchList "orig" xs
    watchList "canny" ys
    watchList "both" zs
    mainLoop

watchList title zs = watch title (size (head zs)) (\k ims -> drawImage (ims!!k)) (inf zs)
    where inf xs = xs ++ repeat (last xs)

watch title sz f x = evWindow 0 title sz (Just disp) (mouse kbdQuit)
    where
    disp st = do
        k <- get st
        f k x
        windowTitle $= (title ++ ": frame #"++ show k)
    mouse _ st (MouseButton WheelUp) Down _ _ = do
        st $~ (+1)
        postRedisplay Nothing
    mouse _ st (MouseButton WheelDown) Down _ _ = do
        st $~ (max 0 . subtract 1)
        postRedisplay Nothing
    mouse def _ a b c d = def a b c d
