-- Detects and optionally saves the static frames in the image sequence

import EasyVision
import System(system)

main = do
    sz <- findSize
    th <- getOption "--sensi" 0.01
    env <- getOption "--env" 5
    sv <- optionalSaver sz

    prepare

    cam <- getCam 0 sz  >>= detectStatic th env
    w <- evWindow () "static" sz Nothing (const kbdQuit)

    launch (worker w cam sv)

-----------------------------------------------------------------

worker w cam save = do

    im <- cam

    inWin w $ do
        drawImage im

    system "mplayer /usr/share/sounds/KDE_Notify.wav > /dev/null &"

    save im
