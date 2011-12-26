-- Detects and optionally saves the static frames in the image sequence

import EasyVision
import System.Process(system)
import Control.Concurrent(forkIO)
import Util.Options

main = do
    th <- getOption "--sensi" 0.01
    thf <- getOption "--factor" 5
    env <- getOption "--env" 5
    
    run   $ camera 
        >>= detectStatic th thf env gray rgb
        >>= monitor "Snapshot" (mpSize 20) f
        >>= saveWin yuv

f im = do
    drawImage (rgb im)
    forkIO $ system "play /usr/share/sounds/gnome/default/alerts/drip.ogg 2> /dev/null" >> return ()
    return ()

