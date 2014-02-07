{-# LANGUAGE TemplateHaskell, RecordWildCards #-}

import Vision.GUI
import System.Process
import Data.IORef
import Control.Monad(join,when)

autoParam "V4l2_ctl" ""
    [ ("device",               "Int",     intParam 1 0 1)
    , ("focus",                "Int",     intParam 0 0 255)
    , ("exposure_auto",        "String",  stringParam "1" ["0","1","3"])
    , ("exposure_absolute",    "Int"   ,  intParam 166 1 10000)
    , ("power_line_frequency", "String",  stringParam "0" ["0","1"])
    ]

ctl d p v = system ("v4l2-ctl -d /dev/video"++show d++" -c "++p++"="++v) >> return ()

main = runIt $ do
    (wp,gp) <- mkParam :: MkParam V4l2_ctl
    writeIORef (evNotify wp) $ do
        V4l2_ctl{..} <- gp
        ctl device "focus" (show focus)
        ctl device "sharpness" "0"
        when (exposure_auto/="0") $
            ctl device "exposure_auto" exposure_auto
        when (exposure_auto=="0") $
            ctl device "exposure_absolute" (show exposure_absolute)
        ctl device "power_line_frequency" power_line_frequency
    (join . readIORef) (evNotify wp)

