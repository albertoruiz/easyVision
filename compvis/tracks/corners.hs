import EasyVision hiding ((.@.))
import EasyVision.MiniApps.Corners
import Control.Arrow((&&&))
import Util.Options

main1 = do
    corners <- getCornerDetector
    mintrk <- getOption "--mintrk" 20
    run $ camera ~> float . gray >>= corners >>= cornerTracker mintrk >>= timeMonitor

main2 = run $ (camera ~> float . gray)
            ~> (id &&& corners defCornerParam) >>= cornerMonitor "kk" >>= timeMonitor

main3 = run $ (camera ~> float . gray .&. winCornerParam)
           ~> (fst &&& (uncurry $ flip corners)) >>= cornerMonitor "kk" >>= timeMonitor


main = run $ camera ~> float . gray >>= corners .@. winCornerParam >>= cornerMonitor "kk" >>= timeMonitor


main4 = run $ camera ~> float . gray >>= cornerDetector >>= cornerMonitor "kk" >>= timeMonitor


f .@. wp = (wp .&. ) . return >~> snd &&& uncurry f
