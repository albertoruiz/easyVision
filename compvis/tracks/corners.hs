import EasyVision hiding ((.@.))
import EasyVision.MiniApps.Corners
import Control.Arrow((&&&))
import Util.Options

main1 = do
    corners <- getCornerDetector
    mintrk <- getOption "--mintrk" 20
    run $ camera ~> float . grayscale >>= corners >>= cornerTracker mintrk >>= timeMonitor

main2 = run $ (camera ~> float . grayscale)
            ~> (id &&& corners defCornerParam) >>= cornerMonitor "kk" >>= timeMonitor

main3 = run $ (camera ~> float . grayscale .&. winCornerParam)
           ~> (fst &&& (uncurry $ flip corners)) >>= cornerMonitor "kk" >>= timeMonitor


main = run $ camera ~> float . grayscale >>= corners .@. winCornerParam >>= cornerMonitor "kk" >>= timeMonitor


main4 = run $ camera ~> float . grayscale >>= cornerDetector >>= cornerMonitor "kk" >>= timeMonitor


f .@. wp = (wp .&. ) . return >~> snd &&& uncurry f
