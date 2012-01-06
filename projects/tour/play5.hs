import EasyVision.GUI
import ImagProc.Base
import Data.Time(getCurrentTime, UTCTime)
import Control.Concurrent(threadDelay)
 
main = run $ clock >>= see

see :: Show x => VC x x
see = observe "thing" (text (Point 0.9 0) . show)

clock :: IO (IO UTCTime)
clock = return (threadDelay 10000 >> getCurrentTime )

