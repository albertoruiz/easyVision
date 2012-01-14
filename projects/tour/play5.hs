import EasyVision.GUI
import ImagProc.Base
import Data.Time(getCurrentTime, UTCTime)
import Control.Concurrent(threadDelay)
 
main = runT_ clock see

see :: Show x => Trans x x
see = observe "time" (text (Point 0.9 0) . show)

clock :: IO (IO UTCTime)
clock = return (threadDelay 10000 >> getCurrentTime )

