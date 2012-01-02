import EasyVision
import Data.Time(getCurrentTime, UTCTime)
 
main = run $ clock >>= f >>= timeMonitor

f :: Show x => VC x x
f = observe "thing" (text (Point 0.9 0) . show)

clock :: IO (IO UTCTime)
clock = return getCurrentTime 
