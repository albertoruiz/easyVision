import EasyVision
import Data.Time(getCurrentTime, UTCTime)
 
main = run $ clock >>= see

see :: Show x => VC x x
see = observe "thing" (text (Point 0.9 0) . show)

clock :: IO (IO UTCTime)
clock = return getCurrentTime 
