import Vision.GUI
import Image.Processing
import Data.Time(getCurrentTime, UTCTime)
import Control.Concurrent(threadDelay)
 
main = runT_ clock see

see :: Show x => ITrans x x
see = observe "time" (text (Point 0.9 0) . show)

clock :: IO (IO (Maybe UTCTime))
clock = return (threadDelay 10000 >> Just `fmap` getCurrentTime )

