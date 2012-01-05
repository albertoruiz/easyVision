import EasyVision
import System.Random(randomIO)
import Util.Misc(mean,splitEvery)
import Control.Concurrent(threadDelay)
 
main = run  $    rnd
            >>=  see "x" ~~> f >>= see "mean"
            >>=  timeMonitor

see name = observe name (text (Point 0.9 0) . show)

rnd = return (threadDelay 1000 >> fmap (flip mod 10) randomIO)

avg = map (mean . map fromIntegral) . splitEvery 100

f :: [Int] -> [(Int,Double)]
f = zip [1..20] . avg
