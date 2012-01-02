import EasyVision
import System.Random(randomIO)
import Util.Misc(mean,splitEvery) 
 
main = run $ rnd >>= f "x" ~~> zip [1..5] . avg >>= f "mean" >>= timeMonitor

f :: Show x => String -> VC x x
f name = observe name (text (Point 0.9 0) . show)

rnd :: IO (IO Int)
rnd = return (fmap (flip mod 10) (randomIO :: IO Int)) 

avg :: [Int] -> [Double]
avg = map (mean . map fromIntegral) . splitEvery 100
