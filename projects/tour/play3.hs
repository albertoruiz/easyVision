import EasyVision
import Util.Misc(splitEvery)
 
main = run $ camera ~~> f >>= observe "RGB" rgb >>= timeMonitor

f = concatMap (\x -> x ++ reverse x ++ x) . splitEvery 5

