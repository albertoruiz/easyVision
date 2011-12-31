import EasyVision
import Util.Misc(splitEvery)
 
main = runFPS 45 $ camera ~~> f >>= observe "RGB" rgb >>= timeMonitor

f = g . concat . map (\x -> x ++ reverse x ++ x) . splitEvery 5

g = map head . splitEvery 2

-- FPS limit (!)

