import EasyVision.GUI
import ImagProc
import Util.Misc(splitEvery)
 
main = run $ arrL f >>> observe "RGB" rgb >>> wait (100`div`30)

f = concatMap (\x -> x ++ reverse x ++ x) . splitEvery 30

