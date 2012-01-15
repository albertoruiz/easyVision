import EasyVision.GUI
import ImagProc
import Util.Misc(splitEvery)
 
main = run $ arrL f >>> observe "RGB" rgb >>> freqMonitor

f = concatMap (\x -> x ++ reverse x ++ x) . splitEvery 5

