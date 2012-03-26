import Vision.GUI
import ImagProc
import ImagProc.GPU.SIFT
import Util.Misc(debug)
import Data.Function(on)
import Data.List(maximumBy)


main = do
    prepare
    match <- getMatchGPU
    runNT_ camera $ sift grayscale >>> getView >>> viewProts >>> viewRecog match

getView = clickList "click to get prototype" (const id) g []
  where
    g ((x,_),_) = Draw (rgb x)

viewProts = sMonitor "prototypes" f
  where
    f _ (_,prots) = map (Draw . rgb . fst) prots

viewRecog match = observe "classify" f
  where
    f ((x,fs),[]) = Draw ()
    f ((x,fs),prots) | score < 20 = text (Point 0 0) "???"
                     | otherwise  = Draw (rgb p)
      where
        scores = map (\(p,fps) -> (p, length (match 0.7 0.8 fs fps))) prots
        (p,score) = maximumBy (compare `on` snd) scores

