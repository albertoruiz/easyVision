import Vision.GUI
import Image.Processing
import Picglaze.Histogram
import Picglaze.Tools(histoInter)
import Util.Misc(debug)

main = run $ getTemplate
         >>> observe "template" snd
         >>> observe "imageM" hm


getTemplate = clickKeep "define region and click to set template" f g Nothing
  where
    f r = setRegion r . rgb
    g = Draw . rgb . fst

nor img = recip mx .* img
  where
    (_,mx) = minmax img

hm (x,t) = Draw [ Draw (rgb x)
                , text (Point 0.9 0.65) h
                ]
  where
    h = printf "%.2f" $ histoInter (histogram3D 4 t) (histogram3D 4 . rgb $ x)


