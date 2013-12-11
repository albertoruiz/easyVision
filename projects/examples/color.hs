import Vision.GUI
import Image.Processing
import Image.Processing.Custom(histogram3D)
import Numeric.LinearAlgebra
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


histoInter :: Vector Float -> Vector Float -> Float
histoInter h1 h2 = s12 / s1
  where
    s1  = sumElements h1
    s2  = sumElements h2
    s12 = sumElements (minEach h1 h2)
    minEach a b = cond a b a a b

