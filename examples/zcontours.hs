import Vision.GUI
import Image.Processing
import ImagProc.Contrib.Contours

main = run  $   arr grayscale
            >>> arr id &&& (npContours @@@ winParam)
            >>> sMonitor "contours" gs

--------------------------------------------------------------------------------

gs _ (image,contours) = [ Draw [Draw image, found]
                        , found
                        ]
  where
    ((dark,light),open) = contours
    found = Draw [ color lightgreen open
                 , color blue  $ dark
                 , color red $ light
                 ]

