import EasyVision.GUI
import ImagProc
import Vision.Apps.Contours

main = run  $   grayscale
            >.  contours
            .>  id *** filter (not . elongated 8) . map shape
            >>> showCanonical
            >>> injectPrototypes
            .>  matchShapes 0.3 0.25
            >>> showAlignment
            >>> freqMonitor

