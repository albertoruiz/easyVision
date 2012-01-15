import EasyVision.GUI
import ImagProc
import Vision.Apps.Contours

main = run $ arr grayscale
         >>> contours
         >>> arr (id *** filter (not . elongated 8) . map shape)
         >>> showCanonical
         >>> injectPrototypes "../../data/shapes/all.txt"
         >>> arr (matchShapes 0.3 0.25)
         >>> showAlignment
         >>> freqMonitor

