import Vision.GUI
import ImagProc
import ImagProc.Contrib.ZBar
import Contours(roi2poly)

main = run $ observe "zbar" (f.grayscale)

f x = Draw  [  Draw x, drawBarcodes (size x) (zbar x) ]
  where
    drawBarcodes sz = Draw . map (drBC sz)
    drBC sz bc = Draw [ color blue, Draw bb, text p (bcType bc ++ ": " ++ bcValue bc) ]
      where
        bb = roi2poly sz (bcROI bc)
        p = last (polyPts bb)

