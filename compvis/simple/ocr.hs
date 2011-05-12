import EasyVision

main = runFPS 10
    $   camera
    >>= regionMarker (mpSize 20) rgb 
    >>= rectifyRegion gray 400 .@. winAspectRatioParam ~> snd.snd
    >>= ocrWindow autoBinarize
    >>= timeMonitor

autoBinarize img = binarize8u (otsuThreshold img) img

