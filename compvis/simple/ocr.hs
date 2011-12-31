import EasyVision

main = runFPS 10
    $   camera
    >>= regionMarker rgb 
    >>= rectifyRegion grayscale 400 .@. winAspectRatioParam ~> snd.snd
    >>= ocrWindow autoBinarize
    >>= timeMonitor

