import EasyVision

main = runFPS 10
    $   camera
    >>= regionMarker rgb 
    >>= rectifyRegion rgb 400 .@. winAspectRatioParam ~> snd.snd
    >>= observe "result" id


