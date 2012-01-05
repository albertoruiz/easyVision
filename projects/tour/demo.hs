import EasyVision

main = run $ camera >>= sMonitor "result" f >>= timeMonitor

f roi x = [ msg "grayscale"        [ Draw g ]
          , msg "gaussian filter " [ Draw smooth ]
          , msg "canny edges"      [ Draw (notI edges) ]
          , nothing ]
  where
    img = rgb x 
    g   = setRegion roi (grayscale x)
    f   = float g
    smooth = gauss Mask5x5 f
    edges = canny (0.1,0.3) . gradients $ smooth

    msg s x = Draw [ Draw img, Draw x , color yellow, text (Point 0.9 0.65) s ]
    nothing = Draw [ color yellow, text (Point 0.9 0.65) "nothing" ]

