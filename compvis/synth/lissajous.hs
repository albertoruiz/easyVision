import EasyVision
import Graphics.UI.GLUT hiding (Point)

deltaf = 2*pi/1000
sz = mpSize 20

vert a b f t = vertex $ Point (0.6*cos (a*t-f)) (0.6*sin (b*t))

fi n = fromIntegral (n::Int) 

main = do
    prepare
    w <- evWindow 0 "Curve" sz Nothing (const kbdQuit)
    lineSmooth $= Enabled
    lineWidth $= 5
    setColor 1 1 0
    pointCoordinates sz
    p <- createParameters [("wx",intParam 2 1 10),
                           ("wy",intParam 3 1 10),
                           ("nd",intParam 5 1 100)]
    launchFreq 50 $ inWin w $ do
        a <- getParam p "wx"
        b <- getParam p "wy"
        nd <- getParam p "nd"
        let delta = 2*pi/(fi nd)
            t = [0,delta..2*pi]
        phi <- getW w
        putW w (phi+deltaf)
        renderPrimitive LineStrip $ mapM_ (vert (fi a) (fi b) phi) t
