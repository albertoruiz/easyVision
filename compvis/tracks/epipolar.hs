import EasyVision
import Data.List(transpose)
import Control.Arrow((&&&))
import Control.Monad(when)
import Vision
import Numeric.LinearAlgebra hiding ((.*))
import Graphics.UI.GLUT as GL hiding(Point,Matrix)
import Text.Printf(printf)

main = do
    corners <- getCornerDetector
    mintrk <- getOption "--mintrk" 20
    run $ camera ~> float . gray >>= corners >>= cornerTracker mintrk ~> fst
          ~~> zip [0::Int ..] >>= selectClick (drawImage.fst.snd) ~> extrem >>= showCorresp >>= timeMonitor

pru cam = do
    return $ do
        (im,(f,l)) <- cam
        let fun = estimateFundamental (map p2l f) (map p2l l)
        print (bougnoux fun)

p2l (Point x y) = [x,y]
disp = putStr . dispf 5

extrem ((k0,(im0,_)),(k,(im,tracks))) = (im,im0,(f,l)) where
    t = k - k0 + 1
    full = filter ((==t).length) . map (take t) $ tracks
    (f,l) = unzip . map (head &&& last) $ full

selectClick fundisp cam = do
    w <- evWindow (undefined,True) "SelectClick" (mpSize 10) Nothing (mouseGen acts kbdQuit)
    return $ do
        x <- cam
        (_, getit) <- getW w
        when getit $ putW w (x,False)
        (xok , _) <- getW w
        inWin w (fundisp x)
        return (xok,x)

acts = [((MouseButton LeftButton, Down, modif)  , \_ (x,_) -> (x,True) )]

showCorresp = monitor "Correspondences" (mpSize 10) g where
    g (a,b,(f,l)) =  do
        drawImage $ 0.5 .* a |+|  0.5 .* b
        let h(x,y) = vertex x >> vertex y
        pointCoordinates (mpSize 20)
        setColor 1 0 0
        renderPrimitive Lines $ mapM_ h (zip f l)
        let fun = estimateFundamental (map p2l f) (map p2l l)
            (e,df,err) = estimateEssential 1.6 (correctFundamental fun)
        text2D 0.9 0.6 (printf "%.3f" $ bougnoux $ correctFundamental fun)
        text2D 0.9 0.5 (printf "%.3f" $ df)

correctFundamental :: Matrix Double -> Matrix Double
correctFundamental f = f' where
    (u,s,v) = svd f
    s1:s2:_ = toList s
    f' = u <> diag (fromList [s1,s2,0.0]) <> trans v
