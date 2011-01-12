import EasyVision
import Graphics.UI.GLUT as GL hiding(Point) 
import Numeric.LinearAlgebra
import Numeric.GSL.Fourier

------------------------------------------------------------

maxw = 8

freqs w = [("f "++show w,realParam 0 0 1)| w <- [-w .. w :: Int]]

main = do

    sz <- findSize

    prepare

    pa <- createParameters (freqs maxw)
    pf <- createParameters (freqs maxw)

    w <- evWindow () "Synthesizer" sz (Just $ disp pa pf) (const kbdQuit)

    mainLoop

-----------------------------------------------------------------

disp pa pf _ = do
    wa <- sequence $ map (\w -> getParam pa ("f "++show w)) [-maxw .. maxw]
    wf <- sequence $ map (\w -> getParam pf ("f "++show w)) [-maxw .. maxw]
    let f k = (wa!!(k+maxw) :+ 0) * cis (2*pi*wf!!(k+maxw))
    pointCoordinates (mpSize 20)
    shcont $ invFou 200 maxw f
    postRedisplay Nothing

shcont (Closed c) = do
    renderPrimitive LineLoop $ mapM_ vertex c
    pointSize $= 5
    renderPrimitive Points (vertex (head c))    


