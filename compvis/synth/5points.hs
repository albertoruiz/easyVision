import EasyVision hiding ((~>),c1,c2,shift)
import Graphics.UI.GLUT as GL
import Vision.Multiview
import Numeric.LinearAlgebra.Array.Util
import Numeric.LinearAlgebra
import Vision
import Data.List(transpose)
import Util.Misc(pairsWith,degree)
import Util.Rotation
import Util.Homogeneous
import Vision.TensorRep
import Vision.Tensorial

disp = putStr . dispf 2

n = 10
m = 2

pixNoise = 0.1
pixFactor = 640/2
sigma = pixNoise / pixFactor
seeds = [5555555,77777,99999,22222]

prob = genProb n m sigma seeds

--p1:p2:_ = map (toLists . asMatrix .(~>"nv")) $ flip parts "c" $ inhomogT "v" $ p2d prob

p3D = toLists $ asMatrix $ (4*) $ inhomogT "x" $ p3d prob
c1 = syntheticCamera $  easyCamera (90*degree) (0,0,-5) (0,0,1) 0
c2 = syntheticCamera $  easyCamera (90*degree) (1,0,-2) (1,0,1) 0
p1 = ht c1 p3D
p2 = ht c2 p3D

hv = homog . fromList

inter [p1,p2] [q1,q2] = [p1,int,q1,int] where
    int = toList $ inHomog $ (hv p1 `cross` hv p2) `cross` (hv q1 `cross` hv q2)

main = prepare >> proc >> mainLoop

proc = do
    evWindow (p1,p2,ident 3) "5 points" (mpSize 20) (Just dispfun) (mouseGen acts kbdQuit)

triang [p,t,r] (p1,p2') = [a,b,c] where
    rot = rot3 r <> rot2 p <> rot1 t
    p2 = ht rot p2'
    ints = pairsWith inter (take 5 $ transpose [p1,p2])   
    a = last $ head ints
    b = last $ last ints
    c = last $ head (tail ints)
    
dispfun st = do
    (p1,p2',rot) <- get st
    let p2 = ht rot p2'
        p12 = transpose [p1,p2]
        allint = pairsWith inter p12
        ints = pairsWith inter (take 5 p12)
        a = last $ head ints
        b = last $ last ints
        c = last $ head (tail ints)
    print a
    pointCoordinates (mpSize 20)
    pointSize $= 3
    setColor 0.2 0.2 0.2
    renderPrimitive Lines $ mapM vertex (concat allint)
    setColor 0 0.5 0
    renderPrimitive Lines $ mapM vertex (concat p12)
    setColor 1 1 1
    renderPrimitive Lines $ mapM vertex ([a,b,a,c,b,c])
    setColor 1 0 0
    renderPrimitive Points $ mapM_ vertex p1
    setColor 0 0 1
    renderPrimitive Points $ mapM_ vertex p2
    --setColor 1 1 0
    --renderPrimitive Points $ mapM vertex (map last allint)
    --pointSize $= 5
    setColor 1 1 1
    --renderPrimitive Points $ mapM vertex ([a,b,c])
    
        
---------------------------------------------------------

acts = [((Char 'x'            ,   Down, modif             )  , \(p1,p2,_) -> (p1,p2, ident 3))
       ,((MouseButton WheelUp,   Down, modif {ctrl = Down}) , \(p1,p2,r) -> (p1,p2, rot1 ( 5*degree) <> r))
       ,((MouseButton WheelDown, Down, modif {ctrl = Down}) , \(p1,p2,r) -> (p1,p2, rot1 (-5*degree) <> r))
       ,((MouseButton WheelUp,   Down, modif {alt = Down})  , \(p1,p2,r) -> (p1,p2, rot2 ( 5*degree) <> r))
       ,((MouseButton WheelDown, Down, modif {alt = Down})  , \(p1,p2,r) -> (p1,p2, rot2 (-5*degree) <> r))
       ,((MouseButton WheelUp,   Down, modif)               , \(p1,p2,r) -> (p1,p2, rot3 ( 5*degree) <> r))
       ,((MouseButton WheelDown, Down, modif)               , \(p1,p2,r) -> (p1,p2, rot3 (-5*degree) <> r))
       ,((MouseButton WheelUp,   Down, modif {ctrl = Down, shift = Down}) , \(p1,p2,r) -> (p1,p2, rot1 ( 0.1*degree) <> r))
       ,((MouseButton WheelDown, Down, modif {ctrl = Down, shift = Down}) , \(p1,p2,r) -> (p1,p2, rot1 (-0.1*degree) <> r))
       ,((MouseButton WheelUp,   Down, modif {alt = Down, shift = Down})  , \(p1,p2,r) -> (p1,p2, rot2 ( 0.1*degree) <> r))
       ,((MouseButton WheelDown, Down, modif {alt = Down, shift = Down})  , \(p1,p2,r) -> (p1,p2, rot2 (-0.1*degree) <> r))
       ,((MouseButton WheelUp,   Down, modif {shift = Down})               , \(p1,p2,r) -> (p1,p2, rot3 ( 0.1*degree) <> r))
       ,((MouseButton WheelDown, Down, modif {shift = Down})               , \(p1,p2,r) -> (p1,p2, rot3 (-0.1*degree) <> r))]

