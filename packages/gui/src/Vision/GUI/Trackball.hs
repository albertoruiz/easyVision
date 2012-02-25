module Vision.GUI.Trackball (newTrackball) where

import Vision.GUI.Types
import Util.Quaternion
import Util.Homogeneous(cross)
import Numeric.LinearAlgebra
import Graphics.UI.GLUT hiding (normalize, Matrix, matrix)
import Data.IORef

vector v = fromList  v :: Vector Double
norm x = pnorm PNorm2 x


-- adapted from tracball.h and trackball.c 
-- A virtual trackball implementation
-- Written by Gavin Bell for Silicon Graphics, November 1988.

projectToSphere r (x,y) = z where
    d = sqrt $ x^2 + y^2
    t = r / sqrt 2
    z = if d < r * 0.707
        then sqrt (r^2 - d^2)   -- inside sphere
        else t*t / d            -- on hyperbola

-------------------------------------------------------

{-
/*
 * Ok, simulate a track-ball.  Project the points onto the virtual
 * trackball, then figure out the axis of rotation, which is the cross
 * product of P1 P2 and O P1 (O is the center of the ball, 0,0,0)
 * Note:  This is a deformed trackball-- is a trackball in the center,
 * but is deformed into a hyperbolic sheet of rotation away from the
 * center.  This particular function was chosen after trying out
 * several variations.
 *
 * It is assumed that the arguments to this routine are in the range
 * (-1.0 ... 1.0)
 */
-}

trackball (x',y') (x,y)
    | x'==x && y'==y = Quat { qs = 1.0, qv = vector [0,0,0]}  -- zero rotation
    | otherwise = axisToQuat phi axis
  where
    trackballSize = 0.8

    axis = cross p1 p2
    p1 = vector [x',y',projectToSphere trackballSize (x',y')]
    p2 = vector [x, y, projectToSphere trackballSize (x ,y )]

    phi = 2 * asin t
    t' = norm (p1-p2) / (2*trackballSize)
    t = max (-1) (min 1 t')

----------------------------------------------------------------------

data TrackballState = TBST {tbQuat :: Quaternion, prev :: [Double],
                            dist :: Double, wsize :: Double, vertAngle:: Double,
                            autoSpeed :: Int }


{- | This function creates a virtual trackball to control the viewpoint in a 3D window. It returns an @IO()@ which sets the appropriate rotation in the current window and the keyboard and mouse movement callbacks which controls the trackball. The keyboard callback admits a default callback. See usage examples in @pose.hs@ and @frontal.hs@.
-}
newTrackball :: IO ( (IO(), (t -> KeyboardMouseCallback) -> (t -> KeyboardMouseCallback), MotionCallback, IO Bool))
newTrackball = do
    st <- newIORef TBST { tbQuat = Quat {qs = 1.0, qv = vector [0,0,0]},
                          prev = [], dist = 20, wsize = 400, vertAngle = 0,
                          autoSpeed = 0 }
    let trackball = do
            s <- readIORef st
            let rot = map doubleGL $ getRotationHL (tbQuat s)
            mat <- newMatrix RowMajor rot :: IO (GLmatrix GLdouble)
            matrixMode $= Projection
            loadIdentity
            perspective 40 1 1 100
            lookAt (Vertex3 0 0 (doubleGL $ dist s))
                   (Vertex3 0 0 0)
                   (Vector3 0 1 0)
            matrixMode $= Modelview 0
            loadIdentity
            multMatrix mat
            rotate (doubleGL $ vertAngle s) (Vector3 0 0 (1::GLdouble))
            Size sz _ <- get windowSize
            writeIORef st s {wsize = fromIntegral sz}
            postRedisplay Nothing

    return (trackball, quatkbd st, quatmot st, autoRot st)


quatkbd str _ _ (MouseButton LeftButton) Down _ pos@(Position x y) = do
    st <- readIORef str
    let sz = wsize st
    let sz2 = sz/2
    writeIORef str st { prev = [(fromIntegral x - sz2 )/ sz,
                               -(fromIntegral y - sz2) / sz]}

quatkbd str _ _ (Char 'o') Down _ _ = do
    modifyIORef str $ \s -> s { tbQuat = 1 }

quatkbd str _ _ (Char 'm') Down _ _ = do
    modifyIORef str $ \s -> s { autoSpeed = f (autoSpeed s) }
  where f 0 = 5
        f _ = 0

quatkbd st _ _ (MouseButton WheelUp) _ (Modifiers{shift=Down}) _ = do
    modifyIORef st $ \s -> s { vertAngle = vertAngle s + 1 }
quatkbd st _ _ (MouseButton WheelDown) _ (Modifiers{shift=Down}) _ = do
    modifyIORef st $ \s -> s { vertAngle = vertAngle s - 1 }
quatkbd st _ _ (SpecialKey KeyUp) _ (Modifiers{shift=Down}) _ = do
    modifyIORef st $ \s -> s { vertAngle = vertAngle s + 1 }
quatkbd st _ _ (SpecialKey KeyDown) _ (Modifiers{shift=Down}) _ = do
    modifyIORef st $ \s -> s { vertAngle = vertAngle s - 1 }


quatkbd st _ _ (MouseButton WheelUp) _ (Modifiers{ctrl=Down}) _ = do
    modifyIORef st $ \s -> s { dist = dist s /1.1}
quatkbd st _ _ (MouseButton WheelDown) _ (Modifiers{ctrl=Down}) _ = do
    modifyIORef st $ \s -> s { dist = dist s *1.1}
quatkbd st _ _ (SpecialKey KeyUp) _ (Modifiers{ctrl=Down}) _ = do
    modifyIORef st $ \s -> s { dist = dist s /1.1}
quatkbd st _ _ (SpecialKey KeyDown) _ (Modifiers{ctrl=Down}) _ = do
    modifyIORef st $ \s -> s { dist = dist s *1.1}


quatkbd _ k st b s m p = k st b s m p

quatmot str pos@(Position x y) = do
    st <- readIORef str
    let sz = wsize st
    let sz2 = sz/2
    let [xc,yc] = [(fromIntegral x - sz2 )/ sz,
                  -(fromIntegral y - sz2) / sz]
    case prev st of
        [] -> return ()
        [xp,yp] -> do
            let q = trackball (xp,yp) (xc,yc) .*. tbQuat st
            writeIORef str st {tbQuat = q, prev = [xc,yc]}

autoRot st = do
    s <- readIORef st
    writeIORef st $ s {vertAngle = vertAngle s + fromIntegral (autoSpeed s)/10}
    return (autoSpeed s /= 0)
