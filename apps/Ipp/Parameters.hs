-----------------------------------------------------------------------------
{- |
Module      :  Ipp.Parameters
Copyright   :  (c) Alberto Ruiz 2006
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  very provisional
Portability :  hmm...

A parameter list with its own window. See the example wrapper.hs.

-}
-----------------------------------------------------------------------------

module Ipp.Parameters (
    createParameters
) where

import Ipp.Core
import Ipp.HEasyVision
import Graphics.UI.GLUT hiding (RGB, Matrix, Size)
import qualified Graphics.UI.GLUT as GL
import Data.IORef
import qualified Data.Map as Map
import Data.Map

sizePar = 35

-- | Given an assoc list of names and initial values of some \'global\' application parameters, it creates a window for controlling them and returns a function to get the current value of the desired parameter. Currently the parameters are Int between 0 and 100 (percents).
createParameters :: [(String, Int)]
                 -> IORef (State userState)
                 -> IO (String -> IO Int)
createParameters ops st = do
    o <- newIORef (Map.fromList ops)
    w <- addWindow "Parameters" (Size (2+length ops * sizePar) 200)
                                (Just (f o))
                                (const $ kbdopts o k)
                                st
    return (giveme o)
 where k _ _ _ _ = return ()
       f o s = do
           m <- readIORef o
           let els = Map.elems m
           pixelCoordinates (Size (2+length els * sizePar) 200)

           sequence_ $ zipWith3 bar [0..] els (keys m) 
           return () 
       bar p k s = do
           setColor 0 0 0.5
           renderPrimitive Polygon (mapM_ vertex [Pixel r1 c1,
                                                         Pixel r1 c2,
                                                         Pixel r2 c2,
                                                         Pixel r2 c1])
           setColor 1 1 1
           rasterPos (Vertex2 (5::GLfloat) (4+fromIntegral r1/2+fromIntegral r2/2))
           renderString Helvetica12 (s++" = "++show k++"%")

        where   r1 = 2+p*sizePar
                r2 = 2+p*sizePar+(sizePar -2)
                c1 = 1
                c2 = 2*k

kbdopts opts def = kbd where
    kbd (MouseButton WheelUp) _ _ pos@(Position x y) = do
        m <- readIORef opts
        let s' = keys m
        let s = (s' !! (fromIntegral y `div` sizePar))
        let v = m!s 
        let m' = insert s (min 100 (v+1)) m
        writeIORef opts m'
        postRedisplay Nothing
    kbd (MouseButton WheelDown) _ _ pos@(Position x y) = do
        m <- readIORef opts
        let s' = keys m
        let s = (s' !! (fromIntegral y `div` sizePar))
        let v = m!s 
        let m' = insert s (max 0 (v-1)) m
        writeIORef opts m'
        postRedisplay Nothing
    kbd (MouseButton LeftButton) Down _ pos@(Position x y) = do
        m <- readIORef opts
        let s' = keys m
        let s = (s' !! (fromIntegral y `div` sizePar))
        let v = m!s 
        let m' = insert s (fromIntegral x `div` 2) m
        writeIORef opts m'
        postRedisplay Nothing
    kbd a b c d = def a b c d

giveme opts val = do
    m <- readIORef opts
    return (m!val)
