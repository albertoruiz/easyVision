-----------------------------------------------------------------------------
{- |
Module      :  EasyVision.MiniApss.CornerTracker
Copyright   :  (c) Alberto Ruiz 2009
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)

-}
-----------------------------------------------------------------------------

module EasyVision.MiniApps.CornerTracker (
    cornerTracker
) where

import ImagProc
import Features.Matching
import EasyVision.Combinators
import EasyVision.Util
import EasyVision.Draw
import EasyVision.GUI
import EasyVision.Parameters
import EasyVision.Concurrent
import Graphics.UI.GLUT hiding (Point)
import Control.Monad(when)
import Vision
import Numeric.LinearAlgebra

cornerTracker cam = do
    w<- evWindow ([],ident 3) "Tracker" (mpSize 20) Nothing  (const kbdQuit) -- (mouse (kbdcam ctrl))
    o <- createParameters' "Tracker" [("err",realParam 0.01 0 0.1)
                           ]
    return $ do
        err <- getParam o "err"
        (img,pts') <- cam
        (prevpts,h) <- getW w
        let pts = pixelsToPoints (size img) pts'
            predicted = map f prevpts
                where f as = htp1 h (head as) : as

        let (g1',g2,b1,b2,_) = basicMatches' (predicted, pts) distPathPoint err
            g1 = map tail g1'
            matches = zip g1 g2
            ok = length prevpts > 0 && length pts > 0

        let f1 (a,b) = b:a
            f2 p     = [p]
            reset = map f2 pts
            add   = map f1 matches ++ map f2 b2
            new = if ok then add else reset
            hnew = if ok && length matches > 4
                then estimateHomographyRaw (map pl g2) (map (pl.head) g1)
                else ident 3

        putW w (new,hnew)
        inWin w $ do
                drawImage' img
                pointCoordinates (size img)
                setColor 1 1 1
                mapM_ shPath prevpts
                text2D 0.9 0.7 $ show $ length $ prevpts

                pointSize $= 5
                when ok $ do
                    setColor 1 0 0
                    mapM_ shMatch matches
                    renderPrimitive Points $ mapM_ vertex g2
                    setColor 0 0 1
                    renderPrimitive Points $ mapM_ vertex b2
                    pointSize $=3
                    setColor 0 1 0
                    renderPrimitive Points $ mapM_ vertex (map head g1')
                    setColor 1 1 1
                    text2D 0.9 0.65 $ show $ length $ matches

        return (img,new)


distPathPoint [] _    = 1000
distPathPoint (p:_) q = distPoints p q

shMatch (a:_,b) = do
    renderPrimitive Lines $ mapM_ vertex [a,b]

shPath p = renderPrimitive LineStrip $ mapM_ vertex p

pl (Point x y) = [x,y]
lp [x,y] = Point x y
htp h = map lp . ht h . map pl
htp1 h = lp . head . ht h . return . pl