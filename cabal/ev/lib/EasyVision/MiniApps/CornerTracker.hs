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
import EasyVision.MiniApps.Combinators
import EasyVision.GUI
import EasyVision.MiniApps.Concurrent
import Graphics.UI.GLUT hiding (Point)
import Control.Monad(when)
import Control.Arrow((&&&))
import Vision
import Numeric.LinearAlgebra
import Data.List
import Data.IORef
--import HUBIGraph as G
--import Control.Monad.Reader (runReaderT)


accept = 100

--newVertex' n = do newVertexWithID n
    --                  vertexLabel n $ show n
--                  vertexShape n G.Sphere
                      --vertexShapedetail n 8

cornerTracker mintrk cam = do
    w<- evWindow ([],[],ident 3) "Tracker" (mpSize 20) Nothing  (const kbdQuit) -- (mouse (kbdcam ctrl))
    o <- createParameters' "Tracker" [("err",realParam 0.01 0 0.1)
                           ]
--     s <- initHUBIGraph "http://127.0.0.1:20738/RPC2"
--     let ubiadd f = runReaderT f s
--     ubiadd (G.clear)

    pnf <- newIORef 0

    return $ do
        err <- getParam o "err"
        (img,pts') <- cam
        (prevpts,_,h) <- getW w
        nf <- readIORef pnf
        pnf $~ (+1)
        let pts = pixelsToPoints (size img) pts'
            predicted = map f $ prevpts
                where f as = htp1 h (head as) : as
            --(visible,novisible) = partition (inImage.head) predicted

        let (g1',g2,b1',b2,_) = basicMatches' (predicted, pts) distPathPoint err
            g1 = map tail g1'
            b1 = filter ((>accept).length) (map tail b1')
            matches = zip g1 g2
            ok = length prevpts > 0 && length pts > 0

        let f1 (a,b) = b:a
            f2 p     = [p]
            reset = map f2 pts
            add   = map f1 matches ++ map f2 b2 -- ++ lostpredicted
            new' = if ok then add else reset
            new = sortBy (compare `on` (negate.length)) new'
            hnew = if ok && length matches > 4
                then -- fst $ estimateHomographyRansac 0.6 0.01 (map pl g2) (map (pl.head) g1)
                     estimateHomographyRaw (map pl g2) (map (pl.head) g1)
                else ident 3
            lostpredicted = map f (b1)
                where f as = htp1 hnew (head as) : as

        putW w (new,b1,hnew)


--        ubiadd (newVertex' nf)
--        ubiadd (newEdge (nf,nf-1))

        let bh = snd . head . snd . span ((<mintrk).fst) $ infoTracks new
            bn = nf-bh+1

            okt = filter ((>=bh).length) new
            as = map head okt
            bs = map (last.take bh) okt
            h = estimateHomographyRaw (map pl bs) (map pl as)

--        print (length okt)
--        print (map length okt)
--        print h

        inWin w $ do
                drawImage' img
                pointCoordinates (size img)
                setColor 1 1 1
                mapM_ shPath prevpts
                text2D 0.9 0.7 $ show $ length $ prevpts
                text2D 0.9 0.6 $ show $ take 10 $  map (length &&& head) $ group $ map length new
                text2D 0.9 0.5 $ show $ take 10 $ infoTracks new
                --text2D 0.9 0.6 $ show $ bh

                --ubiadd (newEdge (nf, bn))
                --ubiadd (vertexLabel bn $ show bn)
                text2D 0.9 0.4 $ "link to " ++ show bn
                text2D 0.9 0.3 $ show $ reverse $ sort $ map length new

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
                    setColor 1 1 0
                    renderPrimitive Points $ mapM_ vertex (map head lostpredicted)
                    setColor 1 1 1
                    --text2D 0.9 0.65 $ show $ length $ matches
                    text2D 0.9 0.2 $ show $ map length b1

        return ((img,new),(nf,bn,h))


distPathPoint [] _    = 1000
distPathPoint (p:_) q = distPoints p q

shMatch (a:_,b) = do
    renderPrimitive Lines $ mapM_ vertex [a,b]

shPath p = renderPrimitive LineStrip $ mapM_ vertex p

pl (Point x y) = [x,y]
lp [x,y] = Point x y
htp h = map lp . ht h . map pl
htp1 h = lp . head . ht h . return . pl

infoTracks = ac . map (length &&& head) . group . map length

ac l = zip (scanl1 (+) c) m where (c,m) = unzip l

posMax l = k where
    Just k = elemIndex (maximum l) l

inImage (Point x y) = abs x < 1 && abs y < 0.75
