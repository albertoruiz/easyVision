module Tools(
    -- * Invariant features
    toCanonic,
    protos, protoOri,
    foufeat, toFun,
    icaConts, icaConts',
    -- * Display
    shapeCatalog,
    examplesBrowser,
    shcont, shcontO
) where


import EasyVision as EV
import Graphics.UI.GLUT hiding (Point)
import Util.Misc(diagl,degree,Vec,norm,debug,diagl,memo,Mat,mat,norm)
import Util.Rotation(rot3)
import Control.Arrow
import Control.Applicative((<$>),(<*>))
import Control.Monad(when,ap)
import Data.Colour.Names as Col
import Vision(desp, estimateHomography)
import Numeric.LinearAlgebra -- ((<>),fromList,toList,fromComplex,join,dim)
import Data.List(sortBy,minimumBy,groupBy,sort,zipWith4)
import Data.Function(on)
import Text.Printf(printf)
import Data.IORef
import Util.Options

import Data.Complex
import Classifier(Sample)
import Data.Maybe(isJust)

----------------------------------------------------------------------
----------------------------------------------------------------------


-- | smoothed frequential invariant representations
toCanonic :: Polyline -> [(Mat,Vec)]
toCanonic p = zip (map (<>w) cs) cps
  where
    w = rot3 (20*degree) <> whitener p
    wp = transPol w p
    cs = map rot3 (icaAngles' wp)
    cps = map (foufeat .flip transPol wp) cs

protoOri :: Sample Polyline -> Sample Polyline
protoOri = concatMap (repSample . (f *** id))
  where
    f = icaConts'' . whitenContour . transPol (rot3 (10*degree)) -- hmm
    repSample (xs,l) = map (id &&& const l) xs


protos ::  Sample Polyline -> Sample Vec
protos = map (foufeat *** id)


foufeat = prec 10 .  normalizeStart . fourierPL


prec n f = join[r,c]
    where 
      (r,c) = fromComplex $ fromList $ map f ([-n..n])


toFun cb k = cb@>(k+n) :+ cb@>(k+3*n+1)
    where n = (dim cb - 2) `div` 4

----------------------------------------------------------------------

icaConts w = map g (icaAngles w)
  where
    g a = transPol (rot3 a) w

icaConts' w = map g (icaAngles' w)
  where
    g a = transPol (rot3 a) w

icaConts'' w = map g (icaAngles'' w)
  where
    g a = transPol (rot3 a) w

icaAngles' w = as ++ map (+pi) as
  where as = icaAngles w

icaAngles'' w = [head as, last as]
  where as = icaAngles w

----------------------------------------------------------------------
----------------------------------------------------------------------

shapeCatalog gprot feat cam = do
    raw <- gprot
    let prototypes = feat raw
        sz = mpSize 10
    w <- evWindow (raw,prototypes,Nothing,False) "Contour Selection" sz Nothing (marker kbdQuit)
    s <- shapesBrowser "Prototypes" (mpSize 10) raw
    return $ do
        (raw',prots',click,save) <- getW w
        (im,cs) <- cam
        let (raw,prots) = case click of
                Nothing -> (raw',prots')
                Just pix  -> let [pt] = pixelsToPoints sz [pix]
                                 newc = (closestTo pt cs, "?" ++ show (length prots' + 1))
                           in (newc:raw', feat [newc] ++ prots')
        when (isJust click) $ putW s (0, raw) >> postRedisplay (Just (evW s))
        inWin w $ do
            drawImage im
            pointCoordinates sz
            text2D 0.9 0.6 $ show (length cs)
            mapM_ shcont cs
            
        when save $ writeFile "shapes.txt" (show raw)
        putW w (raw,prots,Nothing,False)
        return (im,cs,prots)
  where
    marker _ st (MouseButton LeftButton) Down _ pos@(Position x y) = do
        (r,p,_,_) <- readIORef st
        let clicked = Pixel (fromIntegral y) (fromIntegral x)
        writeIORef st (r,p, Just clicked, False)

    marker _ st (Char 's') Down _ _ = do
        (r,p,_,_) <- readIORef st
        writeIORef st (r,p, Nothing, True)

    marker def _ a b c d = def a b c d

    closestTo pt = minimumBy (compare `on` (d pt))
      where
        d p c = distPoints p (cen c)
        cen (Closed c) = Point cx cy where (cx,cy,_,_,_) = momentsContour c


shapesBrowser :: String -> EV.Size -> Sample Polyline -> IO (EVWindow (Int, Sample Polyline))
shapesBrowser name sz = examplesBrowser name sz f
  where
    f = shcont . transPol (diagl[-0.2,0.2,1]) . whitenContour


examplesBrowser :: String -> EV.Size -> (t -> IO a) -> Sample t -> IO (EVWindow (Int, Sample t))
examplesBrowser name sz f exs =
    evWindow (0,exs) name sz (Just disp) (mouseGen acts kbdQuit)
  where
    disp st = do
        (k,exs) <- get st
        when (not $ null exs) $ do 
            let (x,label) = exs!!k
            f x
            windowTitle $= name++" #"++show (k+1)++ ": "++label
    acts = [((MouseButton WheelUp,   Down, modif), \_ (k,exs) -> (min (k+1) (length exs - 1), exs))
           ,((MouseButton WheelDown, Down, modif), \_ (k,exs) -> (max (k-1) 0, exs))]

----------------------------------------------------------------------

shcont (Closed c) = do
    renderPrimitive LineLoop $ mapM_ vertex c
--    renderPrimitive Points $ vertex (head c)
shcont (Open c) = do
    renderPrimitive LineStrip $ mapM_ vertex c

shcontO (Closed c) = do
    pointCoordinates (EV.Size 500 500)
    renderPrimitive LineLoop $ mapM_ vertex c
    pointSize $= 6
    renderPrimitive Points $ vertex (c!!0)
    pointSize $= 4
    renderPrimitive Points $ vertex (c!!1)
    --print c

----------------------------------------------------------------------

