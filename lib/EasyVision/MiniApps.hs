-----------------------------------------------------------------------------
{- |
Module      :  EasyVision.MiniApss
Copyright   :  (c) Alberto Ruiz 2007
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  very provisional
Portability :  hmm...

Useful windows with associated behaviour.

-}
-----------------------------------------------------------------------------

module EasyVision.MiniApps (
    readCatalog,
    getCatalog,
    catalogBrowser,
    hsvPalette,
    scatterWindow,
    regionDetector, regionTracker
)where

import Graphics.UI.GLUT as GL hiding (Size,Point,Matrix,matrix)
import EasyVision.GUI hiding (State)
import EasyVision.Parameters
import ImagProc
import Data.List(transpose)
import Control.Monad(when)
import ImagProc.Ipp.Core
import Foreign.C.Types(CUChar)
import Foreign
import qualified Data.Map as Map
import Data.List(sort,nub,sortBy)
import EasyVision.Util
import Numeric.LinearAlgebra
import Classifier.Base(group)
import Data.IORef

-- | reads a labeled video
readCatalog :: String -> Size -> String -> Maybe Int -> (ImageYUV-> a) -> IO [(a,String)]
readCatalog video sz classesfile mbn prepro = do
    cam <- mplayer (video++" -benchmark") sz
    rawclasses <- readFile classesfile
    let classfilelines = lines rawclasses
        effectivelines = case mbn of
            Nothing -> classfilelines
            Just n  -> take n classfilelines
    let n = length effectivelines
        words' s = let (n:ws) = words s in [n , unwords ws]
    let [frames, classes] = transpose $ map words' effectivelines
    when (map read frames /= [1..n]) $ error ("inconsistent file "++ classesfile)
    imgs <- sequence (replicate n cam)
    return (zip (map prepro imgs) classes)

----------------------------------------------------------

classMap :: [[String]] -> String -> String
classMap lls = if null lls then id else search
    where f l@(h:_) = [(e,h)| e <- l]
          m = Map.fromList $ concatMap f lls
          search v = case Map.lookup v m of
                        Nothing -> v
                        Just c  -> c

-- | higher level version of getCatalog allowing for --group and --desired
getCatalog :: String -> Size -> String -> Maybe Int -> (ImageYUV-> a) -> IO [(a,String)]
getCatalog name sz lbs mbn feat = do
    dat <- readCatalog name sz lbs mbn feat

    group <- fmap classMap $ getOption "--group" []

    desi <- getFlag "desired"

    desired <- if not desi
                then fmap concat $ getOption "--group" []
                else getOption "--desired" []

    let okclasses = [(img, group cl) | (img,cl) <- dat, cl /= "?", desired == [] || cl `elem` desired]

    putStr "Valid images: "
    print (length okclasses)
    putStr "Classes found: "
    print (sort $ nub $ map snd okclasses)
    return okclasses


-- | to do (ImageYUV???)
catalogBrowser :: Int -> [(ImageYUV, String)] -> String -> Size -> IO (EVWindow (Int, [(ImageYUV, String)]))
catalogBrowser n catalog name sz =
    evWindow (n-1,catalog) name sz (Just disp) (mouse $ kbdQuit)
  where
    disp st = do
        (k,catalog) <- get st
        let (img,label) = catalog!!k
        drawImage img
        windowTitle $= name++" #"++show (k+1)++ ": "++label
    mouse _ st (MouseButton WheelUp) Down _ _ = do
        (k,catalog) <- get st
        st $= (min (k+1) (length catalog -1), catalog)
        postRedisplay Nothing
    mouse _ st (MouseButton WheelDown) Down _ _ = do
        (k,catalog) <- get st
        st $= (max (k-1) 0, catalog)
        postRedisplay Nothing
    mouse def _ a b c d = def a b c d

-------------------------------------------------------------------------------------

hsvPalette :: IO (EVWindow (CUChar, CUChar, CUChar))
hsvPalette = evWindow (128,128,255) "HSV" (Size 256 256) (Just disp) (mouse kbdQuit)
  where
    disp st = do
        (r',c',k) <- get st
        drawImage =<< palette k
        pixelCoordinates (Size 256 256)
        setColor 0 0 0
        let r = fromIntegral r'
            c = fromIntegral c'
        renderPrimitive LineLoop $ mapM_ vertex
            [Pixel (r-2) (c-2), Pixel (r-2) (c+2), Pixel (r+2) (c+2), Pixel (r+2) (c-2)]
        text2D 20 20 (show (c,r,k))
    mouse _ st (MouseButton WheelUp) Down m _ = do
        (r,c,k) <- get st
        st $= (r,c,k + if GL.shift m == Down then 10 else 1)
        postRedisplay Nothing
    mouse _ st (MouseButton WheelDown) Down m _ = do
        (r,c,k) <- get st
        st $= (r,c,k- if GL.shift m == Down then 10 else 1)
        postRedisplay Nothing
    mouse _ st (MouseButton LeftButton) Down m (Position x y) = do
        (_,_,k) <- get st
        st $= (fromIntegral y, fromIntegral x,k)
        postRedisplay Nothing
    mouse def _ a b c d = def a b c d


palette k = hsvToRGB $ putChannels (fromLists ramp ,
                                    fromLists (transpose ramp),
                                    constImage k (Size 256 256))
    where
        ramp = replicate 256 [0..255]

        fromLists ls = unsafePerformIO $ do
            r <- image (Size 256 256)
            setData8u r ls
            return r

---------------------------------------------------------------------------

scatter examples (i,j) = do
    let (gs,lbs) = group examples
        plot = map (\v-> Point (v@>i) (v@>j))
        xs = map ((@>i).fst) examples
        ys = map ((@>j).fst) examples
        a1 = minimum xs
        a2 = maximum xs
        b1 = minimum ys
        b2 = maximum ys
        da = 0.05*(a2-a1)
        db = 0.05*(b2-b1)
        colors = [setColor 1 0 0, setColor 0 0 1, setColor 0 1 0] ++
                 [setColor 1 1 0, setColor 0 1 1, setColor 1 0 1] ++
                 [setColor 1 0.5 0.5, setColor 0.5 0.5 1, setColor 0.5 1 0.5] ++
                 repeat (setColor 1 1 1)
    clear [ColorBuffer]
    matrixMode $= Projection
    loadIdentity
    ortho2D (a1-da) (a2+da) (b1-db) (b2+db)
    matrixMode $= Modelview 0
    loadIdentity
    let f pts col = do
            col
            GL.renderPrimitive GL.Points . mapM_ GL.vertex . plot $ pts

    pointSize $= 3
    sequence_ $ zipWith f gs colors

    let text2D x y s = do
        rasterPos (Vertex2 x (y::GLdouble))
        renderString Helvetica12 s

    setColor 0.5 0.5 0.5
    text2D a2 b1 (show i)
    text2D a1 b2 (show j)


scatterWindow name sz exs coor  = do
    w <- evWindow coor name sz (Just disp) kbd
    return w
  where n = dim . fst . head $ exs
        disp rdesi = do
            coord <- get rdesi
            scatter exs coord

        kbd rdesi (SpecialKey KeyUp) Down _ _ = do
            (i,j) <- get rdesi
            rdesi $= (i,(j+1) `mod` n)
            postRedisplay Nothing
        kbd rdesi (SpecialKey KeyDown) Down _ _ = do
            (i,j) <- get rdesi
            rdesi $= (i, (j-1) `mod`n)
            postRedisplay Nothing
        kbd rdesi (SpecialKey KeyRight) Down _ _ = do
            (i,j) <- get rdesi
            rdesi $= ((i+1)`mod`n,j)
            postRedisplay Nothing
        kbd rdesi (SpecialKey KeyLeft) Down _ _ = do
            (i,j) <- get rdesi
            rdesi $= ((i-1) `mod` n,j)
            postRedisplay Nothing
        kbd _ a b c d = kbdQuit a b c d

-----------------------------------------------------------------------

getIntegral o name = fromIntegral `fmap` (getParam o name :: IO Int)

regionDetector :: String -> IO Channels -> IO (IO (Channels, Maybe (Point,[Point])))
regionDetector "" cam = do
    o <- createParameters [("kb",intParam 60  0 255),
                           ("kg",intParam 100 0 255),
                           ("kw",intParam 200 0 255),
                           ("c1",intParam 7 1 9),
                           ("c2",intParam 8 1 9)]
    return $ do
        kb <- getIntegral o "kb"
        kg <- getIntegral o "kg"
        kw <- getIntegral o "kw"
        c1 <- getIntegral o "c1"
        c2 <- getIntegral o "c2"
        orig <- cam
        let col = detectRange c1 c2 . hsvCode kb kg kw . hsv $ orig
            rawconts = map (pixelsToPoints (size col). fst3) $
                       sortBy (compare `on` (negate.fst2)) $
                       contours 5 200 1 True col
            conts = map momentsContour rawconts
            p = case conts of
                    [] -> Nothing
                    (x,y,_,_,_):_ -> Just (Point x y, head rawconts)
        return (orig, p)


regionDetector name cam = do
    e <- evWindow () name (mpSize 8) Nothing (const kbdQuit)
    cam' <- regionDetector "" cam
    return $ do
        (orig,pc) <- cam'
        inWin e $ do
            drawImage (rgb orig)
            setColor 1 1 1
            pointSize $= 5
            pointCoordinates (size (rgb orig))
            case pc of
                Just (p,cont) -> do
                    renderPrimitive Points $ mapM_ vertex [p]
                    renderPrimitive LineLoop $ mapM_ vertex cont
                Nothing -> return ()
        return (orig, pc)

detectRange a b im = purifyWith (set8u 0) $
    thresholdVal8u a 0 IppCmpLess im >>=
    thresholdVal8u b 0 IppCmpGreater

fst3 (a,_,_) = a
fst2 (_,a,_) = a

----------------------------------------------------------------

-- | to do
regionTracker :: String -> IO (Channels,Maybe (Point,[Point])) -> IO (IO (Channels, (Point,(Double,Double))))
regionTracker "" detector = do
    r <- newIORef s0
    return $ do
        (orig,p) <- detector
        st <- get r
        let st'@(State x c) =
                case p of
                    Nothing -> blind sys st
                    Just (Point x y,_)  -> kalman sys st (vector [x, y])
        r $= st'
        let pt = Point (x@>0) (x@>1)
            v = (x@>2,x@>3)
        return (orig, (pt,v))

regionTracker name detector = do
    det <- regionTracker "" detector
    e <- evWindow () name (mpSize 8) Nothing (const kbdQuit)
    return $ do
        (orig, (pt@(Point x y),v@(vx,vy))) <- det
        let pt2 = Point (x+vx) (y+vy)
        inWin e $ do
            drawImage (rgb orig)
            setColor 1 1 1
            pointSize $= 5
            pointCoordinates (size (rgb orig))
            renderPrimitive Points $ mapM_ vertex [pt]
            setColor 1 0 0
            lineWidth $= 2
            renderPrimitive Lines $ mapM_ vertex [pt,pt2]
        return (orig, (pt,v))

-- Kalman filter for 2D position and velocity

vector l = fromList l :: Vector Double
matrix ls = fromLists ls :: Matrix Double
diagl = diag . vector

f = matrix [[1,0,1,0],
            [0,1,0,1],
            [0,0,1,0],
            [0,0,0,1]]

h = matrix [[1,0,0,0],
            [0,1,0,0]]

q = 1 * diagl [1,1,1,1]

r = 10 * diagl [1,1]

s0 = State (vector [0, 0, 0, 0]) (diagl [1, 1, 1, 1])

sys = System f h q r

data System = System {kF, kH, kQ, kR :: Matrix Double}
data State = State {sX :: Vector Double , sP :: Matrix Double} deriving Show
type Measurement = Vector Double

kalman :: System -> State -> Measurement -> State
kalman (System f h q r) (State x p) z = State x' p' where
    px = f <> x                            -- prediction
    pq = f <> p <> trans f + q             -- its covariance
    y  = z - h <> px                       -- residue
    cy = h <> pq <> trans h + r            -- its covariance
    k  = pq <> trans h <> inv cy           -- kalman gain
    x' = px + k <> y                       -- new state
    p' = (ident (dim x) - k <> h) <> pq    -- its covariance


blind :: System -> State -> State
blind (System f h q r) (State x p) = State x' p' where
    x' = f <> x
    p' = f <> p <> trans f + q
