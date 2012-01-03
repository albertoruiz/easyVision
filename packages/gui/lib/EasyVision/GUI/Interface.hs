{-# LANGUAGE NoMonomorphismRestriction #-}

---------------------------------------------------------------------------
{- |
Module      :  EasyVision.GUI.Interface
Copyright   :  (c) Alberto Ruiz 2006-11
License     :  GPL

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional

User interface tools

-}
-----------------------------------------------------------------------------

module EasyVision.GUI.Interface (
    -- * Interface
    Interface, Command, WinInit, WinRegion, VC,
    runFPS, runIdle, runIt, interface, observe, sMonitor,
    -- * Tools
    prepare,
    evWindow, evWindow3D, evWin3D,
    launch, launchFreq,
    inWin, getW, putW, updateW, getROI, setEVROI,
    kbdcam, kbdQuit, keyAction, mouseGen, mouseGenPt, modif, withPause
) where

import EasyVision.GUI.Util
import EasyVision.GUI.Draw
import ImagProc.Base
import ImagProc.Ipp(Size(..),ippSetNumThreads,ROI(..),saveRGB')
import Graphics.UI.GLUT hiding (RGB, Matrix, Size, None, Point,color)
import qualified Graphics.UI.GLUT as GL
import Data.IORef
import System.Process(system)
import System.Exit
import Control.Monad(when)
import System.Environment(getArgs)
import qualified Data.Map as Map
import Data.Map
--import Util.Misc(debug)
import Data.Traversable
import Control.Applicative
import Control.Arrow
import Data.Colour.Names
import Contours.Base


keyAction upds acts def w a b c d = do
    st <- getW w
    gp <- unZoomPoint w
    roi <- get (evRegion w)
    case Prelude.lookup (a,b,c) upds of
        Just op -> putW w (withPoint op roi gp d st) >> postRedisplay Nothing
        Nothing -> case Prelude.lookup (a,b,c) acts of
                        Just op -> withPoint op roi gp d st
                        Nothing -> def a b c d
  where
    withPoint f roi gp pos = f roi (gp pos)

modif = Modifiers {ctrl = Up, shift = Up, alt = Up }

--------------------------------------------------------------------------------

type Interface s a b x y = Size -> String -> s 
                    -> WinInit s a -> [Command s s] -> [Command s (IO())]
                    -> Maybe (s->y)
                    -> (WinRegion -> s -> a -> (s,b))
                    -> (WinRegion -> s -> b -> x) 
                    -> VC a b

type Command state result = ((Key,KeyState,Modifiers), WinRegion -> Point -> state -> result)
type WinInit state input = EVWindow state -> input -> IO()
type WinRegion = Polyline
type PreCommand t state result = (t -> result) -> Command state result

type VC a b = IO a -> IO (IO b)

interface :: (Renderable x, Renderable y) => Interface s a b x y 
interface sz0 name st0 ft upds acts mbkeyDisp resultFun resultDisp cam = do
    (cam', ctrl) <- withPause cam
    firstTimeRef <- newIORef True
    w <- evWindow st0 name sz0 Nothing (keyAction upds acts (kbdcam ctrl))
    let draw = case mbkeyDisp of
            Nothing -> return ()
            Just fun -> do
                clear [ColorBuffer]
                st <- getW w
                renderIn w (fun st)
                swapBuffers
    displayCallback $= draw

    return $ do
        thing <- cam'
        firstTime <- readIORef firstTimeRef
        when firstTime $ ft w thing >> writeIORef firstTimeRef False
        state <- getW w
        roi <- get (evRegion w)
        let (newState, result) = resultFun roi state thing
        visible <- get (evVisible w)
        when visible $ do
            r <- get (evRegion w)
            inWin w (prepZoom w >> renderIn w (resultDisp roi newState result))
        putW w newState
        return result

sMonitor :: Renderable x => String -> (WinRegion -> b -> [x]) -> VC b b
sMonitor name f = interface (Size 240 360) name 0 (const.const.return $ ()) (c2 acts) [] nothingR (const (,)) g
  where
    g roi k x = r !! j
      where
        r = f roi x
        j = k `mod` length r
    acts = [((MouseButton WheelUp,   Down, modif), (+1))
           ,((SpecialKey  KeyUp,     Down, modif), (+1))
           ,((MouseButton WheelDown, Down, modif), pred)
           ,((SpecialKey  KeyDown,   Down, modif), pred)]
    c2 = Prelude.map (id *** const.const)


observe :: Renderable x => String -> (b -> x) -> VC b b
observe name f = interface (Size 240 360) name () (const.const.return $ ()) [] [] nothingR (const (,)) (const.const $ f)

-----------------------------------------------------------------

-- | Initializes the HOpenGL system.
prepare :: IO ()
prepare = do
    getArgsAndInitialize
    initialDisplayMode $= [DoubleBuffered, WithDepthBuffer]
    ippSetNumThreads 1
    return ()

-- | Starts the application with a worker function (idle callback).
launch :: IO () -> IO ()
launch worker = do
    idleCallback $= Just worker
    mainLoop

-- | Starts the application with a worker function which runs at the desired frequency (in Hz).
launchFreq :: Int -> IO () -> IO ()
launchFreq freq worker = do
    let callback = do
        addTimerCallback (1000 `div` freq) callback
        worker
    addTimerCallback 1000 callback
    mainLoop


runIdle :: IO (IO a) -> IO ()
runIdle c = prepare >> (c >>= launch . (>> return ()))

runFPS :: Int -> IO (IO a) -> IO ()
runFPS n c = prepare >> (c >>= launchFreq n . (>> return ()))

runIt f = prepare >> f >> mainLoop

----------------------------------------------------------------

irr = Closed [Point p p, Point n n]
  where p = 0.5; n = -0.5    

evWindow st0 name size mdisp kbd = do
    st <- newIORef st0
    glw <- createWindow name
    iconTitle $= name
    windowSize $= glSize size

    -- provisionally kept for compatibility
    let draw = case mdisp of
            Nothing -> return ()
            Just fun -> do
                clear [ColorBuffer]
                fun st
                swapBuffers
    displayCallback $= draw

    actionOnWindowClose $= ContinueExectuion

    let Size h w = size

    rr <- newIORef irr
        
    zd <- newIORef (1,0,0)
    ms <- newIORef None
    po <- newIORef StaticSize
    ps <- newIORef Nothing
    vi <- newIORef True

    let w = EVW { evW = glw
                , evSt = st
                , evRegion = rr
                , evZoom = zd
                , evMove = ms
                , evPolicy = po
                , evPrefSize = ps
                , evVisible = vi
                , evInit = clear [ColorBuffer] }

    keyboardMouseCallback $= Just (kbdroi w (kbd w))
    motionCallback $= Just (mvroi w)
    -- callback to detect minimization?
    return w

---------------------------------------------------------------

inWin w f = do
    saved <- get currentWindow
    currentWindow $= Just (evW w)
    evInit w
    f
    swapBuffers
    currentWindow $= saved

getW = get . evSt
putW w x = evSt w $= x
updateW w f = evSt w $~ f

----------------------------------------------------------------

evWindow3D ist name sz kbd = do
    (trackball,kc,mc,_) <- newTrackball
    w <- evWindow ist name (Size sz sz) Nothing (kc kbd)
    motionCallback $= Just mc
    depthFunc $= Just Less
    textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
    textureFunction $= Replace
    return w { evInit = clear [ColorBuffer, DepthBuffer] >> trackball}

-- provisional
evWin3D ist name sz mdisp kbd = do
    (trackball,kc,mc,auto) <- newTrackball
    w <- evWindow ist name (Size sz sz) Nothing (redik (kc kbd))
    motionCallback $= Just (redim mc)
    depthFunc $= Just Less
    textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
    textureFunction $= Replace
    let draw = case mdisp of
            Nothing -> return ()
            Just fun -> do
                clear [ColorBuffer, DepthBuffer] >> trackball
                fun (evSt w)
                swapBuffers
    displayCallback $= draw
    let callback = do
        addTimerCallback 50 callback
        ok <- auto
        when ok $ postRedisplay (Just (evW w))
    addTimerCallback 1000 callback
    return w { evInit = clear [ColorBuffer, DepthBuffer] >> trackball}


redim f p = f p >> postRedisplay Nothing
redik f a1 a2 a3 a4 a5 = f a1 a2 a3 a4 a5 >> postRedisplay Nothing

----------------------------------------------------------------

minroi = 20

nextPolicy UserSize = DynamicSize
nextPolicy StaticSize = UserSize
nextPolicy DynamicSize = UserSize


-- | keyboard callback for camera control and exiting the application with ESC. p or SPACE pauses, s sets frame by frame mode.
kbdcam :: (IO (),IO(),IO()) -> KeyboardMouseCallback
kbdcam (pauseC,stepC,passC) = kbd where
    kbd (Char ' ') Down Modifiers {shift=Up} _ = pauseC
    kbd (Char ' ') Down Modifiers {shift=Down} _ = passC
    kbd (Char 's') Down _ _ = stepC
    kbd a b c d = kbdQuit a b c d


-- | keyboard callback for exiting the application with ESC or q, useful as default callback.
-- Also, pressing i saves a screenshot of the full opengl window contents.
kbdQuit :: KeyboardMouseCallback
kbdQuit (Char '\27') Down Modifiers {shift=Down} _ = leaveMainLoop >> system "killall mplayer" >> return ()
kbdQuit (Char '\27') Down Modifiers {ctrl=Down} _ = exitWith ExitSuccess
kbdQuit (Char '\27') Down _ _ = leaveMainLoop
kbdQuit (Char   'i') Down _ _ = captureGL >>= saveRGB' Nothing
kbdQuit a Down m _            = putStrLn (show a ++ " " ++ show m ++ " not defined")
kbdQuit _ _ _ _               = return ()


kbdroi w _ (Char '0') Down Modifiers {alt=Down} _ = do
    mbsz <- readIORef (evPrefSize w)
    case mbsz of
        Nothing -> return ()
        Just (Size h w') -> writeIORef (evRegion w) irr

kbdroi w _ (MouseButton WheelUp) Down Modifiers {ctrl=Down} _ =
    modifyIORef (evZoom w) (\(z,x,y)->(z*1.1,x*1.1,y*1.1))
kbdroi w _ (MouseButton WheelDown) Down Modifiers {ctrl=Down} _ =
    modifyIORef (evZoom w) (\(z,x,y)->(z/1.1,x/1.1,y/1.1))

kbdroi w _ (SpecialKey KeyUp) Down Modifiers {ctrl=Down} _ =
    modifyIORef (evZoom w) (\(z,x,y)->(z*1.1,x*1.1,y*1.1))
kbdroi w _ (SpecialKey KeyDown) Down Modifiers {ctrl=Down} _ =
    modifyIORef (evZoom w) (\(z,x,y)->(z/1.1,x/1.1,y/1.1))


kbdroi w _ (MouseButton LeftButton) Down Modifiers {ctrl=Down} (Position x y) =
    writeIORef (evMove w) (MoveZoom x y)

kbdroi w _ (MouseButton RightButton) Down Modifiers {ctrl=Down} p = do
    gp <- unZoomPoint w
    let pt = gp p
    modifyIORef (evRegion w) $ \(Closed[_,b]) -> (Closed [pt,b])
    writeIORef (evMove w) SetROI

kbdroi w _ (MouseButton LeftButton) Up _ _ = writeIORef (evMove w) None
kbdroi w _ (MouseButton RightButton) Up _ _ = writeIORef (evMove w) None

kbdroi w _ (SpecialKey KeyF3) Down Modifiers {ctrl=Down} _ = do
    vi <- get (evVisible w)
    if vi
        then writeIORef (evVisible w) False >> windowStatus $= Iconified
        else writeIORef (evVisible w) True

kbdroi w _ (SpecialKey KeyF3) Down _ _ = modifyIORef (evPolicy w) nextPolicy


kbdroi w _ (Char '0') Down Modifiers {ctrl=Down} _ = writeIORef (evZoom w) (1,0,0)

kbdroi _ defaultFunc a b c d = defaultFunc a b c d


mvroi w (Position x1' y1') = do
    ms <- readIORef (evMove w)
    z@(z0,_,dy) <- readIORef (evZoom w)
    gp <- unZoomPoint w
    let pt = gp (Position x1' y1') 
    case ms of
        None -> return ()
        SetROI -> modifyIORef (evRegion w) $ \(Closed[p,_]) -> Closed [p,pt]
        MoveZoom x0 y0 -> do
            modifyIORef (evZoom w) $
                \(z,x,y) -> (z, x+fromIntegral (x1'-x0), y-fromIntegral (y1'-y0))
            writeIORef (evMove w) (MoveZoom x1' y1')


unZoomPoint w = do
    z@(z0,_,dy) <- readIORef (evZoom w)
    vp <- get viewport
    Size wh ww <- evSize `fmap` get windowSize
    let f (Position x y) = pt
          where
            (x',y') = unZoom z vp (x,y) 
            [pt] = pixelsToPoints (Size (wh - round (4*dy/z0)) ww) [Pixel y' x'] 
    return f

--------------------------------------------------------------------------------

withPause :: IO a                          -- ^ original camera
          -> IO (IO a, (IO(), IO(), IO())) -- ^ camera and controller (pause, step, pass)
withPause grab = do
    paused <- newIORef False
    frozen <- newIORef undefined
    step   <- newIORef False
    pass   <- newIORef False

    let pauseC = do
            modifyIORef paused not
            p <- readIORef paused
            if p then grab >>= writeIORef frozen
                 else return ()

        stepC = modifyIORef step not
        passC = modifyIORef pass not

        control = (pauseC,stepC,passC)

    let virtual = do
        s <- readIORef step
        p <- readIORef paused
        g <- readIORef pass
        let grab' = if g then grab >> readIORef frozen else readIORef frozen
        if not s && p
             then grab'
             else 
                if s then if p then grab'
                               else do writeIORef paused True
                                       grab >>= writeIORef frozen
                                       readIORef frozen
             else grab

    return (virtual,control)

-----------------------------------------------------------------
-- (for compatibility, to be removed)

keyAction' g1 upds def w a b c d = do
    v <- getW w
    sz <- evSize `fmap` get windowSize
    case Prelude.lookup (a,b,c) upds of
        Just op -> putW w (g1 op sz d v) >> postRedisplay Nothing
        Nothing -> def a b c d

mouseGen acts = keyAction' (const) acts

mouseGenPt acts = keyAction' (withPoint') acts
  where
    withPoint' f sz (Position c r) = f p
      where
       [p] = pixelsToPoints sz [Pixel (fromIntegral r) (fromIntegral c)]


getROI w = do
    r <- get (evRegion w)
    sz <- evSize `fmap` get windowSize 
    return (poly2roi sz r)

setEVROI w r = do
    sz <- evSize `fmap` get windowSize
    let Closed [a,b,c,d] = roi2poly sz r
    (evRegion w) $= Closed [a,d]

-----------------------------------------------------------------

