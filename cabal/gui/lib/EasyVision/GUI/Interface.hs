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
  prepare,
  evWindow, evWindow3D, evWin3D, EVWindow(..)
, launch, launchFreq, runFPS, runIt, runIdle, interface, sMonitor, observe
, inWin, getW, putW, updateW, getROI
, kbdcam, kbdQuit, keyAction, mouseGen, mouseGenPt, modif, withPause
) where

import EasyVision.GUI.Util
import EasyVision.GUI.Draw
import ImagProc.Base
import ImagProc.Ipp(Size(..),ippSetNumThreads,ROI(..),saveRGB')
import Graphics.UI.GLUT hiding (RGB, Matrix, Size)
import qualified Graphics.UI.GLUT as GL
import Data.IORef
import System.Process(system)
import System.Exit
import Control.Monad(when)
import System.Environment(getArgs)
import qualified Data.Map as Map
import Data.Map
import EasyVision.GUI.Objects


-- | keyboard callback for camera control and exiting the application with ESC. p or SPACE pauses, s sets frame by frame mode.
kbdcam :: (IO (),IO(),IO()) -> KeyboardMouseCallback
kbdcam (pauseC,stepC,passC) = kbd where
    kbd (Char 'p') Down _ _ = pauseC
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
kbdQuit _ _ _ _               = return ()


keyAction g1 upds g2 acts def w a b c d = do
    v <- getW w
    sz <- evSize `fmap` get windowSize
    roi <- getROI w
    case Prelude.lookup (a,b,c) upds of
        Just op -> putW w (g1 op roi sz d v) >> postRedisplay Nothing
        Nothing -> case Prelude.lookup (a,b,c) acts of
                        Just op -> g2 op roi sz d v
                        Nothing -> def a b c d

withPoint f sz (Position c r) = f p
  where
   [p] = pixelsToPoints sz [Pixel (fromIntegral r) (fromIntegral c)]

modif = Modifiers {ctrl = Up, shift = Up, alt = Up }

mouseGen acts = keyAction (const.const) acts id []

mouseGenPt acts = keyAction (const.withPoint) acts id []

-----------------------------------------------------------------

interface sz0 name st0 g1 upds g2 acts mbkeyDisp resultFun resultDisp cam = do
    (cam', ctrl) <- withPause cam
    w <- evWindow st0 name sz0 mbkeyDisp (keyAction g1 upds g2 acts (kbdcam ctrl))
    return $ do
        thing <- cam'
        state <- getW w
        roi <- getROI w
        let (newState, result) = resultFun roi state thing
        inWin w (render $ resultDisp roi newState result)
        putW w newState
        return result

sMonitor name f = interface (Size 240 360) name 0 (const.const.const) acts id [] Nothing (const (,)) g
  where
    g roi k x = r !! j
      where
        r = f roi x
        j = k `mod` length r
    acts = [((MouseButton WheelUp,   Down, modif), (+1))
           ,((SpecialKey  KeyUp,     Down, modif), (+1))
           ,((MouseButton WheelDown, Down, modif), pred)
           ,((SpecialKey  KeyDown,   Down, modif), pred)]

observe name f = interface (Size 240 360) name () id [] id [] Nothing (const (,)) (const.const $ f)

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

data EVWindow st = EVW { evW   :: Window
                       , evSt   :: IORef st
                       , evROI :: IORef ROI
                       , evInit :: IO ()
                       }

evWindow st0 name size mdisp kbd = do
    st <- newIORef st0
    glw <- createWindow name
    windowSize $= glSize size
    let draw = case mdisp of
            Nothing -> return ()
            Just fun -> do
                clear [ColorBuffer]
                fun st
                swapBuffers
    displayCallback $= draw

    actionOnWindowClose $= ContinueExectuion

    let Size h w = size
        initROI = ROI {r1=0, r2=h-1, c1=0, c2=w-1}
    r <- newIORef initROI

    let w = EVW { evW = glw
                , evSt = st
                , evROI = r
                , evInit = clear [ColorBuffer] }

    keyboardMouseCallback $= Just (kbdroi r initROI (kbd w))
    motionCallback $= Just (mvroi r)

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

getROI = get . evROI

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

kbdroi r _ _ (MouseButton RightButton) Down Modifiers {shift=Up} pos@(Position x y) =
    modifyIORef r (\ (ROI _ r2 _ c2) ->
                        ROI (min (r2-minroi) (fromIntegral y)) r2
                            (min (c2-minroi) (fromIntegral x)) c2)
kbdroi r _ _ (MouseButton RightButton) Down Modifiers {shift=Down} pos@(Position x y) =
    modifyIORef r (\ (ROI r1 _ c1 _) ->
                        ROI r1 (max (r1+minroi) (fromIntegral y))
                            c1 (max (c1+minroi) (fromIntegral x)))
kbdroi r initroi _ (Char 'r') Down _ _ = writeIORef r initroi
kbdroi _ _ defaultFunc a b c d = defaultFunc a b c d

mvroi r (Position x y) =
    modifyIORef r (\ (ROI r1 _ c1 _) ->
                        ROI r1 (max (r1+minroi) (fromIntegral y))
                            c1 (max (c1+minroi) (fromIntegral x)))

--------------------------------------------------------------------------------

{- | Adds a pause control to a camera. Commands:

    \"pause\" -> toggles the pause state

    \"step\"  -> toggles the frame by frame state (the next frame is obtained by \"pause\")

-}
withPause :: IO a                          -- ^ original camera
          -> IO (IO a, (IO(), IO(), IO())) -- ^ camera and controller
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

