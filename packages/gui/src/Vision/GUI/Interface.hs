{-# LANGUAGE BangPatterns #-}

---------------------------------------------------------------------------
{- |
Module      :  Vision.GUI.Interface
Copyright   :  (c) Alberto Ruiz 2006-12
License     :  GPL

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional

User interface tools.

-}
-----------------------------------------------------------------------------

module Vision.GUI.Interface (
    -- * Interface
    VCN, 
    Command, WinInit,
    interface, standalone, interface3D, standalone3D,
    -- * Tools
    Size(..),
    prepare, runIt,
    evWindow,
    inWin, getW, putW, updateW, putWRaw, updateWRaw,
    kbdcam, kbdQuit, keyAction,
    Key(..), SpecialKey(..), MouseButton(..), key, kUp, kCtrl, kShift, kAlt, BitmapFont(..)
) where

import Vision.GUI.Types
import Vision.GUI.Draw
import Vision.GUI.Trackball
import Util.Geometry
--import ImagProc.Ipp(ippSetNumThreads)
import Image
import Image.Devel(shSize,pixelsToPoints)
import Graphics.UI.GLUT hiding (RGB, Matrix, Size, None, Point,color)
import Data.IORef
import System.Process(system)
import System.Exit
import Control.Monad(when,join,filterM)
import Control.Monad.IO.Class(MonadIO)
import System.Environment
import Util.Debug(errMsg)
import Data.Colour.Names
import Control.Concurrent
import Data.List(intercalate)
import System.Directory


keyAction
    :: [((Key, KeyState, Modifiers), WinRegion -> Point -> st -> st)]
    -> [((Key, KeyState, Modifiers), WinRegion -> Point -> st -> IO ())]
    -> (Key -> KeyState -> Modifiers -> Position -> IO ())
    -> EVWindow st
    -> Key
    -> KeyState
    -> Modifiers
    -> Position
    -> IO ()
keyAction upds acts def w a b c d = do
    st <- getW w
    gp <- unZoomPoint w
    roii <- get (evRegion w)
    case Prelude.lookup (a,b,c) upds of
        Just op -> putW w (withPoint op roii gp d st)
        Nothing -> case Prelude.lookup (a,b,c) (help acts) of
                        Just op -> withPoint op roii gp d st
                        Nothing -> def a b c d
  where
    withPoint f roii gp pos = f roii (gp pos)
    help actss = actss ++ [(key (SpecialKey KeyF1), \_p _r _w -> callHelp s)]
    s = evWinTitle w

modif :: Modifiers
modif = Modifiers {ctrl = Up, shift = Up, alt = Up }

kCtrl :: (t, t1, Modifiers) -> (t, t1, Modifiers)
kCtrl (k,s,m) = (k, s, m {ctrl = Down})

kShift :: (t, t1, Modifiers) -> (t, t1, Modifiers)
kShift (k,s,m) = (k, s, m {shift = Down})

kAlt :: (t, t1, Modifiers) -> (t, t1, Modifiers)
kAlt (k,s,m) = (k, s, m {alt = Down})

kUp :: (t1, t, t2) -> (t1, KeyState, t2)
kUp (k,_s,m) = (k, Up, m)

key :: t -> (t, KeyState, Modifiers)
key k = (k, Down, modif)

--------------------------------------------------------------------------------

type Command state result = ((Key,KeyState,Modifiers), WinRegion -> Point -> state -> result)
type WinInit state input = EVWindow state -> input -> IO()

type VCN a b = IO (IO (Maybe a) -> IO (Maybe b))

interface
    :: Size                              -- win size
    -> String                            -- win title
    -> s                                 -- state 0
    -> WinInit s a                       -- init Window
    -> [Command s s]                     -- upds
    -> [Command s (IO())]                -- acts
    -> (WinRegion -> s -> a -> (s,b))    -- result
    -> (WinRegion -> s -> a -> b -> Drawing)  -- draw 
    -> VCN a b

interface = interfaceG False

interface3D :: Size -> String -> s 
          -> WinInit s a -> [Command s s] -> [Command s (IO())]
          -> (WinRegion -> s -> a -> (s,b))
          -> (WinRegion -> s -> a -> b -> Drawing) 
          -> VCN a b

interface3D = interfaceG True


interfaceG
    :: Bool
    -> Size
    -> String
    -> st
    -> (EVWindow st -> t -> IO a1)
    -> [((Key, KeyState, Modifiers), WinRegion -> Point -> st -> st)]
    -> [((Key, KeyState, Modifiers),
         WinRegion -> Point -> st -> IO ())]
    -> (WinRegion -> st -> t -> (st, a))
    -> (WinRegion -> st -> t -> a -> Drawing)
    -> IO (IO (Maybe t) -> IO (Maybe a))
interfaceG threeD sz0 name st0 ft upds acts resultFun resultDisp = do

    firstTimeRef <- newIORef True
    let evWin = if threeD then evWindow3D else evWindow
    w <- evWin st0 name sz0 (keyAction upds acts kbdQuit)

    displayCallback $= do
        evInit w
        dr <- readMVar (evDraw w)
        renderIn w dr
        drawRegion w
        swapBuffers
        join . get . evAfterD $ w
        --putStrLn "  D"

    callbackFreq 5 $ do
        visible <- get (evVisible w)
        sync <- readIORef (evSync w)
        ready <- readMVar (evReady w)
        when (visible && ready && not sync) $ do
            postRedisplay (Just (evW w))
            _ <- swapMVar (evReady w) False
            return ()

    pauser <- newPauser (evPause w)

    return $ \cam -> do
     end <- readIORef (evEnd w)
     if end then return Nothing
            else do
      mbthing <- pauser cam
      case mbthing of
       Nothing -> return Nothing -- FIXME :(
       Just thing -> do
        firstTime <- readIORef firstTimeRef
        when firstTime $ ft w thing >> writeIORef firstTimeRef False
        state <- getW w
        roii <- get (evRegion w)
        let (newState, result) = resultFun roii state thing
            drawing = resultDisp roii newState thing result
        seq newState $ putW w newState
        pause <- readIORef (evPause w)
        when (not (pause==PauseDraw)) $ swapMVar (evDraw w) drawing >> return ()
        _ <- swapMVar(evReady w) True
        --putStrLn "W"
        sync <- readIORef (evSync w)
        when sync $ postRedisplay (Just (evW w))
        modifyIORef (evStats w) (\s -> s { evNCall = evNCall s + 1 })
        return (Just result)

drawRegion :: EVWindow st -> IO ()
drawRegion w = do
    ok <- readIORef (evDrReg w)
    modifyIORef (evStats w) (\s -> s { evNDraw = evNDraw s + 1 })
    when ok $ do
        (Point x1 y1, Point x2 y2) <- readIORef (evRegion w)
        stats <- readIORef (evStats w)
        psz <- readIORef (evPrefSize w)
        -- (z,a,b) <- readIORef (evZoom w)
        wsz <- get windowSize
        let shpsz = case psz of
                Nothing -> "  "
                Just sz -> "    pSize: " ++ shSize sz ++ " / "
            info = show (evNCall stats) ++ " frames / " ++ show (evNDraw stats)
                   ++ " draws (" ++ show (evNCall stats - evNDraw stats) ++ ")"
                   ++ shpsz ++ "wSize: " ++ shSize (evSize wsz)
        --         ++ "   zoom = " ++ printf "(%.2f,%.1f,%.1f)" z a b
        render $ Draw [ color red . lineWd 1.5 $
                        Closed [ Point x1 y1, Point x2 y1
                               , Point x2 y2, Point x1 y2]
                      , textF Helvetica10 (Point 0.95 (-0.7)) info
                      ]
        
----------------------------------------

standalone :: Size -> String -> s
           -> [Command s s] -> [Command s (IO ())]
           -> (s -> Drawing)
           -> IO (EVWindow s)
standalone = standaloneG False

standalone3D :: Size -> String -> s
             -> [Command s s] -> [Command s (IO ())]
             -> (s -> Drawing)
             -> IO (EVWindow s)
standalone3D = standaloneG True

standaloneG
    :: Renderable x
    => Bool
    -> Size
    -> String
    -> st
    -> [((Key, KeyState, Modifiers), WinRegion -> Point -> st -> st)]
    -> [((Key, KeyState, Modifiers), WinRegion -> Point -> st -> IO ())]
    -> (st -> x)
    -> IO (EVWindow st)
standaloneG threeD sz0 name st0 upds acts disp = do
    let evWin = if threeD then evWindow3D else evWindow
    w <- evWin st0 name sz0 (keyAction upds acts kbdQuit)

    displayCallback $= do
        evInit w
        --prepZoom w
        st <- getW w
        renderIn w (disp st)
        drawRegion w
        swapBuffers
        join . get . evAfterD $ w

    return w

-----------------------------------------------------------------

-- | Initializes the HOpenGL system.
prepare :: IO ()
prepare = do
    _ <- getArgsAndInitialize
    initialDisplayMode $= [DoubleBuffered, WithDepthBuffer]
    -- ippSetNumThreads 1
    return ()


callbackFreq :: Timeout -> IO () -> IO ()
callbackFreq freq worker = do
    let callback = do
        addTimerCallback (1000 `div` freq) callback
        worker
    addTimerCallback 10 callback


runIt :: IO a -> IO ()
runIt f = prepare >> f >> mainLoop

----------------------------------------------------------------

irr :: (Point, Point)
irr = (Point p p, Point n n)
  where p = 0.5; n = -0.5    

evWindow
    :: st
    -> String
    -> Size
    -> (EVWindow st -> Key -> KeyState -> Modifiers -> Position -> IO ())
    -> IO (EVWindow st)
evWindow st0 name sz kbd = do
    st <- newIORef st0
    glw <- createWindow name
    iconTitle $= name
    windowSize $= glSize sz

--  actionOnWindowClose $= ContinueExectuion
--  Exit by default

    rr <- newIORef irr
    drr <- newIORef False
        
    zd <- newIORef (1,0,0)
    ms <- newIORef None
    po <- newIORef StaticSize
    ps <- newIORef Nothing
    vi <- newIORef True
    re <- newMVar True
    dr <- newMVar (Draw ())
    sy <- newIORef True
    pa <- newIORef NoPause
    dc <- newIORef (WStatus 0 0)
    ad <- newIORef (return ())
    no <- newIORef (return ())
    rend <- newIORef False

    let w = EVW { evW = glw
                , evSt = st
                , evDraw = dr
                , evAfterD = ad
                , evNotify = no
                , evSync = sy
                , evReady = re
                , evRegion = rr
                , evDrReg = drr
                , evZoom = zd
                , evMove = ms
                , evPolicy = po
                , evPrefSize = ps
                , evVisible = vi
                , evPause = pa
                , evStats = dc
                , evEnd = rend
                , evWinTitle = name
                , evInit = clear [ColorBuffer] >> prepZoom w }

    keyboardMouseCallback $= Just (\k d m p -> kbdroi w (kbd w) k d m p >> postRedisplay Nothing)
    motionCallback $= Just (\p -> mvroi w p >> postRedisplay Nothing)
    -- callback to detect minimization?

    return w


evWindow3D
    :: st
    -> String
    -> Size
    -> (EVWindow st -> KeyboardMouseCallback)
    -> IO (EVWindow st)
evWindow3D ist name sz kbd = do
    (trackball,kc,mc,auto) <- newTrackball
    w <- evWindow ist name sz (kc kbd)
    motionCallback $= Just mc
    depthFunc $= Just Less
    textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
    textureFunction $= Replace
    let callback = do
        addTimerCallback 50 callback
        ok <- auto
        when ok $ postRedisplay (Just (evW w))
    addTimerCallback 1000 callback
    return w { evInit = clear [ColorBuffer, DepthBuffer] >> trackball}


---------------------------------------------------------------

inWin :: EVWindow st -> IO a -> IO ()
inWin w f = do
    saved <- get currentWindow
    currentWindow $= Just (evW w)
    evInit w
    _ <- f
    swapBuffers
    currentWindow $= saved

getW :: EVWindow a -> IO a
getW = get . evSt

putWRaw :: MonadIO m => EVWindow a -> a -> m ()
putWRaw    w x = evSt w $= x

updateWRaw :: MonadIO m => EVWindow a -> (a -> a) -> m ()
updateWRaw w f = evSt w $~ f

putW :: EVWindow st -> st -> IO ()
putW    w x = putWRaw    w x >> (join . get . evNotify) w

updateW :: EVWindow st -> (st -> st) -> IO ()
updateW w f = updateWRaw w f >> (join . get . evNotify) w

----------------------------------------------------------------

nextPolicy :: ResizePolicy -> ResizePolicy
nextPolicy UserSize = DynamicSize
nextPolicy StaticSize = UserSize
nextPolicy DynamicSize = UserSize

nextPauseDraw :: PauseStatus -> PauseStatus
nextPauseDraw NoPause = PauseDraw
nextPauseDraw _ = NoPause

nextPauseCam :: PauseStatus -> PauseStatus
nextPauseCam NoPause = PauseCam
nextPauseCam _ = NoPause


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

--kbdQuit (Char '\27') Down Modifiers {alt=Down} _ = leaveMainLoop >> system "killall mplayer" >> return ()
kbdQuit (Char '\27') Down _ _ = exitWith ExitSuccess
kbdQuit (Char   'i') Down _ _ = captureGL >>= saveImage ".png"
kbdQuit a Down m _            = errMsg (show a ++ " " ++ show m ++ " not defined")
kbdQuit _ _ _ _               = return ()


kbdroi
    :: EVWindow st
    -> (Key -> KeyState -> Modifiers -> Position -> IO ())
    -> Key
    -> KeyState
    -> Modifiers
    -> Position
    -> IO ()
kbdroi w _ (Char '0') Down Modifiers {alt=Down} _ = do
    mbsz <- readIORef (evPrefSize w)
    case mbsz of
        Nothing -> return ()
        Just _ -> writeIORef (evRegion w) irr

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
    modifyIORef (evRegion w) $ \(_,b) -> (pt,b)
    writeIORef (evDrReg w) True
    writeIORef (evMove w) SetROI

kbdroi w _ (MouseButton LeftButton) Up _ _ = writeIORef (evMove w) None
kbdroi w _ (MouseButton RightButton) Up _ _ = writeIORef (evMove w) None

kbdroi w _ (SpecialKey KeyF3) Down Modifiers {ctrl=Down} _ = do
    vi <- get (evVisible w)
    if vi
        then writeIORef (evVisible w) False >> windowStatus $= Iconified
        else writeIORef (evVisible w) True

kbdroi w _ (SpecialKey KeyF3) Down _ _ = modifyIORef (evPolicy w) nextPolicy

kbdroi w _ (SpecialKey KeyF10) Down _ _ = modifyIORef (evSync w) not
kbdroi w _ (SpecialKey KeyF11) Down _ _ = modifyIORef (evDrReg w) not

kbdroi w _ (Char '0') Down Modifiers {ctrl=Down} _ = writeIORef (evZoom w) (1,0,0)

kbdroi w _ (Char ' ') Down Modifiers {shift=Up} _ = modifyIORef (evPause w) nextPauseCam
kbdroi w _ (Char ' ') Down Modifiers {shift=Down} _ = modifyIORef (evPause w) nextPauseDraw
kbdroi w _ (Char 's') Down _ _ = writeIORef (evPause w) PauseStep

kbdroi w _ (Char '\27') Down Modifiers {ctrl=Down} _ = writeIORef (evEnd w) True

kbdroi _ defaultFunc a b c d = defaultFunc a b c d


mvroi :: EVWindow st -> Position -> IO ()
mvroi w (Position x1' y1') = do
    ms <- readIORef (evMove w)
    gp <- unZoomPoint w
    let pt = gp (Position x1' y1') 
    case ms of
        None -> return ()
        SetROI -> do modifyIORef (evRegion w) $ \(p,_) -> (p,pt)
                     writeIORef (evDrReg w) True
        MoveZoom x0 y0 -> do
            modifyIORef (evZoom w) $
                \(z,x,y) -> (z, x+fromIntegral (x1'-x0), y-fromIntegral (y1'-y0))
            writeIORef (evMove w) (MoveZoom x1' y1')


unZoomPoint :: EVWindow st -> IO (Position -> Point)
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

newPauser :: IORef PauseStatus -> IO (IO b -> IO b)
newPauser refPau = do
    frozen <- newIORef Nothing
    return $ \cam -> do
        pau <- readIORef refPau
        when (pau == PauseStep) (writeIORef refPau PauseCam >> writeIORef frozen Nothing)
        old  <- readIORef frozen
        if (pau == PauseCam || pau == PauseStep)
          then do
            case old of
                Nothing -> do {x <- cam; writeIORef frozen (Just x); return x}
                Just x   -> threadDelay 100000 >> return x
          else do
            case old of
                Just _ -> writeIORef frozen Nothing
                _      -> return ()
            cam   

--------------------------------------------------------------------------------

callHelp :: String -> IO ()
callHelp wn = do
    pname <- getProgName
    errMsg wn
    errMsg pname
    let helpnames = map (++".html") $ concatMap (\x -> [x,"help/"++x,"../help/"++x])
          [ intercalate "-" [pname, wn']
          , pname
          , wn'
          , "help"
          ]
    fs <- filterM doesFileExist helpnames
    let f = head fs
    if null fs
      then errMsg "no help file available"
      else system ("xdg-open "++f) >> return ()
  where
    wn' = map f wn
      where
        f ' ' = '_'
        f  x  =  x

