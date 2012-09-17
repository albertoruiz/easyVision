{-# LANGUAGE TupleSections #-}

-----------------------------------------------------------------------------
{- |
Module      :  Vision.GUI.Util
Copyright   :  (c) Alberto Ruiz 2012
License     :  GPL

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional

common interfaces

-}
-----------------------------------------------------------------------------

module Vision.GUI.Util (
    observe, observe3D, camG, cameraFolderIM,
    sMonitor,
    browser, browser3D,
    editor,
    updateItem,
    camera, run,
    freqMonitor, wait,
    browseLabeled,
    choose, optDo, optDont,
    withParam,
    drawParam, draw3DParam,
    connectParamWith,
    connectWith, connectWith',
    clickPoints,
    interactive3D,
    clickKeep, clickList, clickTag
) where

import Graphics.UI.GLUT hiding (Point,Size,color)
import Vision.GUI.Types
import Vision.GUI.Interface
import Vision.GUI.Parameters(ParamRecord(..))
import Control.Arrow((***),(>>>),arr)
import Control.Monad((>=>),join)
import Control.Applicative((<*>),(<$>))
import ImagProc
import ImagProc.Camera(findSize,readFolderMP,readFolderIM,getCam)
import Vision.GUI.Arrow--(ITrans, Trans,transUI,transUI2,runT_)
import Util.LazyIO((~>),(>~>),mkGenerator,Generator)
import Util.Misc(replaceAt,posMin)
import Util.Options
import Control.Concurrent(threadDelay,forkIO)
import Data.Colour.Names
import Data.Time
import System.CPUTime
import Text.Printf(printf)
import Data.IORef
import Data.Maybe(fromJust)

--createGrab :: [b] -> IO (IO b)
--createGrab = fmap (fmap (fmap fromJust)) mkGenerator



editor :: [Command (Int,[x]) (Int,[x])] -> [Command (Int,[x]) (IO())]
       -> String -> [x] -> (Int -> x -> Drawing) -> IO (EVWindow (Int,[x]))
editor upds acts name xs drw = standalone (Size 400 400) name (0,xs) (upds ++ move) acts f
  where
    f (k,xs) | null xs = text (Point 0 0) "empty list!"
             | otherwise = drw j (xs !! j)
                 where
                   j = k `mod` (length xs)   
    move = g2 [ ( key (MouseButton WheelUp),   (+1) )
              , ( key (SpecialKey  KeyUp),     (+1) )
              , ( key (MouseButton WheelDown), pred )
              , ( key (SpecialKey  KeyDown),   pred ) ]
    g2 = map (id *** const.const.(***id))

--    g1 = map (id *** const.const.(snd.))
--    g3 = map (id *** const.const)


--updateItem :: key -> (a -> a) -> (key, roi -> pt -> (Int, [a]) -> (Int, [a]))
updateItem key f = (key, \roi pt (k,xs) -> (k, replaceAt [k] [f roi pt (xs!!k)] xs))

--------------------------------------------------------------------------------

browser :: String -> [x] -> (Int -> x -> Drawing) -> IO (EVWindow (Int,[x]))
browser = editor [] []

-- editor 3D?

browser3D :: String -> [x] -> (Int -> x -> Drawing) -> IO (EVWindow (Int,[x]))
browser3D name xs drw = standalone3D (Size 400 400) name (0,xs) move [] f
  where
    f (k,xs) | null xs = text (Point 0 0) "empty list!"
             | otherwise = drw j (xs !! j)
                 where
                   j = k `mod` (length xs)   
    move = g2 [ ( key (MouseButton WheelUp),   (+1) )
              , ( key (SpecialKey  KeyUp),     (+1) )
              , ( key (MouseButton WheelDown), pred )
              , ( key (SpecialKey  KeyDown),   pred ) ]
    g2 = map (id *** const.const.(***id))


--------------------------------------------------------------------------------

browseLabeled :: String -> [(x,String)] -> (x -> Drawing)
              -> IO (EVWindow (Int, [(x,String)] ))
browseLabeled name examples disp = browser name examples f
  where
    f k (x,lab) = Draw [ disp x , winTitle (name++" #"++show (k+1)++ ": "++lab) ]

--------------------------------------------------------------------------------

sMonitor :: String ->
            (WinRegion -> b -> [Drawing])
            -> IMTrans b b
sMonitor name f = optDont ("--no-"++name)
                $ optDont ("--no-gui")
                $ transIA
                $ interface
                (Size 240 360)            -- window size
                name                      -- win title
                0                         -- init state
                (const.const.return $ ()) -- init action
                (c2 acts)                 -- state update
                []                        -- key IO acts
                (const (,))               -- result
                g                         -- draw
  where
    g roi k _ x | null r    = Draw ()
                | otherwise = r !! j
      where
        r = f roi x
        j = k `mod` length r
    acts = [ ( key (MouseButton WheelUp),   (+1) )
           , ( key (SpecialKey  KeyUp),     (+1) )
           , ( key (MouseButton WheelDown), pred )
           , ( key (SpecialKey  KeyDown),   pred ) ]
    c2 = Prelude.map (id *** const.const)

--------------------------------------------------------------------------------

observe :: Renderable x => String -> (b -> x) -> IMTrans b b
observe name f = optDont ("--no-"++name)
               $ optDont ("--no-gui")
               $ transIA 
               $ interface (Size 240 360) name () (const.const.return $ ())
                           [] [] (const (,)) (const.const.const $ Draw . f)


observe3D :: Renderable x => String -> (b -> x) -> IMTrans b b
observe3D name f = optDont ("--no-"++name)
               $ optDont ("--no-gui")
               $ transIA 
               $ interface3D (Size 400 400) name () (const.const.return $ ())
                             [] [] (const (,)) (const.const.const $ Draw . f)

--------------------------------------------------------------------------------

{- |
Returns the first image source given in the command line.

By default it uses the first alias in cameras.def.

It also admits --photos=path/to/folder/ containing separate image files (.jpg or .png), currently read using imagemagick (slower, lazy),
and --photosmp=path/to/folder, to read images of the same type and size using mplayer (faster, strict).
-}
camera :: Generator Channels
camera = do
    f <- hasValue "--photos"
    g <- hasValue "--photosmp"
    h <- hasValue "--sphotos"
    if f then cameraFolderIM
         else if g then cameraFolderMP
                   else if h then cameraP
                             else cameraV

cameraP :: Generator Channels
cameraP = do
    hp <- optionString "--sphotos" "."
    g <- readFolderIM hp
    c <- mkGenerator g
    return (fmap fst <$> c)

cameraV :: Generator Channels
cameraV = findSize >>= getCam 0 ~> channels

cameraFolderIM :: Generator Channels
cameraFolderIM = camG "--photos" r <*> dummy
  where
    r p _ = readFolderIM p

cameraFolderMP :: Generator Channels
cameraFolderMP = camG "--photosmp" readFolderMP <*> dummy

camG opt readf = do
    path <- optionString opt "."
    sz <- findSize
    imgs <- readf path (Just sz)
    interface (Size 240 320) "photos" (0,imgs) ft (keys imgs) [] r sh
  where
    keys xs = acts (length xs -1)
    acts n = [ (key (MouseButton WheelUp),   \_ _ (k,xs) -> (min (k+1) n,xs))
             , (key (SpecialKey  KeyUp),     \_ _ (k,xs) -> (min (k+1) n,xs))
             , (key (MouseButton WheelDown), \_ _ (k,xs) -> (max (k-1) 0,xs))
             , (key (SpecialKey  KeyDown),   \_ _ (k,xs) -> (max (k-1) 0,xs))]
    r _ (k,xs) _ = ((k,xs), fst $ xs!!k)
    sh _ (k,xs) _a x = Draw [Draw (rgb x), info (k,xs) ]
    -- ft w _ = evPrefSize w $= Just (Size 240 320)
    ft _ _ = return ()
    info (k,xs) = Draw [color black $ textF Helvetica12 (Point 0.9 0.6)
                        (show w ++ "x" ++ show h ++ " " ++snd ( xs!!k)) ]
      where
        Size h w = size (grayscale $ fst $ xs!!k)

dummy :: Generator ()
dummy = return (threadDelay 100000 >> return (Just ()))


run t = runT_ camera (t >>> optDo "--freq" freqMonitor) 

--------------------------------------------------------------------------------

frameRate = do
    t0 <- getCurrentTime
    rt <- newIORef (t0,(1/20,0.01))
    return $ \cam -> do
            (t0,(av,cav)) <- readIORef rt
            ct0 <- getCPUTime
            r <- cam
            ct1 <- getCPUTime
            t1 <- getCurrentTime
            let dt = realToFrac $ diffUTCTime t1 t0 :: Double
                av' = av *0.99 + 0.01* dt
                cdt = fromIntegral (ct1-ct0) / (10**9 :: Double)
                cav' = cav *0.99 + 0.01* cdt
            writeIORef rt (t1,(av',cav'))
            return $ fmap (,(av',cav')) r


freqMonitor :: IMTrans x x
freqMonitor = transIA frameRate >>> transIA g
  where
    g = interface (Size 60 300) "Frame Rate" (1/20,0.1) (const . const . return $ ()) [] [] f sh
    f _roi _state (t,x) = (x,t)
    sh _roi (t2,t1) _a _result = text (Point 0.9 0) $
                               printf " %3.0f ms CPU  / %4.0f Hz   /   %3.0f%%"
                               (t1::Double) (1/t2::Double) (t1/t2/10)

--------------------------------------------------------------------------------

wait n = transUI $ return $ \cam -> do
    x <- cam
    threadDelay (n*1000)
    return x

--------------------------------------------------------------------------------

choose :: IO Bool -> IMTrans a b -> IMTrans a b -> IMTrans a b
-- ^ select process depending on something (e.g. a command line flag)
choose q (IMTrans a1) (IMTrans a2) = IMTrans $ do
    yes <- q
    if yes then do {a<-a1; return a} else do {a<-a2; return a}

optDo :: String -> IMTrans a a -> IMTrans a a
optDo flag trans = choose (getFlag (fixFlag flag)) trans (arr id)

optDont :: String -> IMTrans a a -> IMTrans a a
optDont flag trans = choose (getFlag (fixFlag flag)) (arr id) trans

fixFlag = map sp
  where
    sp ' ' = '_'
    sp x = x

withParam :: ParamRecord p => (p -> a -> b) -> IMTrans a b
-- ^ get the first argument from an interactive window, unless the command line options
-- --no-gui or --default are given, in which case the function takes the default parameters.
withParam f = choose c (arr $ f defParam) ( f @@@ (fmap (fmap Just) $ winParam) )
  where
    c = (||) <$> getFlag "--no-gui" <*> getFlag "--default"

--------------------------------------------------------------------------------

drawParam :: ParamRecord t => String -> (t -> [Drawing]) -> IO ()
-- ^ drawing window with interactive parameters
drawParam title f = connectParamWithWin g (browser title [] (const id))
  where
    g (k,_) p = (k, f p)


draw3DParam :: ParamRecord t => String -> (t -> [Drawing]) -> IO ()
-- ^ 3D drawing window with interactive parameters
draw3DParam title f = connectParamWithWin g (browser3D title [] (const id))
  where
    g (k,_) p = (k, f p)


connectParamWith f (wp,gp) b = do
    (evNotify wp) $= do
        p <- gp
        st <- getW b
        putW b (f st p)
        postRedisplay (Just (evW b))
    join . get . evNotify $ wp

connectParamWithWin f win = do
    p <- mkParam
    b <- win
    connectParamWith f p b


connectWithG p f w1 w2 = do
    (evNotify w1) $~ (>> do
        s1 <- getW w1
        s2 <- getW w2
        p w2 (f s2 s1)
        postRedisplay (Just (evW w2)) )
    join . get . evNotify $ w1 


connectWith :: (s2 -> s1 -> s2) -> EVWindow s1 -> EVWindow s2 -> IO ()
connectWith = connectWithG putW

connectWith' :: (s2 -> s1 -> s2) -> EVWindow s1 -> EVWindow s2 -> IO ()
connectWith' = connectWithG putWRaw

--------------------------------------------------------------------------------

clickPoints :: String -- ^ window name
             -> String -- ^ command line option name for loading points
             -> a      -- ^ additional state
             -> (([Point],a) -> Drawing) -- ^ display function
             -> IO (EVWindow ([Point],a))
clickPoints name ldopt st sh = do
    pts <- optionFromFile ldopt []
    standalone (Size 400 400) name (pts,st) updts acts sh
  where

    updts = [ (key (MouseButton LeftButton), const new)
            , (key (MouseButton RightButton), const move)
            , (key (Char '\DEL'), \_ _ (ps,st) -> if null ps then (ps,st) else (init ps,st))
            ]

    acts = [(ctrlS, \_ _ (ps,_) -> print ps)]
    ctrlS = kCtrl (key (Char '\DC3'))

    new p (ps,st) = (ps++[p],st)

    move p (ps,st) = (replaceAt [j] [p] ps, st)
      where
        j = posMin (map (distPoints p) ps)

--------------------------------------------------------------------------------

interactive3D :: String -> IO (Drawing -> IO (), Drawing -> IO ())
-- ^ interactive 3D drawing, useful for ghci
interactive3D name = do
        prepare
        b <- browser3D name [] (const id)
        forkIO mainLoop
        return (resetDrawing b, addDrawing b)
  where
    resetDrawing w d = putW w (0,[d]) >> p
      where
        p = postRedisplay (Just (evW w))

    addDrawing w d = do
        (0,[x]) <- getW w
        resetDrawing w (Draw [x,d])

--------------------------------------------------------------------------------

clickKeep :: String -> (WinRegion -> a -> b) -> ((a,b) -> Drawing) -> Maybe b -> IMTrans a (a,b)
-- ^ click to set something to be forwarded
clickKeep name f sh s0 = transUI $ interface (Size 240 320) name s0 ft updt [] r sh'
  where
    r _ (Just s) input = (Just s, (input,s))
    r roi Nothing input = (Just s, (input,s))
      where
        s = f roi input
    sh' _ _ _ = sh
    updt = [(key (MouseButton LeftButton), \_ _ _ -> Nothing )]
    ft _ _ = return ()

--------------------------------------------------------------------------------

clickList :: String -> (WinRegion -> a -> b) -> ((a,[b]) -> Drawing) -> [b] -> IMTrans a (a,[b])
-- ^ click to add things to a list to be forwarded
clickList name f sh xs0 = transUI $ interface (Size 240 320) name (xs0,False) ft updt [] r sh'
  where
    r _ (xs,False) input = ((xs,False), (input,xs))
    r roi (xs,True) input = ((xs',False), (input,xs'))
      where
        xs' = f roi input : xs
    sh' _ _ _ = sh
    updt = [(key (MouseButton LeftButton), \_ _ (xs,_) -> (xs,True) )]
    ft _ _ = return ()

--------------------------------------------------------------------------------

clickTag :: (x -> l) -> (x -> r) -> (Either l r -> Drawing) -> String -> IMTrans x (Either l r)
clickTag l r sh name = transUI $ interface (Size 240 320) name False ft updt [] y ((const.const.const) sh)
  where
    y _ sv input = (sv, if sv then Right (r input) else Left (l input))
    updt = [(key (MouseButton LeftButton), \_ _ sv -> not sv)]
    ft _ _ = return ()

