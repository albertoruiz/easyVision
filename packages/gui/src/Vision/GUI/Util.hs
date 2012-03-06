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
    drawParam, draw3DParam
) where

import Graphics.UI.GLUT hiding (Point,Size,color)
import Vision.GUI.Types
import Vision.GUI.Interface
import Vision.GUI.Parameters(ParamRecord(..))
import Control.Arrow((***),(>>>),arr)
import Control.Monad((>=>))
import Control.Applicative((<*>),(<$>))
import ImagProc
import ImagProc.Camera(findSize,readFolderMP,readFolderIM,getCam)
import Vision.GUI.Arrow--(ITrans, Trans,transUI,transUI2,runT_)
import Util.LazyIO((~>),(>~>),createGrab)
import Util.Misc(replaceAt)
import Util.Options
import Control.Concurrent(threadDelay)
import Data.Colour.Names
import Data.Time
import System.CPUTime
import Text.Printf(printf)
import Data.IORef

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

sMonitor :: String -> (WinRegion -> b -> [Drawing]) -> ITrans b b
sMonitor name f = optDont ("--no-"++name)
                $ transUI 
                $ interface (Size 240 360) name 0 (const.const.return $ ()) 
                            (c2 acts) [] (const (,)) g
  where
    g roi k x | null r    = Draw ()
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

observe :: Renderable x => String -> (b -> x) -> ITrans b b
observe name f = optDont ("--no-"++name)
               $ transUI 
               $ interface (Size 240 360) name () (const.const.return $ ())
                           [] [] (const (,)) (const.const $ Draw . f)


observe3D :: Renderable x => String -> (b -> x) -> ITrans b b
observe3D name f = optDont ("--no-"++name)
               $ transUI 
               $ interface3D (Size 400 400) name () (const.const.return $ ())
                             [] [] (const (,)) (const.const $ Draw . f)

--------------------------------------------------------------------------------

{- |
Returns the first image source given in the command line.

By default it uses the first alias in cameras.def.

It also admits --photos=path/to/folder/ containing separate image files (.jpg or .png), currently read using imagemagick (slower, lazy),
and --photosmp=path/to/folder, to read images of the same type and size using mplayer (faster, strict).
-}
camera :: IO (IO Channels)
camera = do
    f <- hasValue "--photos"
    g <- hasValue "--photosmp"
    h <- hasValue "--sphotos"
    if f then cameraFolderIM
         else if g then cameraFolderMP
                   else if h then cameraP
                             else cameraV

cameraP = do
    hp <- optionString "--sphotos" "."
    g <- readFolderIM hp
    c <- createGrab g
    return (fst <$> c)

cameraV = findSize >>= getCam 0 ~> channels


cameraFolderIM = camG "--photos" r <*> dummy
  where
    r p _ = readFolderIM p

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
    sh _ (k,xs) x = Draw [Draw (rgb x), info (k,xs) ]
    -- ft w _ = evPrefSize w $= Just (Size 240 320)
    ft _ _ = return ()
    info (k,xs) = Draw [color black $ textF Helvetica12 (Point 0.9 0.6)
                        (show w ++ "x" ++ show h ++ " " ++snd ( xs!!k)) ]
      where
        Size h w = size (grayscale $ fst $ xs!!k)

dummy :: IO (IO ())
dummy = return (threadDelay 100000 >> return ())


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
            return (r,(av',cav'))


freqMonitor :: ITrans x x
freqMonitor = transUI frameRate >>> transUI g
  where
    g = interface (Size 60 300) "Frame Rate" (1/20,0.1) (const . const . return $ ()) [] [] f sh
    f _roi _state (t,x) = (x,t)
    sh _roi (t2,t1) _result = text (Point 0.9 0) $
                               printf " %3.0f ms CPU  / %4.0f Hz   /   %3.0f%%"
                               (t1::Double) (1/t2::Double) (t1/t2/10)

--------------------------------------------------------------------------------

wait n = transUI $ return $ \cam -> do
    x <- cam
    threadDelay (n*1000)
    return x

--------------------------------------------------------------------------------

choose :: IO Bool -> ITrans a b -> ITrans a b -> ITrans a b
-- ^ select process depending on something (e.g. a command line flag)
choose q (ITrans a1) (ITrans a2) = ITrans $ do
    yes <- q
    if yes then do {a<-a1; return a} else do {a<-a2; return a}

optDo :: String -> ITrans a a -> ITrans a a
optDo flag trans = choose (getFlag (fixFlag flag)) trans (arr id)

optDont :: String -> ITrans a a -> ITrans a a
optDont flag trans = choose (getFlag (fixFlag flag)) (arr id) trans

fixFlag = map sp
  where
    sp ' ' = '_'
    sp x = x

withParam :: ParamRecord p => (p -> a -> b) -> ITrans a b
-- ^ get the first argument from an interactive window, unless the command line options
-- --no-gui or --default are given, in which case the function takes the default parameters.
withParam f = choose c (arr $ f defParam) (f @@@ winParam) 
  where
    c = (||) <$> getFlag "--no-gui" <*> getFlag "--default"

--------------------------------------------------------------------------------

drawParam :: ParamRecord t => String -> (t -> [Drawing]) -> IO ()
-- ^ drawing window with interactive parameters
drawParam title f = do
    (wp,gp) <- mkParam
    b <- browser title [] (const id)
    (evAfterD wp) $= do
        p <- gp
        (k,_) <- getW b
        putW b (k, f p)
        postRedisplay (Just (evW b))

--------------------------------------------------------------------------------

draw3DParam :: ParamRecord t => String -> (t -> [Drawing]) -> IO ()
-- ^ 3D drawing window with interactive parameters
draw3DParam title f = do
    (wp,gp) <- mkParam
    b <- browser3D title [] (const id)
    (evAfterD wp) $= do
        p <- gp
        (k,_) <- getW b
        putW b (k, f p)
        postRedisplay (Just (evW b))

