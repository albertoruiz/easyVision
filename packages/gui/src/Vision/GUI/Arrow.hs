{-# LANGUAGE Arrows #-}
{-# LANGUAGE BangPatterns #-}

-----------------------------------------------------------------------------
{- |
Module      :  Vision.GUI.Arrow
Copyright   :  (c) Alberto Ruiz 2012
License     :  GPL

Maintainer  :  Alberto Ruiz (aruiz at um dot es)

Arrow interface.

-}
-----------------------------------------------------------------------------

module Vision.GUI.Arrow(
    runITrans, runT_, runT, runS,
    ITrans(ITrans), Trans(Trans),
    transUI, arrL, (@@@), delay', arrIO,
    runNT_
)where

import Control.Concurrent   (forkIO)
import Util.LazyIO          (mkGenerator, lazyList, grabAll, createGrab)
import Vision.GUI.Interface (runIt,VC,VCN)

import qualified Control.Category as Cat
import Control.Arrow
import Control.Monad
import Data.Either(lefts,rights)
import System.IO.Unsafe(unsafeInterleaveIO)
import Control.Concurrent
import Util.Misc(debug)
import Data.IORef
import System.Exit       (exitWith, ExitCode(ExitSuccess))
import Graphics.UI.GLUT (mainLoopEvent)

--------------------------------------------------------------------------------

newtype Trans a b = Trans ( IO a -> IO b )

newtype ITrans a b = ITrans (IO (Trans a b))

instance Cat.Category Trans
  where
    id = Trans id
    Trans f . Trans g = Trans ( f . g )


instance Cat.Category ITrans
  where
    id = ITrans $ return (Cat.id)
    ITrans gf . ITrans gg = ITrans (liftM2 (>>>) gg gf)

  
instance Arrow ITrans
  where
    arr f = ITrans (return (Trans (fmap f)))
    first f = f *** Cat.id
    ITrans gf *** ITrans gg = ITrans $ do
        Trans f <- gf
        Trans g <- gg
        ma <- newEmptyMVar
        mb <- newEmptyMVar

        let sepa c = do
                ea <- isEmptyMVar ma
                eb <- isEmptyMVar mb
                if ea
                  then do
                    (a,b) <- c
                    if eb then putMVar mb b else swapMVar mb b >> return ()
                    --putStrLn "RA"
                    return a
                  else
                    --putStrLn "A" >>
                    takeMVar ma

                
            sepb c = do
                ea <- isEmptyMVar ma
                eb <- isEmptyMVar mb
                if eb
                  then do
                    (a,b) <- c
                    if ea then putMVar ma a else swapMVar ma a >> return ()
                    --putStrLn "RB"
                    return b
                  else
                    --putStrLn "B" >>
                    takeMVar mb


        return $ Trans $ \ab -> do
            b <- f (sepa ab)
            d <- g (sepb ab)
            return (b,d)
            
--------------------------------------------------------------------------------

arrL :: ([a]->[b]) -> ITrans a b
-- ^ pure function on the whole list of results
arrL f = ITrans $ do
    pl <- newIORef Nothing
    return $ Trans $ \c -> do
        mbl <- readIORef pl
        case mbl of
            Nothing -> do writeIORef pl =<< fmap (Just . f) (grabAll c)
                          cg pl
            Just _ -> cg pl
  where
    cg pl = do
        Just r <- readIORef pl
        case r of
          h:t -> do writeIORef pl (Just t)
                    return h
          []  -> exitWith ExitSuccess


--------------------------------------------------------------------------------

transUI :: VCN a b -> ITrans a b
transUI = ITrans . fmap Trans


arrIO :: (a -> IO b) -> ITrans a b
-- ^ lift an IO action to the ITrans arrow
arrIO f = transUI . return $ \c -> c >>= f

--------------------------------------------------------------------------------

(@@@) :: (p -> x -> y) -> IO (IO p) -> ITrans x y
-- ^ apply a pure function with parameters taken from the UI
infixl 3 @@@
f @@@ p = arr (uncurry f) <<< (transUI (fmap const p) &&& arr id)

------------------------------------------------------------------

instance ArrowChoice ITrans where
    left f = f +++ arr id
    f +++ g = (f >>> arr Left) ||| (g >>> arr Right)
    ITrans gf ||| ITrans gg = ITrans $ do
        Trans f <- gf
        Trans g <- gg
        return $ Trans $ \ab -> ab >>= either (f.return) (g.return)

--------------------------------------------------------------------------------


delay' :: ITrans a a
-- ^ similar to delay from ArrowCircuit, initialized with the first element
delay' = arrL f
  where
    f [] = []
    f (a:as) = a:a:as


--------------------------------------------------------------------------------

runITrans :: ITrans a b -> [a] -> IO [b]
runITrans (ITrans gt) as = do
    Trans t <- gt
    x <- createGrab as
    grabAll (t x)


runT_ :: IO (IO a) -> ITrans a b -> IO ()
-- ^ run a camera generator on a transformer
runT_ gcam gt = runIt $ do
    bs <- runS gcam gt
    forkIO $ mapM_ g bs
  where
    g !_x = putStr ""


runNT_ :: IO (IO a) -> ITrans a b -> IO ()
-- ^ run process without fork, (needs explicit "prepare").
-- This is currently required for certain GPU applications.
runNT_ gcam gt = do
    bs <- runS gcam gt
    mapM_ g bs
  where
    g !_x = putStr "" >> mainLoopEvent


runT :: IO (IO a) -> ITrans a b -> IO [b]
-- ^ run a camera generator on a transformer, returning the results in a lazy list
runT gcam gt = do
    rbs <- newChan
    forkIO $ runIt $ do
        bs <- runS gcam gt
        forkIO $ mapM_ (writeChan rbs) bs
    getChanContents rbs



runS :: IO (IO a) -> ITrans a b -> IO [b]
-- ^ runT without the GUI (run silent) 
-- runS gcam gt = gcam >>= grabAll >>= runITrans gt
runS gcam (ITrans gt) = do
    cam <- gcam
    Trans t <- gt
    r <- grabAll (t cam)
    return r

