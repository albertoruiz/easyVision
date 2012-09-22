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
    transUI, arrL, (@@@), delay, delay', arrIO, arrIOMb,
    runNT, runNT_
)where

import Control.Concurrent   (forkIO)
import Util.LazyIO          (mkGenerator, lazyList, Generator)
import Vision.GUI.Interface (runIt,VCN)

import qualified Control.Category as Cat
import Control.Arrow
import Control.Monad
import Control.Monad.Fix
import Control.Arrow.Operations(ArrowCircuit(..))
import Control.Concurrent
import Util.Misc(debug)
import Data.IORef
import System.Exit       (exitWith, ExitCode(ExitSuccess))
import Graphics.UI.GLUT (mainLoopEvent)
import Data.Maybe(fromJust)
import Data.Traversable
import Graphics.UI.GLUT(leaveMainLoop)
import System.IO.Unsafe(unsafeInterleaveIO)

--------------------------------------------------------------------------------

newtype Trans a b = Trans ( IO (Maybe a) -> IO (Maybe b) )

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
    arr f = ITrans (return (Trans (fmap (fmap f))))
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
            let ab' = do
                   mbab <- ab
                   case mbab of
                     Just (a,b) -> return (Just a, Just b)
                     Nothing    -> return (Nothing, Nothing) -- FIXME ?
            b <- f (sepa ab')
            d <- g (sepb ab')
            return $ liftM2 (,) b d


            
--------------------------------------------------------------------------------

arrL :: ([a]->[b]) -> ITrans a b
-- ^ pure function on the whole list of results
arrL f = ITrans $ do
     pl <- newIORef Nothing
     return $ Trans $ \c -> do
         mbl <- readIORef pl
         case mbl of
             Nothing -> do writeIORef pl =<< fmap (Just . f) (lazyList c)
                           cg pl
             Just _ -> cg pl
   where
     cg pl = do
         Just r <- readIORef pl
         case r of
           h:t -> do writeIORef pl (Just t)
                     return (Just h)
           []  -> return Nothing

--------------------------------------------------------------------------------

transUI :: VCN a b -> ITrans a b
transUI = ITrans . fmap Trans

arrIO :: (a -> IO b) -> ITrans a b
-- ^ lift an IO action to the ITrans arrow
arrIO f = transUI . return $ adaptMb (>>=f)


arrIOMb :: (a -> IO (Maybe b)) -> ITrans a b
-- ^ lift an IO action to the ITrans arrow
arrIOMb f = transUI . return $ (>>=g)
  where
    g x = case x of
        Nothing -> return Nothing   -- hmmm
        Just y -> f y

--------------------------------------------------------------------------------

(@@@) :: (p -> x -> y) -> Generator p -> ITrans x y
-- ^ apply a pure function with parameters taken from the UI
infixl 3 @@@
f @@@ p = arr (uncurry f) <<< (transUI (fmap const p) &&& arr id)

------------------------------------------------------------------

instance ArrowChoice ITrans
  where
    left f = f +++ arr id
    f +++ g = (f >>> arr Left) ||| (g >>> arr Right)
    ITrans gf ||| ITrans gg = ITrans $ do
        Trans f <- gf
        Trans g <- gg
        let h Nothing  = return Nothing
            h (Just y) = either (f.return.Just) (g.return.Just) y
        return $ Trans $ (>>= h)

-- similar to Kleisli, explictly calling the IO function

--------------------------------------------------------------------------------

adaptMb :: (IO a -> IO b) -> (IO (Maybe a) -> IO (Maybe b))
adaptMb s = (>>= h)
  where 
    h Nothing  = return Nothing
    h (Just x) = fmap Just . s . return $ x


instance ArrowLoop ITrans
  where
    loop (ITrans gf) = ITrans $ do
        Trans f <- gf
        let f' x y = (fmap fromJust . f . return . Just) (x, snd y)
            r = (>>= liftM fst . mfix . f')
        return $ Trans $ adaptMb r

-- the same idea as above, with ugly wrapping


instance ArrowCircuit ITrans
  where
    delay x = arrL (x:)


delay' :: ITrans a a
-- ^ delay initialized with the first element (not suitable for ArrowLoop/ArrowCircuit)
delay' = arrL f
  where
    f [] = []
    f (a:as) = a:a:as

--------------------------------------------------------------------------------

runITrans :: ITrans a b -> [a] -> IO [b]
runITrans (ITrans gt) as = do
    Trans t <- gt
    x <- mkGenerator as
    lazyList (t x)


runT_ :: Generator a -> ITrans a b -> IO ()
-- ^ run a camera generator on a transformer
runT_ gcam gt = runIt $ do
    bs <- runS gcam gt
    forkIO $ mapM_ g bs >> leaveMainLoop
  where
    g !_x = putStr ""


-- ^ run process without fork, (needs explicit "prepare").
-- This is currently required for certain GPU applications.
runNT :: Generator a -> ITrans a b -> IO [b]
runNT gcam gt = do
    bs <- runS gcam gt
    Control.Monad.mapM g bs
  where
    g !x = putStr "" >> mainLoopEvent >> return x


runNT_ :: Generator a -> ITrans a b -> IO ()
-- ^ similar to runNT, discarding the results
runNT_ gcam gt = do
    bs <- runS gcam gt
    mapM_ g bs
  where
    g !_x = putStr "" >> mainLoopEvent


runT :: Generator a -> ITrans a b -> IO [b]
-- ^ run a camera generator on a transformer, returning the results in a lazy list
runT gcam gt = do
    rbs <- newChan
    forkIO $ runIt $ do
        bs <- runS gcam gt
        forkIO $ mapM_ (writeChan rbs . Just) bs >> writeChan rbs Nothing >> leaveMainLoop
    getChanContents' rbs

-- to detect end of stream
getChanContents' :: Chan (Maybe a) -> IO [a]
getChanContents' ch
  = unsafeInterleaveIO $ do
        mx  <- readChan ch
        case mx of
          Just x -> do
            xs <- getChanContents' ch
            return (x:xs)
          Nothing -> return []



runS :: Generator a -> ITrans a b -> IO [b]
-- ^ runT without the GUI (run silent) 
runS gcam (ITrans gt) = do
    cam <- gcam
    Trans t <- gt
    r <- lazyList (t cam)
    return r

