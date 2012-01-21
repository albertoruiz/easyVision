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
    -- * Arrow Interface
    runT_, runT, Trans(..), transUI, arrL, (@@@),
    -- * IO source tools
    (~~>), (~>), (>~~>), (>~>), (.&.), (.@.), grabAll
)where

import ImagProc.Ipp.Core
import ImagProc.Ipp.Convert(loadRGB)
import ImagProc.Generic(Channels,channels,GImg,toYUV,channelsFromRGB)
import ImagProc.Camera
import System.IO.Unsafe(unsafeInterleaveIO)
import Data.List(isPrefixOf,foldl',tails,findIndex,isInfixOf,isSuffixOf)
import Data.Maybe
import System.Directory(doesFileExist, getDirectoryContents)
import System.CPUTime
import Text.Printf
import Control.Applicative((<$>))
import System.Environment(getArgs,getEnvironment)
import Data.Function(on)
import Control.Concurrent
import Data.IORef
import ImagProc.C.UVC
import Util.Options
import System.Exit

import Vision.GUI.Interface

import qualified Control.Category as Cat
import Control.Arrow
import Control.Monad
import Data.Either(lefts,rights)

--------------------------------------------------------------------------------

-- | transformation of a sequence
newtype Trans a b = Trans ( [a] -> IO [b] )

instance Cat.Category Trans
  where
    id = Trans return 
    Trans f . Trans g = Trans ( g >=> f )

instance Arrow Trans
  where
    arr f = Trans (return . map f)
    first f = f *** Cat.id
    (***) (Trans s) (Trans t) = Trans r
      where
        r = g . (s *** t) . unzip
        g (a,b) = do
            as <- a
            bs <- b
            return (zip as bs)


arrL :: ([a]->[b]) -> Trans a b
-- ^ pure function on the whole list of results
arrL f = Trans (return . f)


transUI :: VC a b -> Trans a b
-- ^ convert IO interface to a transformer of lazy lists
transUI ui = Trans $ source . (createGrab >=> ui)



runT_ :: IO (IO a) -> Trans a b -> IO ()
-- ^ run a camera generator on a transformer
runT_ gcam (Trans t) = runIt $ do
    as <- source gcam
    bs <- t as
    f <- createGrab bs
    forkIO (forever $ f >>= g )
  where
    g !_x = putStr ""



runT :: IO (IO a) -> Trans a b -> IO [b]
-- ^ run a camera generator on a transformer, returning the results
runT = error "not yet implemented"

{-
-- TO DO: FIXME
runT gcam (Trans t) = do
    rbs <- newChan
    forkIO $ runIt $ do
        as <- source gcam
        bs <- t as
        f <- createGrab bs
        forkIO $ forever (f >>= writeChan rbs)
    getChanContents rbs
-}    


(@@@) :: (p -> x -> y) -> IO (IO p) -> Trans x y
-- ^ apply a pure function with parameters taken from the UI
infixl 3 @@@
f @@@ p = arr (uncurry f) <<< (transUI (const p) &&& Cat.id)

------------------------------------------------------------------


instance ArrowChoice Trans where
    left f = f +++ arr id
    Trans f +++ Trans g = Trans $ \xs -> do
        ls <- f (lefts xs)
        rs <- g (rights xs)
        return (merge ls rs xs)

merge :: [a] -> [b] -> [Either x y] -> [Either a b]
merge _  _ [] = []
merge ls rs (x:xs) =
    case x of
         Right _ -> Right (head rs): merge ls (tail rs) xs
         Left  _ -> Left  (head ls): merge (tail ls) rs xs


--------------------------------------------------------------------------------

createGrab :: [b] -> IO (IO b)
createGrab l = do
    pl <- newIORef l
    return $ do
        r <- readIORef pl
        case r of
          h:t -> do writeIORef pl t
                    return h
          []  -> exitWith ExitSuccess

grabAll :: IO t -> IO [t]
grabAll grab = do
    im <- grab
    rest <- unsafeInterleaveIO (grabAll grab)
    return (im:rest)


source :: IO (IO x) -> IO [x]
-- ^ convert a camera generator into a lazy list
source gencam = do
    cam <- gencam
    xs <- grabAll cam
    return xs

--------------------------------------------------------------------------------


-- | Creates a virtual camera by some desired processing of the infinite list of images produced by another camera.
virtualCamera :: ([a]-> [b]) -> IO a -> IO (IO b)
virtualCamera filt grab = filt `fmap` grabAll grab >>= createGrab

-- | shortcut for @>>= virtualCamera f@
(~~>) :: IO (IO a) -> ([a]-> [b]) -> IO (IO b)
infixl 1 ~~>
gencam ~~> f = (gencam >>= virtualCamera f)

-- | shortcut for @>>= virtualCamera (map f)@, or equivalently, @>>= return . fmap f@.
(~>) :: IO (IO a) -> (a-> b) -> IO (IO b)
infixl 1 ~>
gencam ~> f = gencam >>= return . fmap f

-- | composition version of @~>@
(>~>) :: (t -> IO (IO a)) -> (a -> b) -> t -> IO (IO b)
infixr 2 >~>
f >~> g = \x -> f x ~> g

-- | composition version of @~~>@
(>~~>) :: (t -> IO (IO a)) -> ([a] -> [b]) -> t -> IO (IO b)
infixr 2 >~~>
f >~~> g = \x -> f x ~~> g

-- | \"union\" of virtual cameras
(.&.) :: IO (IO a) -> IO (IO b) -> IO (IO (a, b))
infixl 0 .&.
(.&.) = liftM2 (liftM2 (,))


-- | a combinator which is useful to compute a pure function on the input stream, 
--     with parameters taken from an interactive window.
(.@.) :: (a -> b -> c) -> IO (IO a) -> IO b -> IO (IO (b, c))
f .@. wp = (wp .&. ) . return >~> snd &&& uncurry f


