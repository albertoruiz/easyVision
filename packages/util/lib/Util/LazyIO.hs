-----------------------------------------------------------------------------
{- |
Module      :  Util.LazyIO
Copyright   :  (c) Alberto Ruiz 2012
License     :  GPL
Maintainer  :  Alberto Ruiz (aruiz at um dot es)

-}
-----------------------------------------------------------------------------

module Util.LazyIO (
    mkGenerator, lazyList,
    virtualCamera,  grabAll, createGrab,
    (~~>), (~>), (>~~>), (>~>), (.&.), (.@.),
) where

import Control.Arrow     ((&&&))
import Control.Monad     (liftM2)
import Data.IORef    
import System.Exit       (exitWith, ExitCode(ExitSuccess))
import System.IO.Unsafe  (unsafeInterleaveIO)

--------------------------------------------------------------------------------

mkGenerator :: [x] -> IO (IO (Maybe x))
mkGenerator l = do
    pl <- newIORef l
    return $ do
        r <- readIORef pl
        case r of
          h:t -> do writeIORef pl t
                    return (Just h)
          []  -> return Nothing


lazyList :: IO (Maybe t) -> IO [t]
lazyList grab = do
    x <- grab
    case x of
        Just im -> do rest <- unsafeInterleaveIO (lazyList grab)
                      return (im:rest)
        Nothing -> return []

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

