-----------------------------------------------------------------------------
{- |
Module      :  Util.LazyIO
Copyright   :  (c) Alberto Ruiz 2012
License     :  GPL
Maintainer  :  Alberto Ruiz (aruiz at um dot es)

-}
-----------------------------------------------------------------------------

module Util.LazyIO (
    Generator,
    mkGenerator, lazyList,
    virtualCamera,
    (~~>), (~>), (>~~>), (>~>), (.&.), (.@.),
) where

import Control.Arrow     ((&&&))
import Control.Monad     (liftM2)
import Data.IORef    
import System.IO.Unsafe  (unsafeInterleaveIO)

--------------------------------------------------------------------------------

type Generator x = IO (IO (Maybe x))

mkGenerator :: [x] -> Generator x
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


-- | Creates a virtual camera by some desired processing of the infinite list of images produced by another camera.
virtualCamera :: ([a]-> [b]) -> IO (Maybe a) -> Generator b
virtualCamera filt grab = filt `fmap` lazyList grab >>= mkGenerator

-- | shortcut for @>>= virtualCamera f@
(~~>) :: Generator a -> ([a]-> [b]) -> Generator b
infixl 1 ~~>
gencam ~~> f = (gencam >>= virtualCamera f)

-- | shortcut for @>>= virtualCamera (map f)@, or equivalently, @>>= return . fmap f@.
(~>) :: Generator a -> (a-> b) -> Generator b
infixl 1 ~>
gencam ~> f = gencam >>= return . fmap (fmap f)

-- | composition version of @~>@
(>~>) :: (t -> Generator a) -> (a -> b) -> t -> Generator b
infixr 2 >~>
f >~> g = \x -> f x ~> g

-- | composition version of @~~>@
(>~~>) :: (t -> Generator a) -> ([a] -> [b]) -> t -> Generator b
infixr 2 >~~>
f >~~> g = \x -> f x ~~> g

-- | \"union\" of virtual cameras
(.&.) :: Generator a -> Generator b -> Generator (a, b)
infixl 0 .&.
(.&.) = liftM2 . liftM2 . liftM2 $ (,)


-- | a combinator which is useful to compute a pure function on the input stream, 
--     with parameters taken from an interactive window.
(.@.) :: (a -> b -> c) -> Generator a -> IO (Maybe b) -> Generator (b, c)
f .@. wp = (wp .&. ) . return >~> snd &&& uncurry f

