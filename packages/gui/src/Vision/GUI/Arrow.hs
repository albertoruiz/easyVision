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
    runT_, runT, Trans(Trans), transUI, arrL, (@@@)
)where

import Control.Concurrent   (forkIO)
import Util.LazyIO          (createGrab, grabAll)
import Vision.GUI.Interface (runIt,VC)

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


--------------------------------------------------------------------------------

source :: IO (IO x) -> IO [x]
-- ^ convert a camera generator into a lazy list
source gencam = do
    cam <- gencam
    xs <- grabAll cam
    return xs


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

