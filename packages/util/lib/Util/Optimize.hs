-----------------------------------------------------------------------------
{- |
Module      :  Util.Optimize
Copyright   :  (c) Alberto Ruiz 2010
License     :  GPL

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional

Auxiliary tools for iterative optimization

-}
-----------------------------------------------------------------------------

module Util.Optimize (
    optimize, optimize2,
    optimizeLM
) where

import Util.Misc(impossible)

convergence :: (Ord t1, Ord t2) => t1 -> t2 -> [(t, t1, t2)] -> [t1] -> (t, [t1])
convergence _ _ [] _  = impossible "convergence"
convergence _ _ [(s,e,_d)] prev = (s,e:prev)
convergence epsabs epsrel ((s,e,d):ss) prev
    | e < epsabs = (s, e:prev)
    | d < epsrel = (s, e:prev)
    | otherwise = convergence epsabs epsrel ss (e:prev)


optimize :: Double        -- ^ absolute tolerance
         -> Double        -- ^ relative tolerance
         -> Int           -- ^ maximum number of interations
         -> (x -> x)      -- ^ method
         -> (x -> Double) -- ^ error function
         -> x             -- ^ starting point
         -> (x, [Double]) -- ^ solution and error history
optimize epsabs epsrel maxit method errfun s0 = (sol,e) where
    sols = take (max 1 (1+maxit)) $ iterate method s0
    errs = map errfun sols
    deltas = 100 : zipWith f errs (tail errs) where f e1 e2 = abs (100*(e1 - e2)/e1)
    (sol,e) = convergence epsabs epsrel (zip3 sols errs deltas) []


optimize2 :: Double             -- ^ absolute tolerance
          -> Double             -- ^ relative tolerance
          -> Int                -- ^ maximum number of interations
          -> (x -> (x,Double))  -- ^ method (new solution, error)
          -> x                  -- ^ starting point
          -> (x, [Double])      -- ^ solution and error history
-- ^ a version of "optimize" in which the iteration function also computes the error
optimize2 epsabs epsrel maxit method s0 = (fst sol, e)
  where
    (sol, e) = optimize epsabs epsrel maxit (method.fst) snd (method s0)


optimizeLM :: Double        -- ^ absolute tolerance
           -> Double        -- ^ relative tolerance
           -> Int           -- ^ maximum number of interations
           -> (t -> x -> x) -- ^ method, depends on parameter
           -> (x -> Double) -- ^ error function
           -> x             -- ^ starting point
           -> t             -- ^ initial parameter (e.g. 0.001)
           -> (t->t)  -- ^ parameter update strategy if cost decreases (eg. (/2))
           -> (t->t)  -- ^ parameter update strategy if cost increases (eg. (10*))
           -> (x, [Double]) -- ^ solution and error history
optimizeLM epsabs epsrel maxit method errfun s0 lambda0 decf incf = r where
    w0 = (lambda0, (s0, errfun s0, 100))

    ws = take (max 1 (1+maxit)) $ map snd $ iterate next w0

    next (l,(s,e,d)) = result where
        s' = method l s
        e' = errfun s'
        d' = abs (100*(e - e')/e)
        l' = updateLM decf incf l e e'
        result | e' < e  =   (l', (s',e',d'))
               | otherwise = next (l', (s,e,d))

    r = convergence epsabs epsrel ws []

    updateLM df uf l e e' = if e' > e then uf l else df l

