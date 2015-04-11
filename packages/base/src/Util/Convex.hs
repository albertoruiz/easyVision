{-# LANGUAGE ViewPatterns, RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Util.Convex where

import Numeric.LinearAlgebra.HMatrix hiding (hess)
import Control.Arrow((&&&))
--import Util.Debug(debugMat, debug)

type V = Vector Double
type M = Matrix Double

vect :: [ℝ] -> V
vect = vector

mat:: Int -> [ℝ] -> Matrix ℝ
mat = matrix

elems :: V -> [Double]
elems = toList

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil q xs = a++ take 1 b
  where
    (a,b) = break q xs

--------------------------------------------------------------------------------

data FunAp = FunAp
  { evF :: V -> Double
  , evG :: V -> V
  , evH :: V -> M
  }

--------------------------------------------------------------------------------

data QuadAp = QuadAp { q0 :: Double, q1 :: V, q2 :: M } deriving Show

evQ0 :: QuadAp -> V -> Double
evQ0 QuadAp{..} z = q0 + q1<·>z + z<·>q2 #>z / 2

evQ1 :: QuadAp -> V -> V
evQ1 QuadAp{..} z = q1 + q2 #> z

evQ2 :: QuadAp -> V -> M
evQ2 QuadAp{..} _ = q2


mkQuadFun :: QuadAp -> FunAp
mkQuadFun q = FunAp{..}
  where
    evF = evQ0 q
    evG = evQ1 q
    evH = evQ2 q

--------------------------------------------------------------------------------

quadAt :: V -> Double -> V -> M -> QuadAp
quadAt y v g h = QuadAp {..}
  where
    q0 = v - g <·> y + y <·> h #>y /2
    q1 = g - tr h #> y
    q2 = h

--------------------------------------------------------------------------------

mkQuadAt :: V -> Double -> V -> M -> FunAp
mkQuadAt y v g h = mkQuadFun (quadAt y v g h)


-- a x + b (< 0)
mkLin :: V -> Double -> FunAp
mkLin a b = mkQuadAt (konst 0 (size a)) b a (konst 0 (size a, size a))

--------------------------------------------------------------------------------

barrier0Debug :: [FunAp] -> V -> Double
barrier0Debug [] _ = 0
barrier0Debug fs z = negate . sum $ map (logp . negate . flip evF z) fs
  where
    logp z' = if z' <= 0 then (5) else log z'


barrier0 :: [FunAp] -> V -> Double
barrier0 [] _ = 0
barrier0 fs z = negate . sum $ map (log . negate . flip evF z) fs


barrier1 :: [FunAp] -> V -> V
barrier1 [] z = konst 0 (size z)
barrier1 fs z = - sum (zipWith scale rfs gs)
  where
    rfs = map (recip . flip evF z) fs
    gs  = map (flip evG z) fs


barrier2 :: [FunAp] -> V -> M
barrier2 [] z = konst 0 (size z, size z)
barrier2 fs z = sum (zipWith scale rf2 g2) - sum (zipWith scale rfs hs)
  where
    rfs = map (recip . flip evF z) fs
    rf2 = zipWith (*) rfs rfs
    gs  = map (flip evG z) fs
    g2  = map (\x -> outer x x) gs
    hs  = map (flip evH z) fs



cost :: FunAp -> [FunAp] -> Double -> V -> Double
cost f cs t z = t * evF f z + barrier0 cs z

grad :: FunAp -> [FunAp] -> Double -> V -> V
grad f cs t z = scalar t * evG f z + barrier1 cs z

--------------------------------------------------------------------------------


newtonStepBarrier
    :: FunAp
    -> [FunAp]
    -> M
    -> V
    -> Double
    -> V
    -> (Double, V)
newtonStepBarrier f cs a b t x = (dec2,dx)
  where
    [dx] = takesV [size x] sol
    sol = m <\> rh
    gf = evG f x
    gb = barrier1 cs x   -- //> "gb"
    hf = evH f x     
    hb = barrier2 cs x  -- /// "hessb"
    h = scalar t * hf + hb
    g = scalar t * gf + gb
    m = fromBlocks [[ h , tr a ]
                   ,[ a ,  0   ]]
    rh = - vjoin [ g ,  res]
    res = a #>x -b
    dec2 = dx <·> h #> dx

--deb msg = debug msg (minElement.eigenvaluesSH)

--infix 0  ///, //>
--m /// msg = debugMat msg 2 id m
--v //> msg = debugMat msg 2 asRow v

backtrack
    :: Double
    -> Double
    -> FunAp
    -> [FunAp]
    -> Double
    -> V
    -> V
    -> (Double, V, Double, Double)
backtrack α β f cs t x dx = head $ dropWhile nok sxf
  where
    f0 = cost f cs t x
    g0 = grad f cs t x <·> dx
    ss = iterate (* β) 1
    sxf = [ (s, x', cost f cs t x', f0+α*s*g0) | s <- ss, let x' = x + scale s dx, ok x' ]
    ok z = all ((<0) . flip evF z) cs
    nok (_,_,v1,v2) = v1 > v2 



nextNewton
    :: Double
    -> Double
    -> Double
    -> FunAp
    -> [FunAp]
    -> M
    -> V
    -> Double
    -> V
    -> [V]
nextNewton eps α β f cs a b t x
    | {- debug "dec2" id -} dec2/2 <= eps = []
    | otherwise = x' : nextNewton eps α β f cs a b t x'
  where
    (dec2,dx) = newtonStepBarrier f cs a b t x
    (_,x',_,_) = backtrack α β f cs t x dx


interiorBarrierPhaseII
    :: Double
    -> Double
    -> Double
    -> Double
    -> Double
    -> FunAp
    -> [FunAp]
    -> M
    -> V
    -> Vector Double
    -> [[V]]
interiorBarrierPhaseII α β μ μ0 ϵ  f fs a b x0 = outerloop
  where
    m = size x0
    ts = takeWhile (<2*fromIntegral m / ϵ) (iterate (*μ) μ0)
    outerloop = scanl g [x0] ts
      where
        g [] _ = []
        g z t = nextNewton ϵ α β f fs a b t (last z)

--------------------------------------------------------------------------------


pathDetails :: (t, [FunAp]) -> [[V]] -> [Matrix Double]
pathDetails (_,fs) path = filter ((>0).rows) $ zipWith (¦) (map fromRows path) (map fromRows cons)
  where
    howcons z = fromList $ map (flip evF z) fs
    cons = map (map howcons) path

interiorBarrierII
    :: Double
    -> Double
    -> Double
    -> Double
    -> Double
    -> FunAp
    -> [FunAp]
    -> M
    -> V
    -> Vector Double
    -> [Matrix Double]
interiorBarrierII α β μ μ0 ϵ  f fs a b x0 = pathDetails (fs,fs) path
  where
    path = interiorBarrierPhaseII α β μ μ0 ϵ  f fs a b x0

interiorBarrierPhaseIa
    :: (([FunAp], [FunAp]) -> [[V]] -> t2)
    -> Double
    -> Double
    -> Double
    -> Double
    -> Double
    -> t
    -> [FunAp]
    -> Matrix t3
    -> t1
    -> Vector Double
    -> t2
interiorBarrierPhaseIa what α β μ μ0 ϵ  _ fs a _ x0
    | rows a > 0 = error "phase I not yet implemented with equality constraints"
    | otherwise = what (fs,fs') path
  where
    path = interiorBarrierPhaseII α β μ μ0 ϵ fun fs' a' b' x0'
    nc = length fs
    nv = size x0
    fun = mkLin (vjoin [konst 0 nv, konst 1 nc]) 0
    e k n = assoc n 0 [(k,1)]
    fs' = zipWith extend fs [0..]  ++ [ mkLin (vjoin [konst 0 nv, -e k nc]) 0 | k <- [0..nc-1] ] 
    a' = mat (nc+nv) []
    b' = vect []
    x0' = vjoin [ x0, vect $ map (max 10 . (* 2) . flip evF x0) fs]
    extend (FunAp f g h) k = FunAp {..}
      where
        zero = konst 0 (nc,nc)
        vars z = (head &&& last) $ takesV [nv,nc] z
        evF (vars -> (x,s)) = f x - s!k
        evG (vars -> (x,_)) = vjoin [g x, - e k nc]
        evH (vars -> (x,_)) = fromBlocks [[h x,  0  ]
                                         ,[ 0 , zero]]


interiorBarrierI
    :: Double
    -> Double
    -> Double
    -> Double
    -> Double
    -> t
    -> [FunAp]
    -> Matrix t2
    -> t1
    -> Vector Double
    -> [Matrix Double]
interiorBarrierI α β μ μ0 ϵ  f fs a b x0
    = interiorBarrierPhaseIa pathDetails α β μ μ0 ϵ  f fs a b x0


interiorBarrierPhaseIb
    :: (([FunAp], [FunAp]) -> [[V]] -> t2)
    -> Double
    -> Double
    -> Double
    -> Double
    -> Double
    -> t
    -> [FunAp]
    -> Matrix t3
    -> t1
    -> Vector Double
    -> t2
interiorBarrierPhaseIb what α β μ μ0 ϵ  _ fs a _ x0
    | rows a > 0 = error "phase I not yet implemented with equality constraints"
    | otherwise = what (fs,fs') path
  where
    path = interiorBarrierPhaseII α β μ μ0 ϵ fun fs' a' b' x0'
    nv = size x0
    fun = mkLin (vjoin [konst 0 nv, 1]) 0
    fs' = map extend fs  ++ [ mkLin (vjoin [konst 0 nv, -1]) 0 ] 
    a' = mat (nv+1) []
    b' = vect []
    x0' = vjoin [ x0, scalar $ maximum $ map (max 10 . (* 2) . flip evF x0) fs]
    extend (FunAp f g h) = FunAp {..}
      where
        vars z = (head &&& last) $ takesV [nv,1] z
        evF (vars -> (x,s)) = f x - s!0
        evG (vars -> (x,_)) = vjoin [g x, - 1]
        evH (vars -> (x,_)) = fromBlocks [[h x,  0 ]
                                         ,[ 0 ,  0 ]]

interiorBarrierIb
    :: Double
    -> Double
    -> Double
    -> Double
    -> Double
    -> t
    -> [FunAp]
    -> Matrix t2
    -> t1
    -> Vector Double
    -> [Matrix Double]
interiorBarrierIb α β μ μ0 ϵ  f fs a b x0
    = interiorBarrierPhaseIb pathDetails α β μ μ0 ϵ  f fs a b x0

untilOk :: ([FunAp], t) -> [[Vector Double]] -> [Vector Double]
untilOk (fs,_) = takeUntil feasible . concat
  where
    feasible z = all g fs
       where
         x = subVector 0 (size z -1) z
         g f = evF f x < 0

interiorBarrier
    :: Double
    -> Double
    -> Double
    -> Double
    -> Double
    -> FunAp
    -> [FunAp]
    -> Matrix Double
    -> V
    -> Maybe V
interiorBarrier α β μ μ0 ϵ f fs a b 
    | feasible x0 = Just (last $ concat $ interiorBarrierPhaseII α β μ μ0 ϵ f fs a b x0)
    | otherwise = Nothing
  where
    n = cols a
    x0 = subVector 0 n $ last $ interiorBarrierPhaseIb untilOk α β μ0 μ ϵ f fs a b (konst 0 n)
    feasible x = all ((<0) . flip evF x) fs

solveL_Inf' :: Matrix Double -> V -> V
solveL_Inf' a b = sol
  where
    n = cols a
    as = toRows a
    bs = elems b
    mk sgn a_i b_i = mkLin (vjoin[scale sgn a_i, vect[-1]]) (-sgn * b_i)
    f = mkLin (vjoin [konst 0 n, 1]) 0
    fs = zipWith (mk 1) as bs ++ zipWith (mk (-1)) as bs
    Just sol = optimizer 1E-4 f fs (mat (n+1) []) (fromList[]::V)

solveL_Inf :: Matrix Double -> V -> Vector Double
solveL_Inf a b = subVector 0 (cols a) $ solveL_Inf' a b

solveL_1' :: Matrix Double -> V -> V
solveL_1' a b = sol
  where
    n = cols a
    ns = rows a
    as = toRows a
    bs = elems b
    mk sgn k a_i b_i = mkLin (vjoin[scale sgn a_i, -e k]) (-sgn * b_i)
    f = mkLin (vjoin [konst 0 n, konst 1 ns]) 0
    fs = zipWith3 (mk 1) ks as bs ++ zipWith3 (mk (-1)) ks as bs
    ks = [0..]
    e k = assoc ns 0 [(k,1)]
    Just sol = optimizer 1E-4 f fs (mat (n+ns) []) (fromList[]::V)

solveL_1 :: Matrix Double -> V -> Vector Double
solveL_1 a b = subVector 0 (cols a) $ solveL_1' a b

optimizer :: Double -> FunAp -> [FunAp] -> Matrix Double -> V -> Maybe V
optimizer = interiorBarrier α β μ0 μ
  where
    α = 0.01
    β = 0.8
    μ0 = 10
    μ = 10

