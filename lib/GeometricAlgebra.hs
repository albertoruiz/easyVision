-- experiments with Geometric Algebra

module GeometricAlgebra(
    Multivector,
    ms, mv, e, rever, rotor, meet, (/\), (\/), (-|)
) where

import Numeric.LinearAlgebra(fromList,toList,reshape,inv,(<>),(<\>),rank,trans)
import Tensor hiding ((/\),(\/))
import Data.List
import Control.Monad(filterM)
import Data.Function(on)
import Text.Printf(printf)
import qualified Data.Map as Map

powerset = filterM (const [True, False])  -- !!

base :: Int -> [[Int]]
base k = sortBy (compare `on` length) (powerset [1..k])

base' k = map (\b -> MV [(1,b)]) (base k)


data Multivector = MV [(Double,[Int])] deriving Eq

grade (MV l) = maximum . map (length.snd) $ l

maxDim (MV [(_,[])]) = 0
maxDim (MV l) = maximum . concat . map snd $ l

rever (MV l) = MV (map r l) where
    r (c,b) = (c*fromIntegral s,b)
        where s = signum (-1)^(k*(k-1)`div`2)
              k = length b


instance Show Multivector where
    show (MV l) = concatMap (shbld) l
        where shbld (c,[]) = show c
              shbld (c,l) = shsign c ++ showc c ++ "e"++ concatMap show l
              shsign c = if c > 0 then " +" else " -"
              showc c
                  | abs c == 1  = ""
                  | otherwise = printf "%.2f" (abs c)

ms s = MV [(s,[])]
mv v = MV $ simplify $ zip v (map (:[]) [1..])


-- different product rules

-- reorders the base indices remembering the original position
r1 :: [Int] -> [(Int,[Int])]
r1 [] = []
r1 l = (m,elemIndices m l):(r1 (filter (/=m) l))
    where m = minimum l

-- geometric product
r2 :: [(Int, [Int])] -> (Double, [Int])
r2 = foldl' g (1,[])
    where g (k,l) (e,ps) = (k*s,l++t)
              where t = if even (length ps) then [] else [e]
                    s = product (map f ps')
                         where f x = if even x then 1 else -1
                               ps' = zipWith (subtract) ps [0..]

-- exterior product
r3 :: [(Int, [Int])] -> (Double, [Int])
r3 = foldl' g (1,[])
    where g (k,l) (e,ps) = (k*s,l++[e])
              where s = if length ps > 1 then 0 else if even (head ps) then 1 else -1


-- simplification and cleaning of the list of coordinates
simplify = chop . grp . sortBy (compare `on` snd)
    where grp [] = []
          grp [a] = [a]
          grp ((c1,b1):(c2,b2):rest)
              | b1 == b2  = grp ( (c1+c2,b1) : rest)
              | otherwise = (c1,b1): grp ((c2,b2):rest)
          zero (c,_) = abs c < 1E-8
          chop = cz . filter (not.zero)
          cz [] = [(0,[])]
          cz x  = x

-- sum of multivectors
gs (MV l1) (MV l2) = MV $ simplify (l1++l2)

-- geometric product
gp (MV l1) (MV l2) = MV $ simplify [g x y | x<-l1, y <-l2]
    where g (c1,b1) (c2,b2) = (k*c1*c2,b3) where (k,b3) = gpr b1 b2 --(r2.r1) (b1++b2)

-- exterior product
ge (MV l1) (MV l2) = MV $ simplify [g x y | x<-l1, y <-l2]
    where g (c1,b1) (c2,b2) = (k*c1*c2,b3) where (k,b3) = epr b1 b2 -- (r3.r1) (b1++b2)

-- contraction inner product
gi (MV l1) (MV l2) = sum [g x y | x<-l1, y <-l2]
    where g (c1,[]) (c2,[]) = MV [(c1*c2,[])]
          g (c1,[i]) (c2,[]) = 0
          --g (c1,[]) (c2,[i]) = MV [(c1*c2,[i])]
          g (c1,[]) (c2,is) = MV [(c1*c2,is)]
          g (c1,[i]) (c2,[j]) = if i==j then MV [(c1*c2,[])] else 0
          g (c1,[i]) (c2,j:js) = g (c1,[i]) (c2,[j]) /\ MV [(1,js)]
                               - MV [(c2,[j])] /\ g (c1,[i]) (1,js)
          g (c1,i:is) b = gi (MV [(c1,[i])]) (gi (MV[(1,is)]) (MV [b]))


instance Num Multivector where
    (+) = gs
    (*) = gp
    negate (MV l) = MV (map neg l) where neg (k,b) = (-k,b)
    abs x = error "abs of multivector not yet defined"
    signum x = error "signum of multivector not yet defined"
    fromInteger x = MV [(fromInteger x,[])]

instance Fractional Multivector where
    fromRational x = MV [(fromRational x,[])]
    recip (MV [(x,[])]) = MV [(recip x,[])]
    recip x = mvrecip x

e k = MV [(1,[k])]

infixl 7 /\
(/\) = ge

infixl 7 \/
a \/ b = i*((i*b) /\ (i*a))

infixl 7 -|
(-|) = gi


full k = product . map e $ [1 .. k]

i = full 4

meet k a b = (b -| rever (full k)) -| a

-- check that it is a vector
normVec v = sqrt x where MV [(x,[])] = v * v

unitary v = v / ms (normVec v)


-- memoization of the rules
table k = [[ (r2.r1) (x++y) | y<-b] | x<-b]
    where b = base k

tableInner k = [[ x -| y | y<-b] | x<-b]
    where b = base' k

-- the same with maps
gprules k = Map.fromList [(x, Map.fromList [(y,(r2.r1)(x++y)) | y<-base k] )| x<-base k]

eprules k = Map.fromList [(x, Map.fromList [(y,(r3.r1)(x++y)) | y<-base k] )| x<-base k]

--reasonable limit
gpr a b = g Map.! a Map.! b
    where g = gprules 6

epr a b = g Map.! a Map.! b
    where g = eprules 6

----------------------- tensor expansion -----------------------

expand k = g
    where g (MV l) = foldl1' (zipWith (+)) $ map f l
          basepos b = m Map.! b
          m = baseraw k
          f (c,b) = en pk (basepos b) c
          pk = 2^k
          baseraw k = Map.fromList $ zip (base k) [0..]
          en n k v = replicate k 0 ++ v : replicate (n-k-1) 0

compact k t = sum $ zipWith (*) (map ms $ toList (coords t)) (base' k)

gatensor k = tensor [-pk,-pk,pk] . fromList . concat . concat  $ gacoords k
    where pk = 2^k
          gacoords k = [[ f (x * y) | y<-b] | x<-b]
          b = base' k
          f = expand k

tmv k x = tensor [2^k] (fromList (expand k x))


tp a b = comp (g!"ijk" * ta!"i" * tb!"j")
    where k = max (maxDim a) (maxDim b)
          g = gatensor k
          ta = tmv k a
          tb = tmv k b
          comp = compact k

mat rowidx t = reshape c $ coords t'
    where c = idxDim $ last (dims t')
          t' = tridx [rowidx] t

-- on the right
pmat k b = mat "k" $ g!"ijk" * tb!"j"
    where g = gatensor k
          tb = tmv k b

divi k a b = compact k $ tensor [2^k] $ pmat k b <\> coords (tmv k a)

mvrecip b = divi (maxDim b) 1 b

rotor k phi axis = ms (cos (phi/2)) - ms (sin (phi/2)) * (unitary axis*full k)
