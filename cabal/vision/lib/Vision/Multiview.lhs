> {-# LANGUAGE TypeSynonymInstances, FlexibleInstances, NoMonomorphismRestriction #-}

> module Vision.Multiview where

> import Numeric.LinearAlgebra.Exterior
> import qualified Numeric.LinearAlgebra as LA
> import Numeric.LinearAlgebra hiding ((.*))
> import Numeric.LinearAlgebra.Array.Util as Array
> import Numeric.LinearAlgebra.Array.Decomposition
> import Numeric.LinearAlgebra.Array.Solve
> import Graphics.Plot(gnuplotpdf)
> import System.Random
> import Debug.Trace
> import Vision
> -- import System
> import Data.List
> import Text.Printf
> import Numeric.GSL.Minimization
> import Numeric.GSL.Differentiation
> import Control.Applicative hiding ((<|>))
> import Control.Monad hiding (join)
> import Data.Function(on)

> debugOK = True

> debug m f x = if debugOK then trace (m ++ show (f x)) x else x


> genProb n m noise [seedP, seedC, seedD, seedN] = r where
>     centers = uniformSample seedC m [(-1,1),(-1,1),(-1,-0.5)]
>     dirs    = uniformSample seedD m [(-0.1,0.1),(-0.1,0.1),(0.8,1)]
>     mcams = zipWith tcam (toRows centers) (toRows dirs)
>
>     mpoints3D = uniformSample seedP n [(-0.4,0.4),(-0.4,0.4),(0.4,1)]
>
>     cams = subindex "c" $ map ((!"vx") . fromMatrix Contra Co) mcams
>     points3D = homogT "x" $ fromMatrix Co Contra mpoints3D !"nx"
>
>     seedL = seedP * seedC + seedD*seedN -- should be another seed
>     mpoints3D' = uniformSample seedL n [(-0.4,0.4),(-0.4,0.4),(0.4,1)]
>     points3D' = homogT "x" $ fromMatrix Co Contra mpoints3D' !"nx"
>     lines3d = subindex "n" $ zipWith (/\) (parts points3D "n") (parts points3D' "n")
>     lines2d = (cams!"cu1" .* cams!"cv2") * lines3d * eps3!"uvl"

>
>     views = f noise (cams * points3D)
>         where f s = homogT "v" . addNoise seedN s . inhomogT "v"
>
>     r = VProb { p3d = points3D,
>                 p2d = views,
>                 cam = cams,
>                 l3d = lines3d,
>                 l2d = lines2d }

> randomProb n m noise = genProb n m noise <$> replicateM 4 randomIO

Homogeneous and inhomogenous versions of a tensor in a given index:

> inhomogT n t = (init `onIndex` n) t / last (parts t n)
> homogT n t = ((++[1]) `onIndex` n) t

Comparation of homogeneous transformations with the same scale:

> unitT t = t / Array.scalar (frobT t)
> frobT = pnorm PNorm2 . coords
> tnormInf = pnorm Infinity . coords

Show percents nicely:

> --dpc a b = printf " %.2f %% " (dist' a b)
> perc x = printf " %.2f %% " x

> --dist' a b = 100 * min (frobT (a - b')) (frobT (a + b')) / frobT a
> --   where b' = renameRaw b (names a)

Very frequently used tensors:

> eps3 = cov     (leviCivita 3)
> eps4 = contrav (leviCivita 4)

The multiview tensors:

We assume in all cases that the image index (dim 3) is lower than the world index (dim 4).
It is ok when we rename parts of a problem, we get "1x" etc.

> fundamental m n = unitT $
>   eps3!"rpq" * m!"pa" * m!"qb" * eps4!"abcd" *
>   n!"sc" * n!"td" * eps3!"stk"

> trifocal m n p = unitT $
>   eps4!"abcd" * m!"ia" * n!"jb" *p!"pc" * p!"qd" * eps3!"pqk"

> quadrifocal a b c d = unitT $
>   eps4!"ijlk" * a!"pi" * b!"qj" * c!"rk" * d!"sl"

> --tFundamental m n = unitT $
> --  eps3!"rpq" * m!>"pa" * m!>"qb" * eps4!"abcd" *
> --  n!>"sc" * n!>"td" * eps3!"stk"

> -- tTrifocal m n p = unitT $
> --  eps4!"abcd" * m!>"ia" * n!>"jb" *p!>"pc" * p!>"qd" * eps3!"pqk"

> -- tQuadrifocal a b c d = unitT $
> --  eps4!"ijlk" * a!>"pi" * b!>"qj" * c!>"rk" * d!>"sl"


Other tools:

> addNoise seed sigma t = t + sigma .* noise where
>     noise = mapArray (const $ randomVector seed Gaussian (dim $ coords t)) t

> sv m = singularValues m
> svt t = sv (asMatrix t)

> infixl 9 #
> ds # cs = listTensor ds cs :: Tensor Double

> psh t = putStrLn . sh $ t  -- for use at ghci prompt
> sh t = formatFixed 2 $ t  -- for use at this document
> shp t = putStrLn . formatFixed 5 $ t

> tcam c p = syntheticCamera $ easyCamera (60*degree) (f c) (f p) (20*degree)
>   where f v = (x,y,z) where [x,y,z] = toList v

> shcam cam = c where
>    (h,f) = toCameraSystem cam
>    c = ht (h <> diag (fromList [1,1,1,5])) (cameraOutline f)

> drawCameras tit ms pts = do
>   let cmd = map (f.shcam) ms
>       f c = (c,"notitle 'c1' with lines 1")
>
>   gnuplotpdf tit
>          (  "set size square; set view 72,200; "
>          ++ "set xlabel '$x$'; set ylabel '$y$'; set zlabel '$z$';"
>          ++ "set notics;"
>          ++ "splot ")
>          (cmd ++ [(pts,"notitle 'v' with points 3")])

> shProb tit p = drawCameras tit
>     (map asMatrix $ parts (cam p) "c")
>     (toLists $ asMatrix $ inhomogT "x" $ p3d p)

> correctFundamental :: Matrix Double -> Matrix Double
> correctFundamental f = f' where
>    (u,s,v) = svd f
>    s1:s2:_ = toList s
>    f' = u <> diag (fromList [s1,s2,0.0]) <> trans v


> correctFundamentalT t = applyAsMatrix correctFundamental t


> drawPoints pts1 pts2 = do
>   gnuplotpdf "points"
>          (  "set size square; set title \"The first view\";"
>          ++ "set xlabel '$x$'; set ylabel '$y$'; "
>       --   ++ "set notics;"
>          ++ "plot [-1:1] [-1:1] ")
>          [(pts1,"title 'true' with points 3"),
>           (pts2,"title 'observed' with points 1")]



> -- The algorithm in Hartley and Zisserman
> camerasFromTrifocalHZ tri = (t1,t2,t3) where
>    p3 = cameraAtOrigin
>    e1 = unitT $ epitri tri
>    e2 = unitT $ epitri (tri!>"jik") -- hmmm!!!
>    p1 = (asMatrix $ tri!>"ijk"* cov e2!"j") <|> coords e1    -- (e in l slot, in HZ)
>    p2 = (asMatrix (e2!"i"*e2!"j") - ident 3)
>          <> (asMatrix $ tri!>"ijk"* cov e1!"i") <|> coords e2
>    f = unitT . fromMatrix Contra Co
>    t1 = f p1 !"1x"
>    t2 = f p2 !"2x"
>    t3 = f p3 !"3x"

> -- image on c1 of the center of c3
> -- This method should only be used on geometrically correct trifocal tensors
> epitri tri = epi where
>     epi = lin tri (vector[0,0,1]) !"a"
>           * lin tri (vector[1,0,0])!"b" * (contrav eps3)!"abc"
>     lin tri p = linQuad tri * p!"k" * p!"K"
>     linQuad tri = (tri!>"ijk") * vector[0,0,1]!"a"
>                             * eps3!"ajJ" * (tri!>"IJK") * eps3!"iIr"


--------------------------------------------------------------------------


> -- linear autocalibration for diag f1 f2 1 based on the absolute dual quadric
> autoCalibrate p = p { p3d = (p3d p * hi)!>"yx", cam = cs' !> "yx"} where
>     cs = cam p
>     cs' = cs * h
>     hi = applyAsMatrix inv h !"yx"
>     a = -- (map (\m-> m * (setType "x" Contra m!"uy")) `onIndex` "c") cs
>         (map (\m-> m * m!"uy") `onIndex` "c") cs
>     q = --solveHomog1' infoRank (a*sel) (map return "xy")
>         solveH (a*sel) "xy"
>     h = setType "y" Co $ 
>           applyAsMatrix psqrt q

>     psqrt m | defpos = v <> diag (sqrt s')
>             | defneg = psqrt (-m)
>             | otherwise = --debug "NODEFPOS eig = " (const s') $ 
>                           ident 4
>
>        where (s,v) = eigSH' m
>              s' = fromList $ init (toList s) ++ [1]
>              [s1,s2,s3,s4] = toList s
>              defpos = s3>0 && abs s3 > abs s4
>              defneg = s2<0 && abs s2 > abs s1

> sel = (!"ruv") . cov . mkAssoc [7,3,3] . (dg++) . map (flip (,) 1) $
>       [ [0,0,1], [1,0,2], [2,1,2],
>         [3,1,0], [4,2,0], [5,2,1] ]
>   where dg = [([6,0,0],2),([6,1,1],-2)]

-------------------------------------------------------------------------




> fixRot m = p where
>   (_,r,c) = factorizeCamera m
>   p = r <|> -r<>c

> getk m = k where (k,_,_) = factorizeCamera m

> kal cs = mapTat ((!>"xw").applyAsMatrix getk) ["c"] cs

> --canord t = reorder (map iName (sort $ dims t)) t

Helper class for minimization functions.

> class Vectorable a where
>      vectorize :: a -> (a->Vector Double, Vector Double -> a)

> instance Vectorable Double where
>      vectorize x = (fromList.return,(@>0))

> instance Vectorable (Vector Double) where
>      vectorize v = (id,id)

> instance Vectorable a => Vectorable [a] where
>      vectorize xs = (f,g) where
>          (vs,us) = unzip $ map vectorize xs
>          ns = map dim $ zipWith ($) vs xs
>          f = join . zipWith ($) vs
>          g = zipWith ($) us . takesv ns

> takes [] _ = []
> takes (n:ns) xs = take n xs : takes ns (drop n xs)
> takesv ns = map fromList . takes ns . toList

> instance (Vectorable a, Vectorable b) => Vectorable (a,b) where
>      vectorize (a,b) = (f.f', g'.g) where
>            (fa,ga) = vectorize a
>            (fb,gb) = vectorize b
>            f' (a,b) = [fa a, fb b]
>            g' [u,v] = (ga u, gb v)
>            (f,g) = vectorize (f' (a,b))

> iso (f',g') x = (f.f', g'.g) where
>     (f,g) = vectorize . f' $ x

> instance (Vectorable a, Vectorable b, Vectorable c) => Vectorable (a,b,c) where
>      vectorize = iso (\(a,b,c) -> (a,(b,c)),  \(a,(b,c)) -> (a,b,c))

> instance (Vectorable (Tensor Double)) where
>      vectorize t = (f,g) where
>          f = coords
>          g v = mapArray (const v) t

Bundle adjustment ingenuo, con una funci´on de prop´osito general, y que no aprovecha la
estructura sparse. Usamos una sin derivadas, pero habr´ia que usarlas.

> data VProb = VProb { p3d :: Tensor Double,
>                      cam :: Tensor Double,
>                      p2d :: Tensor Double,
>                      l3d :: Tensor Double,
>                      l2d :: Tensor Double }

> ks = kal . cam

> quality prob = sqrt $ frobT (inhomogT "v" (p2d prob) - inhomogT "v" proj) ^2 / (n*m*2)
>    where
>        proj = cam prob * p3d prob
>        n = fromIntegral $ size "n" (p2d prob)
>        m = fromIntegral $ size "c" (p2d prob)

> data BunPar = BunPar { campar :: [[Double]], pts3D :: Tensor Double } deriving Show

> instance Vectorable BunPar where
>   vectorize = iso (\(BunPar c p) -> (c,p), \(c,p) -> BunPar c p)
>   -- vectorize x@(BunPar _ p) = iso (\(BunPar c _) -> c, \c -> BunPar c p) x

> reprocost views b = quality (probFromBun views b)

> probFromBun views (BunPar c p) = prob where
>    prob = VProb { p2d = views,
>                   cam = subindex "c" (map mkCam c),
>                   p3d = homogT "x" p,
>                   l3d = undefined, l2d = undefined}

>    mkCam [a1,a2,a3,x,y,z] = (fromMatrix Contra Co $ flipx $ syntheticCamera (CamPar 1 (a1) (a2) (a3) (x,y,z)))!"vx"


> -- NO: lo usamos calibrado asumiendo f = 1
> k0 = 1.7320508075688774

> initBunPar ct pt = BunPar c p where
>       p = inhomogT "x" $ (pt ~> "nx")
>       c = map (g.asMatrix) $ parts (ct~>"cvx") "c"
>       g m = [a1,a2,a3,x,y,z] where CamPar _ a1 a2 a3 (x,y,z) = poseFromCamera (flipx m)

> flipx = (diag (fromList [-1,1,1]) <>)

> bundle m s p = debug "steps = " (const (rows err, cost (toList xi))) $ p { p3d = newpoints, cam = newcams }
>     where ib = initBunPar (cam p) (p3d p)
>           (f,g) = vectorize ib
>           xi  = debug "dim = " dim $ f ib
>           n = dim xi
>           cost = reprocost (p2d p).g.fromList
>           (y,err) = if m == 0
>              then minimize NMSimplex2 0.001 1000 (replicate n s) cost (toList xi)
>              else minimizeD VectorBFGS2 0.001 100 0.01 0.0001 cost (gradient cost) (toList xi)
>           sol = probFromBun (p2d p) (g $ fromList y)
>           newpoints = p3d sol
>           newcams = cam sol

> -- Numerical estimation of the gradient
> gradient f v = [partialDerivative k f v | k <- [0 .. length v -1]]

> partialDerivative n f v = fst (derivCentral 0.01 g (v!!n)) where
>     g x = f (concat [a,x:b])
>     (a,_:b) = splitAt n v


> pairs _ [] = []
> pairs _ [_] = []
> pairs f (x:xs) = map (f x) xs ++ pairs f xs


The end.

> subspaceEssen p = f $ outers $ renameParts "c" (p2d p~>"cnv") "v" ""
>   where f t = subindex "p" $ solveHomog t ["1","2"] (Right 6)
>
> paramEssen = g . subspaceEssen
>   where g t = \[a,b] -> [3]#[1,a,b]!"p" * t
>
> essenCost t = (s1-s2+s3)/(s1+s2+s3) where [s1,s2,s3] = toList (svt t)

> --sol5 p i = minimizeD VectorBFGS2 0.00001 100 0.01 0.0001 (essenCost.paramEssen p) (gradient (essenCost.paramEssen p)) i
> sol6 p i = -- debug "steps: " (rows.snd) $
>            minimize NMSimplex2 0.0001 50 [0.2,0.2] (essenCost.paramEssen p) i

> explore6 r n p = [fst $ sol6 p [a,b] | a <- xs, b <- xs]
>   where xs = toList (linspace n (-r,r))

> bestEssen r n p = paramEssen p $ bestParams r n p

> bestParams r n p = minimumBy (compare `on` (essenCost.paramEssen p)) $ explore6 r n p


> -- assuming calibrated prob
> trueEssen p = fundamental c1 c2 !"21" where
>   [c1,c2] = take 2 $ parts (cam p) "c"

> truePars p = f $ toList $ coords $ contrav (trueEssen p) * subspaceEssen p
>     where f [a,b,c] = [b/a,c/a]

> exper6 r n = do
>    p <- calibrate <$> randomProb 6 2 0
>    let te = trueEssen p
>        be = unitT $ bestEssen r n p
>        diff = min (frobT (te-be)) (frobT(te+be))
>        cost = essenCost be
>        bp = truePars p
>    printf "E = %.2g, C = %.2g\n" diff cost
>    return (diff, cost)


> calibrate p = p { cam = newcams!>"wv", p2d = newp2d!>"wv" } where
>    ks = parts (kal $ cam p) "c"
>    iks = map ((!"wv").applyAsMatrix inv) ks
>    cs = parts (cam p) "c"
>    ps = parts (p2d p) "c"
>    newcams = subindex "c" $ zipWith (*) iks cs
>    newp2d  = subindex "c" $ zipWith (*) iks ps

