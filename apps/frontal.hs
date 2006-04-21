-- experiments on planar rectification

import GSL
import Vision
import Data.List (elemIndex,sort)
import Debug.Trace
import System.Random
import System.Environment (getArgs)
import Control.Monad (when)

partit :: Int -> [a] -> [[a]]
partit _ [] = []
partit n l  = take n l : partit n (drop n l)

contamina :: Double -> [[[Double]]] -> [[[Double]]]
contamina noiseLevel vs = res where
    nv = length vs
    np = length (vs!!0)
    points = concat $ concat vs
    noisypoints = zipWith (+) points noise
    res = partit np $ partit 2 noisypoints
    noise = randomRs (-noiseLevel, noiseLevel) (mkStdGen 0) 
 
 
muestra str v = trace (str++"="++show v++"\n") v  
 

reference = [[a,a],
             [a,b],
             [b,b],
             [0,1],
             [a,b],
             [b,b], 
             [b,a],
             [a,a]]
    where a = (-0.5)
          b = 0.5

unitSquare = [[a,a],
              [a,b],
              [b,b],
              [b,a]]
    where a = (-0.5)
          b = 0.5

      
                       
-- movimiento lateral 
waving ki = homogZ0 $ syntheticCamera $ 
                easyCamera (40*degree) cen (0,0,0) 0
    where k = (fromIntegral (100 * ki) / 100)
          cen = (cos (3*k*degree),-2,2+cos (2*k*degree))        
           
-- circular orbit around the object
orbit ki = homogZ0 $ syntheticCamera $ 
                easyCamera (40*degree) cen (0,0,0) 0
    where k = (fromIntegral (100 * ki) / 100)
          cen = (2*cos (3*k*degree), 2*sin (3*k*degree), 1)             
                
-- rotando la cámara recorriendo (circularmente el objeto)
-- ¡¡¡Así no se puede!!!
rotation ki = homogZ0 $ syntheticCamera $ 
                easyCamera (40*degree) (0,-3,1) pos 0
    where k = (fromIntegral (100 * ki) / 100)
          pos = (0.5*cos (3*k*degree), 2+3*sin (3*k*degree), 0)       
                       
-- espiral ascendente alrededor del objeto
spiral ki = homogZ0 $ syntheticCamera $ 
                easyCamera (40*degree) cen (0,0,0) 0
    where k = (fromIntegral (100 * ki) / 100)
          cen = (2*cos (3*k*degree), 2*sin (3*k*degree), 1+k/20)             
             
-- follow haciendo zoom
follow ki = homogZ0 $ syntheticCamera $ 
                easyCamera (30*degree-k*degree/7) cen (0,0,0) 0
    where k = (fromIntegral (100 * ki) / 100)
          cen = (0, -3-k/20, 2)                 
             
             
-- movimiento general
general ki = homogZ0 $ syntheticCamera $ 
                easyCamera (40*degree-k*degree/4) cen (0,0,0) (k*degree)
    where k = (fromIntegral (100 * ki) / 100)
          cen = ((2+k/20)*cos (3*k*degree), (2+k/20)*sin (3*k*degree), 1+k/20)             
             
             
             
genCams path = [path (10*k) | k <- [0..10]]   -- 10 views
          
genCams' path = [path (2*k) | k <- [0..50]]   -- 50 views

-- this gives a low value if h is a similar transformation
similarityDegree h = pnorm 1 (m'-v) where
    v = realVector [1,0,0,0,1,0,0,0,0]
    m = flatten (h <> mS <> trans h)
    m' = m <> recip (m!:0)

-- hmm! premature optimization...
omegaGen f = kgen (recip (f*f))

-- this gives a measure of the difference with a camera homography, for known f
orthogonality omega c = pnorm 1 (m'-v) where
    v = realVector [1,0,0,1]
    m = flatten $ subMatrix (0,0) (2,2) q
    m' = m <> recip (m!:0)
    q = trans c <> omega <> c 

-- si das un f (omega) la usa, si no intenta estimarla y si no puede ve si es similar
autoOrthogonality mbOmega c = res where
    res = case mbOmega of
            Just omega -> orthogonality omega c
            Nothing -> auto
    auto = case focal c of
            Just f -> orthogonality (omegaGen f) c
            Nothing -> similarityDegree c

-- rectifier transformation
-- should be precomputed
rectifier ((rho,yh),f) = kgen f <> rot1 (atan2 f yh) <> rot3 (-rho) <> kgen (recip f)

-- associated camera (the inverse of the above)
--rectifier' ((rho,yh),f) = kgen f <> rot3 rho <> rot1 (- atan2 f yh) <> kgen (recip f)
--rectifier' = inv3.rectifier 
rectifier' ((rho,yh),f) = reshape 3 $ realVector [ 
      cr, -ca*sr, -f*sa*sr,
      sr,  ca*cr,  f*cr*sa,
      0,  -sa/f ,  ca ]
    where a = - atan2 f yh
          ca = cos a
          sa = sin a
          cr = cos rho
          sr = - sin rho      


northPoint c = c <> mS <> trans c <> linf    
    
ryf c = focal c >>= \f -> Just ((rho,yh),f) where
    [x,y,w] = toList $ northPoint $ c   
    rho = atan2 x y
    yh = sqrt (nx*nx+ny*ny)
    nx = x/w
    ny = y/w
    
    

polarHoriz :: (Double,Double) -> Vector
polarHoriz (r',y) = h where
    r = -(r'+3*pi/2)
    n = realVector [y* cos r, y* sin r , 1]  
    h = cross n (mA<>n)
    
  
-- estimation of f1 given a polar horiz (r,y) and a interimage homograpy 1<-k
estimateFTransfer (r,y) h = res 
  where
    horiz = polarHoriz (r,y) -- horiz in view 1
    hp = trans h <> horiz  -- horiz in view k
    d = mA <> hp
    n = cross hp d 
    a = inHomog $ unitary $ h <> n
    b = inHomog $ unitary $ h <> d
    ni = inHomog $ unitary $ cross horiz (mA <> horiz)
    yh = norm ni
    x1 = norm (a-ni)
    x2 = norm (b-ni)
    f = sqrt (x1*x2-yh*yh)
    res = if f > 0.5 && f < 10 && x1 < 20 &&  x2 < 20 -- parametrizar mejor
            then Just f
            else Nothing
 
quality ihs mbOmgs c = sum qs / fromIntegral (length ihs) where 
    camscorr = map (<>c) ihs
    qs = zipWith autoOrthogonality mbOmgs camscorr  

posMin l = i where
    Just i = elemIndex (minimum l) l

-- a partir de la distribución de posibles estimaciones da la mediana o Nothing...
estimatorF :: [Maybe Double] -> Maybe Double
estimatorF mbfs = res where
    fs = [ f | Just f <- mbfs ]
    n = length fs
    fe = sort fs !! (n`quot`2) 
    res = case length fs of
            0 -> Nothing
            _ -> Just fe    


type TPars = ((Double,Double),Double)

data RectifInfo = 
    RF { usedViews :: [[[Double]]] -- points used to compute dHs and iHs
       , dHs :: [Matrix] -- tranferencia de k hacia la primera
       , iHs :: [Matrix] -- transferencia desde la primera hasta la k
       , nHoriz :: Int   -- discretización del espacio de horizontes en cada dimension
       , paramHoriz :: [(Double,Double)] -- posiciones polares (rho,h) posibles del horiz
       , trueCams :: [Matrix] -- cámaras verdaderas
       , trueFs :: [Double] -- true fs
       , trueF1 :: Double -- true focal de la vista base
       
       , fDists :: [[Maybe Double]] -- list of f1 estimates for every candidate horiz
       , f1estimated :: [Maybe Double] -- median f1 estimate for every candidate horiz
       , truefDist :: [Maybe Double] -- list of f1 estimates with the true horiz
       
       , trueRectif :: Matrix -- true camera rectifier
       , truePars :: TPars -- pars of true rectifier
       , rectifsKF :: [Matrix] -- candidate camera rectifiers using trueF1 
       , rectifsU :: [Matrix] -- candidate camera rectifiers using estimated f1
       , testQuality :: [Double] -- several quality measures for self-checks
       
       , costsAllF :: [Double] -- consistency of candihorizs with all F known
       , initAllF :: TPars -- the best of it
       , solAllF :: (TPars,Matrix) -- refined solution
       , rectifViewAllF :: [[Double]] -- raw rectified view 0 with sollAllF
       
       , costsF1 :: [Double] -- consistency of candihorizs with f1 known
       , initF1 :: TPars -- the best of it
       , solF1 :: (TPars,Matrix) -- refined solution
       , rectifViewF1 :: [[Double]] -- raw rectified view 0 with sollAllF
       
       , costsAuto :: [Double] -- consistency of candihorizs with all fs unknown
       , initAuto :: (Double,Double) -- the best of it
       , solAuto :: TPars -- refined solution
       , rectifViewAuto :: [[Double]] -- raw rectified view 0 with solAuto
       
       , costsConstant :: [Double] -- consistency assuming all fs equal to estimated f1
       , initConstant :: TPars -- the best of it
       , solConstant :: TPars -- refined solution
       , rectifViewConstant :: [[Double]] -- raw rectified view 0 with solConstant
        }
       

-- falta añadir noise
prepare n noiselevel cams fs = rf where
    rf = RF { usedViews = views
            , dHs = map (estimateHomographyRaw (head views)) (tail views) 
            , iHs = map inv3 (dHs rf)
            , nHoriz = n
            , paramHoriz = pars
            , trueCams = cams
            , trueFs = fs
            , trueF1 = head fs
            , rectifsKF = map rectifier' (zip pars (repeat f1))
            , trueRectif = rectifier' tp
            , truePars = tp
            
            , testQuality = map ($(trueRectif rf)) [q1,q2]
            
            , costsAllF = map q1 (rectifsKF rf)
            , initAllF = (pars !! posMin (costsAllF rf), f1)
            , solAllF = saf
            --, rectifViewAllF = ht (rectifier (fst saf)) (views!!0)
            , rectifViewAllF = ht (rectifier (initAllF rf)) (views!!0)
            
            , costsF1 = map q2 (rectifsKF rf)
            , initF1 = (pars !! posMin (costsF1 rf), f1)
            , solF1 = saf2
            --, rectifViewF1 = ht (rectifier (fst saf2)) (views!!0)
            , rectifViewF1 = ht (rectifier (initF1 rf)) (views!!0)
            
            , fDists = fdis
            , f1estimated = map estimatorF fdis
            , truefDist = fEst (fst tp)
            
            , rectifsU = rtu           
            , costsAuto = map q2 rtu
            , initAuto = pars !! posMin (costsAuto rf)
            , solAuto = solAutoTemp -- to do: minimize
            , rectifViewAuto = ht (rectifier (solAuto rf)) (views!!0)
            
            , costsConstant = zipWith q3 rtu (f1estimated rf)
            , initConstant = icp
            , solConstant = initConstant rf -- to do: minimize
            , rectifViewConstant = ht (rectifier (solConstant rf)) (views!!0)
            }     
    views' = map f cams where f c = ht c unitSquare
    views = contamina noiselevel views'
    f1 = fs!!0   
    pars = [(r,y) | r <- toList (linspace n (-60*degree,60*degree)) 
                  , y <- toList (linspace n (0, 3))] 
    Just tp = ryf (cams!!0)
    trueOmegas = (map omegaGen (tail fs))             
    q1 = quality (iHs rf) (map Just trueOmegas) 
       
    saf = (((r,t),f1),p) where 
        ([r,t],p) = minimizeNMSimplex fun xi [0.1*degree,0.01] 1e-6 500     
        xi = [ri,ti]
        ((ri,ti),_) = initAllF rf
        fun [r,t] = q1 (rectifier' ((r,t),f1))
       
    q2 = quality (iHs rf) (repeat Nothing :: [Maybe Matrix])

    saf2 = (((r,t),f1),p) where 
        ([r,t],p) = minimizeNMSimplex fun xi [0.1*degree,0.01] 1e-6 500     
        xi = [ri,ti]
        ((ri,ti),_) = initF1 rf
        fun [r,t] = q2 (rectifier' ((r,t),f1))
        
    fdis = map fEst pars
    fEst h@(r,y) = map (estimateFTransfer h) (dHs rf)   

    
    rtu = map mbrect (zip pars (f1estimated rf))
        where mbrect (p,Just f) = rectifier' (p,f)
              mbrect (_,Nothing) = ident 3 
              
    pmin = posMin (costsConstant rf)     
    icp = (pars !! pmin, f1est) where (Just f1est) = (f1estimated rf) !! pmin
                 
    q3 r mf1e = quality (iHs rf) (repeat mbomega) r
        where mbomega = case mf1e of
                          Nothing -> Nothing
                          Just f1e -> Just (omegaGen f1e) 
                           
    solAutoTemp = (h,f) where
        h = initAuto rf
        Just f = f1estimated rf !! posMin (costsAuto rf)    
                       
-------------------------------------------------------------

info cams n r = prepare n r cams (map (fj.focal) cams)
    where fj (Just f) = f

experMat v costs info = reshape (nHoriz info) $ realVector $ map (clip v) (costs info) 
 
exper v c i = imshow $ experMat v c i
 
clip v x = if x>v then v else x  

seepath path = do plotOpenGL anim 
    where points = homogMat (realMatrix reference)
          anim k = (x+2,y) where 
            [x,y] = toColumns $ inHomogMat $ points <> trans (path k)

verTrans t = hplot (toColumns $ inHomogMat $ (homogMat  (realMatrix reference)) <> trans t)

verSel cams = mapM_ verTrans cams

study v (rectinfo, name) = do
    let f (cost,nm) = do
        matrixToPGM ("images/"++name++"-"++nm++".pgm") $ experMat v cost rectinfo
        toFile ("images/"++name++"-"++nm++".txt") $ reshape (nHoriz rectinfo) $ realVector (cost rectinfo)
    when False $ mapM_ f [(costsAllF,"all"), 
             (costsF1,"f1"), 
             (costsAuto,"auto"), 
             (costsConstant,"const")]
    
    putStrLn $ "--------------- "++name++" -------------"
    putStr "True horizon: "
    print $ truePars rectinfo
    putStr "Test qualities: "
    print $ testQuality rectinfo
    putStr "Estimated f1 with true horizon: "
    print $ estimatorF $ truefDist $ rectinfo
    putStr "Init AllF: "
    print $ initAllF rectinfo
    putStr "Optimized AllF: "   
    print $ fst $ solAllF rectinfo
    putStr "Init F1: "
    print $ initF1 rectinfo
    putStr "Optimized F1: "
    print $ fst $ solF1 rectinfo
    putStr "Init auto: "
    print $ initAuto rectinfo
    putStr "Init constant: "
    print $ initConstant rectinfo
    when False $ do 
        printViews (rectinfo,name)
        printRectifiedViews (rectinfo,name)


printViews (rectinfo,name) = do
    toFile ("images/"++name++"-views.txt") (realMatrix $ concat (usedViews rectinfo))


printRectifiedViews (rectinfo,name) = do
    toFile ("images/"++name++"-RECT-all.txt") (realMatrix (rectifViewAllF rectinfo))
    toFile ("images/"++name++"-RECT-f1.txt") (realMatrix (rectifViewF1 rectinfo))
    toFile ("images/"++name++"-RECT-auto.txt") (realMatrix (rectifViewAuto rectinfo))
    toFile ("images/"++name++"-RECT-const.txt") (realMatrix (rectifViewConstant rectinfo))

problems = 
    [ (general,  "general")
    , (orbit,    "orbit")
    , (waving,   "waving")
    , (spiral,   "spiral")
    , (follow,   "follow") 
    , (rotation, "rotation")
    ]

versions (prob, name) = 
    [ -- (info (genCams prob) 100 0,    (name))
      (info (genCams prob) 100 0.01, (name++"-l"))
      -- , (info (genCams prob) 100 0.05, (name++"-h"))
      -- , (info (genCams' prob) 100 0.05, (name++"-2h"))
    ]

fullstudy (prob,name) = do
    mapM_ (study 20) (versions (prob,name))

main = do
    mapM_ printViews (map (head.versions) problems)
    s <- getArgs
    fullstudy (problems!! read (s!!0))
    -- fullstudy (problems!!1)
    -- fullstudy (problems!!2)
    -- fullstudy (problems!!3)
