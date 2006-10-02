-----------------------------------------------------------------------------
{- |
Module      :  Vision.Classifier
Copyright   :  (c) Alberto Ruiz 2006
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  very provisional
Portability :  hmm...

Basic Statistical Pattern Recognition algorithms. 

-}
-----------------------------------------------------------------------------

module Vision.Classifier (
-- * Basic definitions
     Attributes, Label, Example, Sample,
     Classifier, Estimator, Learner,
     Feature, TwoGroups, Dicotomizer, multiclass,
     Weights, WeightedDicotomizer, unweight, weight, 
-- * Simple classifiers
     Distance, distance, ordinary, mahalanobis, mahalanobis', closestNeighbour,
     mse, mseWeighted, distWeighted,
     stumps,
-- * Meta algorithms
-- ** Adaboost
     adaboost, adaboostST, adaFun,
-- ** Decision tree
     treeOf, branch,
-- ** PCA preprocessing
     withPCA,
-- * Utilities
     errorRate, confusion, combined, group, ungroup, createClassifier, scramble, breakTies, selectClasses, splitProportion, posMax, preprocess,
-- * 2D toy problems
-- | You can take a look at them using 'combined'.
     linsep, linsepmulti, nosep, ring, moon, rings, sshape, mnist, mnistraw

) where

import GSL
import Data.List(sortBy, sort, nub, elemIndex, intersperse, transpose, partition)
import qualified Data.Map as Map
import System.Random
import System
import Data.Array
import Vision.Stat
import Debug.Trace

debug x = trace (show x) x
debug' msg x = trace (msg ++ show x) x

matrix = realMatrix
vector = realVector

------------------- General definitions ----------------------------

type Attributes = Vector
type Label = String
type Example = (Attributes, String)
type Sample = [Example]

-- | A generic learning machine
type Learner = Sample -> (Classifier, Estimator)

type Classifier = Attributes -> Label     -- crisp decision

-- | More informative 'Classifier', which obtains the relative \"confidence\" of each class (related to an approximation to the posterior probabilities)
type Estimator = Attributes -> [Double]


-- | A function that tries to discriminate between two classes of objects (positive means the first class)
type Feature = Attributes -> Double       -- +/-

type TwoGroups = ([Attributes],[Attributes]) -- +/-

-- | A learning machine for binary classification problems. See 'multiclass'.
type Dicotomizer = TwoGroups -> Feature

type Weights = Vector
-- | An improved 'Dicotomizer' which admits a distribution on the given examples.
type WeightedDicotomizer = TwoGroups -> Weights -> Feature


-- | Class labels info with helper functions
data InfoLabels = InfoLabels {
    labels :: [String],
    getIndex :: String -> Int,
    getLabel :: Int -> String
}

-- | extracts the labels of a sample
extractLabels :: Sample -> InfoLabels
extractLabels l = InfoLabels ls lti itl where
    ls = sort $ nub $ map snd l
    itl = (ls!!)
    m = createMap ls
    lti = (Map.!) m
    -- | given a list of labels creates a map from them to their class index
    createMap :: [String] -> Map.Map String Int
    createMap ls = Map.fromList (zip ls [0..])

compareBy f = (\a b-> compare (f a) (f b))

partit _ [] = []
partit n l  = take n l : partit n (drop n l)

-- | pseudorandom permutation of a list
scramble :: Int -> [a] -> [a]
scramble seed l = map fst $ sortBy (compareBy snd) randomTuples where
    randomTuples = zip l (randomRs (0, 1::Double) (mkStdGen seed))

-- | add noise to break ties
breakTies :: Int -> Double -> Sample -> Sample
breakTies seed sz l = zip rvs lbs where
    (vs,lbs) = unzip l
    n = size (head vs)
    randomVectors = map vector $ partit n $ randomRs (-sz, sz::Double) (mkStdGen seed)
    rvs = zipWith (+) vs randomVectors

-- | obtains training and testing subsamples given the required proportion of training instances. You may need to scramble the original sample.
splitProportion :: Double -> [a] -> ([a],[a])
splitProportion r l = splitAt n l where n = round (r * fromIntegral (length l))

-- | creates a 'Classifier' from an 'Estimator' and the particular class labels of a problem.
createClassifier :: InfoLabels -> Estimator -> Classifier
createClassifier ilbs f = getLabel ilbs . posMax . f

-- | returns the position of the maximum element in a list
posMax :: Ord a => [a] -> Int
posMax l = p where
    Just p = elemIndex (maximum l) l

posMin l = p where
    Just p = elemIndex (minimum l) l


-- | groups the attribute vectors of each class
group :: Sample -> ([[Attributes]], InfoLabels)
group l = (gs, lbs) where
    lbs = extractLabels l
    gs = map proto (labels lbs) where proto lb = [v | (v,s)<-l , s==lb]

-- | converts a list of groups of vectors into a 'Sample' with different labels
ungroup :: [[Attributes]] -> Sample
ungroup gs = s where
    n = length gs
    lbs = map (show) [1 .. n]
    s = concat $ zipWith f gs lbs
    f g lb = zip g (repeat lb)

-- | Estimates the success rate of a classifier on a sample
errorRate :: Sample -> Classifier -> Double
errorRate exs c  = fromIntegral ok / fromIntegral (length exs) where
    ok = length [1 | (v,l)<-exs, l /= c v]

-- | Computes the confusion matrix of a classifier on a sample
confusion ::Sample -> Classifier -> Matrix
confusion exs c = confusionMatrix where
    lbs = extractLabels exs
    l = getIndex lbs
    estimatedLabels = map (l.c.fst) exs
    te = zip (map (l.snd) exs) estimatedLabels 
    nc = length (labels lbs)
    confusionMatrix = fromArray $ 
        accumArray (+) (0::Double) ((0::Int,0::Int),(nc-1,nc-1)) (zip te [1,1 ..])


-- | Applies some preprocessing to the Attributes of a Sample
preprocess :: Codec -> Sample -> Sample
preprocess Codec {encodeVector = f} exs = [(f v, l) | (v,l) <- exs]



rot 0 xs = xs
rot k (x:xs) = rot (k-1) (xs++[x])
rots l = map ((flip rot) l) [0 .. length l - 1]
auxgroup l = map (\(x:xs) -> (x, concat xs)) (rots l)

-- | Constructs a (multiclass) 'Learner' given any 'Dicotomizer' (by creating n features to discriminate each class against the rest)
multiclass :: Dicotomizer -> Learner
multiclass bin exs = (createClassifier lbs f, f) where
    (gs,lbs) = group exs
    f = multiclass' bin gs

multiclass' _ [] = error "multiclass applied to 0 classes"
multiclass' _ [_] = error "multiclass applied to 1 class"
multiclass' bin [g1,g2] = (\x -> [x,-x]) . bin (g1,g2)
multiclass' bin l = f where
    fs = map bin (auxgroup l)
    f v = map ($v) fs


-- | selects the examples with the given labels
selectClasses :: [Label] -> Sample -> Sample
selectClasses validset exs = filter ( (`elem` validset) .snd) exs



-- | combines a learner with pca dimensionality reduction
withPCA :: PCARequest -> Learner -> Learner
withPCA rq method prob = (c,f) where
    st = stat $ fromRows (map fst prob)
    codec = pca rq st
    prob' = preprocess codec prob
    (c',f') = method prob'
    c = c' . encodeVector codec
    f = f' . encodeVector codec

------------------------- drawing utilities --------------------------

gnuplot command = do
    writeFile "gnuplotcommand" $ command ++ "; pause -1 'Press return to continue...'"
    system "gnuplot gnuplotcommand" 
    system "rm gnuplotcommand"

prep = (++"e\n") . unlines . map (unwords . (map show))


show2Dfun :: Int -> Double -> (Vector -> Double) -> IO ()
show2Dfun n r f = imshow (trans z) where
    l1 = toList $ linspace n (-r,r)
    l2 = reverse $ toList $ linspace n (-r,r)
    z = realMatrix $ partit (length l1) $ [ f (realVector [x,y]) | x <- l1, y <- l2]

{- | 2D representation of a 2D feature.

For instance:

@study :: Sample -\> Learner -\> IO ()
study prob meth = do
    seed \<- randomIO
    let (train,test) = splitProportion 0.5 $ scramble seed prob
    let (c,f) = meth train
    putStr \"Test error: \"
    print (errorRate test c)
    print (confusion test c)
    combined 100 2.5 (fromIntegral.posMax.f) train@

@\> study (nosep 500) (distance mahalanobis')@

@\> study (nosep 500) (multiclass mse)@

@\> study (rings 2000) (multiclass (adaboost 100 stumps))@

@\> study (breakTies 100 0.00001 (rings 2000))
          (multiclass (treeOf (branch 0) (unweight stumps)))@

-}
combined :: Int -> Double -> (Vector->Double) -> Sample -> IO ()
combined n r f exs = act where
    (gs,_) = group exs
    l1 = toList $ linspace n (-r,r)
    l2 = toList $ linspace n (-r,r)
    z = [[x,y, f (realVector [x,y])] | x <- l1, y <- l2]
    z' = concat $ map (++[[]]) $ partit n z
    g m = toList m ++ [0]
    preps = concat (map p gs) where p gi = prep (map g gi)
    hs' = map ("\"-\" with points " ++) (map show [1 .. length gs])
    hs = concat (intersperse "," hs') ++ "\n"
    act = do
        gnuplot $ "set size square; set pm3d map explicit; set style data pm3d; set palette gray; "
                  ++ "splot \"-\" with pm3d, "
                  ++ hs
                  ++ prep z'
                  ++ preps
        return ()

---------------------------- some toy classification problems ---------------------------

-- | interlaced S-shapes
sshape :: Int -> Sample
sshape n = dat1 ++ dat2 where
    m = n `quot` 2
    ts k = tail $ toList $ linspace (k+1) (0,4/5*2*pi)
    dat1 = [ (vector [0.5 -  1*cos t, 0.2 -1*sin t],     "a") | t <- ts m ]
    dat2 = [ (vector [-0.5 + 1*cos t, -0.2 + 1*sin t],   "b") | t <- ts m ]


-- | concentric semicircles
moon :: Int -> Sample
moon n = dat1 ++ dat2 where
    m = n `quot` 2
    ts k = tail $ toList $ linspace (k+1) (0,pi)
    dat1 = [ (vector [2*cos t, 2*sin t -1],     "a") | t <- ts m ]
    dat2 = [ (vector [cos t, sin t -1],         "b") | t <- ts m]

-- | 3 concentric rings (nonconvex solution)
rings :: Int -> Sample
rings n = dat1 ++ dat2 ++ dat3 where
    m = n `quot` 4
    ts k = tail $ toList $ linspace (k+1) (0,2*pi)
    dat1 = [ (vector [2*cos t, 2*sin t],     "a") | t <- ts m ]
    dat2 = [ (vector [cos t, sin t],         "b") | t <- ts (2*m)]
    dat3 = [ (vector [0.3*cos t, 0.3*sin t], "a") | t <- ts m ]

-- | concentric rings
ring :: Int -> Sample
ring n = dat1 ++ dat2 where
    m = n `quot` 2
    ts k = tail $ toList $ linspace (k+1) (0,2*pi)
    dat1 = [ (vector [2*cos t, 2*sin t],     "a") | t <- ts m ]
    dat2 = [ (vector [cos t, sin t],         "b") | t <- ts m]

--  | very simple problem
linsep :: Int -> Sample
linsep n = dat1 ++ dat2 where
    m = n `quot` 2
    ts = tail $ toList $ linspace (m+1) (0,2*pi)
    dat1 = [ (vector [1+0.5*cos t, -1+0.8*sin t],   "a") | t <- ts ]
    dat2 = [ (vector [-1+cos t, 1-sin t], "b") | t <- ts ]

--  | simple multiclass problem 
linsepmulti :: Int -> Sample
linsepmulti n = dat1 ++ dat2 ++ dat3 where
    m = n `quot` 2
    ts = tail $ toList $ linspace (m+1) (0,2*pi)
    k = 0.7
    dat1 = [ (vector [1+k*cos t, -1+k*sin t],   "a") | t <- ts ]
    dat2 = [ (vector [-1+k*cos t, 1-k*sin t], "b") | t <- ts ]
    dat3 = [ (vector [1+k*cos t, 1-k*sin t], "c") | t <- ts ]

--  | simple nonlinearly separable problem 
nosep :: Int -> Sample
nosep n = dat1 ++ dat2 ++ dat3 where
    m = n `quot` 4
    ts m = tail $ toList $ linspace (m+1) (0,2*pi)
    k = 0.8
    dat1 = [ (vector [1+2*k*cos t, -1+k*sin t],   "a") | t <- ts m ]
    dat2 = [ (vector [-1+k*cos t, 1-2*k*sin t], "a") | t <- ts m ]
    dat3 = [ (vector [1+k*cos t, 1-k*sin t], "b") | t <- ts (2*m)]

-- | handwritten digits, partitioned and with desired number of pca dimensions
mnist :: Int -> Int -> IO (Sample, Sample)

mnist dim n = do
    m <- gslReadMatrix "mnist.txt" (5000,785)
    let vs = toRows (takeColumns 784 m)
    let ls = map (show.round) $ toList $ flatten $ dropColumns 784 m
    let mnist = zip vs ls
    let (train, test) = splitAt n mnist

    let st = stat (fromRows $ map fst train)
    let codec = pca (NewDimension dim) st

    return (preprocess codec train,
            preprocess codec test)

-- | the mnist raw data
mnistraw :: Int -> IO (Sample,Sample)
mnistraw n = do
    m <- gslReadMatrix "mnist.txt" (5000,785)
    let vs = toRows (takeColumns 784 m)
    let ls = map (show.round) $ toList $ flatten $ dropColumns 784 m
    let mnist = zip vs ls
    return $ splitAt n mnist



------------------------------- a few methods ---------------------------------


-- | mse linear discriminant using the pseudoinverse
mse :: Dicotomizer
mse (g1,g2) = f where
    m = (fromRows g1 <-> fromRows g2) <|> constant 1 (size b)
    b = join [constant 1 (length g1), constant (-1) (length g2)]
    w = pinv m <> b
    f v = tanh (join [v,1] <> w)

--------------------------------------------------------------------------------

-- | A measure of the disimilarity or distance from an attribute vector to a sample of vectors of a certain class
type Distance =  [Attributes] -> Attributes -> Double

-- | Mahalanobis's distance to a population.
mahalanobis :: Distance
mahalanobis vs = f where
    Stat {meanVector = m, invCov = ic} = stat (fromRows vs)
    f x = (x-m) <> ic <> (x-m)

-- | gaussian -log likelihood (mahalanobis + 1\/2 log sqrt det cov)
mahalanobis' :: Distance
mahalanobis' vs = f where
    Stat {meanVector = m, invCov = ic} = stat (fromRows vs)
    k = -log (sqrt (abs( det ic)))
    f x = k + 0.5*((x-m) <> ic <> (x-m))

-- | Distance to the mean value of the population.
ordinary :: Distance
ordinary vs = f where
    Stat {meanVector = m} = stat (fromRows vs)
    f x = norm (x-m)

-- | distance to the nearest neighbour
closestNeighbour :: Distance
closestNeighbour vs v = minimum (map (dist v) vs)
    where dist x y = norm (x-y)


-- | A generic distance-based learning machine.
distance :: Distance -> Learner
distance d exs = (c,f) where
    (gs,lbs) = group exs
    distfuns = map d gs
    f x = map (negate.($x)) distfuns
    c = createClassifier lbs f

------------------------------------------------------------------------------------

-- | Converts a 'WeightedDicotomizer' into an ordinary 'Dicotomizer' (using an uniform distribution).
unweight :: WeightedDicotomizer -> Dicotomizer
unweight dic (g1,g2) = dic (g1,g2) w where
    m = length g1 + length g2
    w = constant (1/fromIntegral m) m

-- | Converts a 'Dicotomizer' into a 'WeightedDicotomizer' (by resampling).
weight :: Int -- ^ seed of the random sequence
          -> Dicotomizer -> WeightedDicotomizer
weight seed dic (g1,g2) w = dic (g1',g2') where
    s = ungroup [g1,g2]
    ac = scanl1 (+) (toList w)
    rs = take (length ac) $ randomRs (0, 1::Double) (mkStdGen seed)
    rul = zip ac s
    elm pos = snd $ head $ fst $ partition ((>pos).fst) rul
    [g1',g2'] = fst (group (breakTies seed 0.0001 $ map elm rs))


-- | an extremely simple learning machine
singlestump :: Learner
singlestump = multiclass (unweight stumps)


-- | weak learner which finds a good threshold on a single attribute
stumps :: WeightedDicotomizer
stumps p = stumpsOk (prepro p)

-- useful precomputation: indices of the sorted features.
prepro (g1,g2) = ((g1,g2),lbs,xs,oxs,is) where
    lbs = join [constant 1 (length g1), constant (-1) (length g2)]
    xs = transpose $ map toList (g1 ++ g2)
    s = map f xs
    f l = sortBy (compareBy fst) (zip l [0..])
    oxs = map (map fst) s
    is  = map (map snd) s


stumpsOk ((g1,g2),lbs,xs,oxs,is) d = f where
    wl = lbs*d
    n1 = length g1
    n2 = length g2
    d1 = pnorm 1 $ subVector  0 n1 d
    d2 = pnorm 1 $ subVector n1 n2 d

    owls = map (map (wl!:)) is
    cs = map (sel .dt . scanl (+) 0 . init) owls
    dt x = ((k,v),(q,w)) where
        k = posMin x
        v = (x!!k) + d2
        q = posMax x
        w = d1 - (x!!q)
    sel ((k,v),(q,w)) = if v < w then (k,v,1) else (q,w,-1)
    r = map g $ zip oxs cs
    g (l,(k,v,s)) = (v,(h l k, s))
    h l k = 0.5*(l'!!(k) + l'!!(k+1)) where
        l' = (l!!0 - (l!!1-l!!0)) : l -- ++ [l!!n + (l!!n - l!!(n-1))]
        -- n = length l - 1
    k = posMin (map (abs.fst) r)
    (_,(v,s)) = r!!k

    f x = s * signum' (x !: k - v)
    signum' x = if x > 0 then 1 else -1

----------------------------------------------------------
-- more complex weak learners, rather bad

-- | mse with weighted examples
mseWeighted :: WeightedDicotomizer
mseWeighted (g1,g2) d = f where
    m = (fromRows g1 <-> fromRows g2) <|> constant 1 (size b)
    b = join [constant 1 (length g1), constant (-1) (length g2)]
    rd  = sqrt d
    rd' = outer rd (constant 1 (cols m))
    w = pinv (m*rd') <> (b*rd)
    f v = tanh (join [v,1] <> w)




-- | a minimum distance dicotomizer using weighted examples
distWeighted :: WeightedDicotomizer
distWeighted (g1,g2) d = f where
    n1 = length g1
    n2 = length g2
    d1 = subVector  0 n1 d
    d2 = subVector n1 n2 d
    ones = constant 1 (size (head g1))
    a1 = outer d1 ones * fromRows g1
    a2 = outer d2 ones * fromRows g2
    m1 = sumColumns a1 <> recip (pnorm 1 d1)
    m2 = sumColumns a2 <> recip (pnorm 1 d2)
    f x = norm (x-m2) - norm (x-m1)
    sumColumns m = constant 1 (rows m) <> m



-- just to check that they are not completely wrong

--mse' = multiclass (unweight mseWeighted)

--dist' = multiclass (unweight distWeighted)


-- | weak learner trained by the adaboost algorithm
type ADBST = (Feature, Vector, Double, Double)


-- this works with a partially applied WeightedDicotomizer to reuse any
-- possible data preprocessing
adaboostStep :: (Weights -> Feature) -> TwoGroups -> Weights -> ADBST
adaboostStep method (g1,g2) d = (f,d',e,a) where
    f = method d
    e1 = map (signum . max 0 . negate . f) g1
    e2 = map (signum . max 0 . f) g2
    e = join [vector e1, vector e2] <> d
    a = 0.5 * log ((1-e)/e) -- it may be Infinity
    kp = exp (-a)
    kn = exp a
    f1 v = if f v > 0 then kp else kn
    f2 v = if f v < 0 then kp else kn
    d1 = map f1 g1
    d2 = map f2 g2
    dr = d * join [vector d1, vector d2]
    d' = dr <> recip (dr <> constant 1 (size dr))

-- | creates a list of weak learners and associated information to build a strong learner using adaboost
adaboostST :: Int -> WeightedDicotomizer -> TwoGroups -> [ADBST]
adaboostST n m p = r where
    (f,st@(g,d,e,a)) = initAdaboost m p
    work = takeok n (iterate (adaboost' f p) st)
    easy = [(g,1,e,1)]
    r = if e > 0.001 then work else easy
    adaboost' f p (_,d,_,_) = adaboostStep f p d
    takeok n = take n . fst . span pos
    pos (_,_,e,_) = e < 0.499


initAdaboost :: WeightedDicotomizer -> TwoGroups -> (Weights -> Feature, ADBST)
initAdaboost method gs =  (f, adaboostStep f gs w)
    where f = method gs
          w = constant (1 / fromIntegral m) m
          m = length g1 + length g2 where (g1,g2) = gs

-- | combines the weak learners obtained by 'adaboostST'
adaFun :: [ADBST] -> Feature
adaFun st = comb where
    comb x = sum $ [a*signum (f x)| (f,_,_,a) <- st]

-- runs directly n steps of the adaboost algorithm
adaboost:: Int -> WeightedDicotomizer -> Dicotomizer
adaboost n m = adaFun . adaboostST n m

------------------------------------------------------------------------

-- | Creates a decision tree
treeOf :: (TwoGroups -> Bool) -> Dicotomizer -> Dicotomizer

treeOf stopQ method gs@(g1,g2) = if stopQ gs || not improved then leaf else node where
    n1 = length g1
    n2 = length g2
    leaf = if n1>n2 then const 1 else const (-1)
    node v = if d v > 0 then d1 v else d2 v
    d = method gs
    (g11,g12) = partition ((>0).d) g1
    (g21,g22) = partition ((>0).d) g2
    d1 = treeOf stopQ method (g11,g21)
    d2 = treeOf stopQ method (g12,g22)
    improved = (length g11, length g21) /= (n1,n2) &&
               (length g12, length g22) /= (n1,n2)

-- | stopping criterium for 'treeOf'. A new decision node is created if the minoritary class has more than n samples
branch :: Int -> (TwoGroups -> Bool)
branch n (g1,g2) = min (length g1) (length g2) <= n