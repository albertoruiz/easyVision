{-# LANGUAGE TemplateHaskell, RecordWildCards #-}

import EasyVision
import Graphics.UI.GLUT hiding (RGB,Size,minmax,histogram,Point,normalize)
import GHC.Float(double2Float)
import Control.Arrow
import Control.Monad
import Data.List
import Data.Function
import qualified Util.Misc as U
import Data.Graph
import Data.Tree as Tree

$(autoParam "Selsol" ""
   [("sol", "Int", intParam 0 0 100)]
 )



main = run $ ((camera ~> gray >>= wcontours id ~> (id *** adjacencies.contSel)) .&. winSelsol ~> sol) >>= monitor "image" (mpSize 20) sh

shContLab ((c,k), s) = do
    setColor 1 0.4 0.4
    lineWidth $= 1
    renderPrimitive LineLoop . mapM_ vertex $ c
    setColor 0 1 1
    textP (cen c) (show (k:s))

textP (Point x y) s = text2D (double2Float x) (double2Float y) s

sh ((img,csl),sol) = do
    drawImage img
    pointCoordinates (size img)
    mapM_ shContLab csl
    let is = [0..length csl -1]
        res = restrict csl
        grs = groups is res
        cans = map subsets grs
        csls = map (separ csl) grs
    setColor 0 0 0
    text2D 0.9 0.6 (show res)
    let hyp = concatMap (findNodes sol res) (zip cans csls)
        f ((_,k),_) = k `elem` hyp
    setColor 0 0.5 1
    lineWidth $= 5
    mapM_ shCont (map (fst.fst)  $ filter f csl)
    --print $ groups is res

----------------------------------

shCont c = renderPrimitive LineLoop . mapM_ vertex $ c

cen cont = (Point cx cy) where
    (cx,cy,_,_,_) = momentsContour cont

-----------------------------------

adj eps (c1,i) (c2,j) = md < eps && (i /= j)
    where md = minimum [distPoints a b | a <- c1, b <- c2 ]

adjs eps cs = map (id &&& g) cs where
     g a = map snd $ filter (adj eps a) cs

adjacencies = adjs (40/640) . flip zip [0::Int ..] . map polyPts

------------------------------------

restrict = filter incre . concatMap f where
    f ((_,k),s) = map ((,) k) s
    incre (a,b) = a<b

subsets = filterM (const [True,False])

decsort = sortBy (compare `on` negate.length) 

ok1 res ns = and [ok ns r | r <- res]
    where ok ns (a,b) = not (a `elem` ns && b `elem` ns)

ok2 cs ns = and [ok c ns | c <- cs]
    where ok ((_,k), vs) ns = (k `elem` ns) || length vs < 2 || length (filter (`elem` ns) vs) > 1


findNodes k res (ns,csl) = (cyk k) $ (++[[]]) $ (U.debug "sols:" length) $ decsort csn where
    csn1 = filter (ok1 res) ns
    csn2 = filter (ok2 csl) csn1
    csn = csn2


cyk k l = l !! max 0 (min (length l -1) k)

separ xs ns = filter g xs where g ((_,k),_) = k `elem` ns

--------------------------------------

-- connected components from the adjacencies
groups :: [Int] -> [(Int,Int)] -> [[Int]]
groups ns rs = map Tree.flatten $ scc g where
    g = buildG (0,length ns-1) (rs++ map h rs) where h (a,b) = (b,a)
