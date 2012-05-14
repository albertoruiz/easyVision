{-# LANGUAGE Arrows #-}

import Vision.GUI
import ImagProc
import ImagProc.GPU.SIFT
import Util.Misc(debug,quartiles,mean,stdpix)
import Control.Arrow((&&&))
import Data.List
import Data.Function(on)

type Matcher = [InterestPoint] -> [InterestPoint] -> Links

type Links = [(Segment,[Int])]

type ExtImg = (Int,(Channels,[InterestPoint]))

main = do
    prepare
    match <- getMatchGPU
    runNT_ camera $ sift grayscale >>> arrL (zip [0..])
                >>> history 6 >>> addMatcher match >>> arr bestLink
        --        >>> see2
                >>> mov
                >>> history 11 >>> addMatcher match >>> arr bestLink
                >>> see
    
history :: Int -> ITrans a [a]
history n = arrL (map (reverse . take n) . tails)
    
addMatcher m = arr (id &&& const (ni m 0.7 0.8))

bestLink :: (([ExtImg]), Matcher) -> (ExtImg, (Links, ExtImg))
bestLink (xs,m) = (h,b)
  where
    h:ts = xs
    (i,(img,ps)) = h
    cs = debug "L" (map (length.fst)) $ map (m ps . snd . snd &&& id) ts
    b = maximumBy (compare `on` crit) cs
    (links, (j,(img',qs))) = b
    crit = crit2
      where
        crit1 = length . fst
        crit2 = sum . map (segmentLength . fst) . fst


mov = arrL (map f . filter g)
  where
    g (_,(ls,_)) = mean (map (segmentLength.fst) ls) > 5*stdpix
    f = fst


ni m t1 t2 as bs = naiveInliers as bs (m t1 t2 as bs)

naiveInliers :: [InterestPoint] -> [InterestPoint] -> [[Int]] -> Links
naiveInliers ps qs ms = map snd . filter ((<t).fst) $ lm
  where
    sms = segMatches ps qs ms `zip` ms
    ds = map (segmentLength.fst) sms
    lm = zip ds sms
    (_,_,m,q,_) = quartiles ds
    t = q+(q-m)
    segMatches ps qs ms = map (\[a,b]-> (Segment (ipPosition (ps!! a)) (ipPosition(qs!! b)))) ms


see :: ITrans (ExtImg, (Links, ExtImg)) (ExtImg, (Links, ExtImg))
see = observe "tracks" g
  where
    g ((i,(x,_)),(links,(j,_))) = Draw [ Draw (rgb x)
                         , (lineWd 2 . color red) $ map fst links
                         , text (Point 0.9 0.6) $ show ((i,j,length links))
                --         , text (Point 0.9 0.5) $ show (mean $ map (segmentLength.fst) links)
                         ]


see2 :: ITrans (ExtImg, (Links, ExtImg)) (ExtImg, (Links, ExtImg))
see2 = observe "short term" g
  where
    g ((i,(x,_)),(links,(j,(y,_)))) = Draw [ Draw (rgb y)
                         , (lineWd 2 . color red) $ map fst links
                         , text (Point 0.9 0.6) $ show ((i,j,length links))
                         , text (Point 0.9 0.5) $ show (mean $ map (segmentLength.fst) links)
                         ]



{-

extend :: [[Int]] -> [[Int]] -> [[Int]]
extend as bs = cs
  where
    as' = sortBy (compare `on` last) as
    bs' = sortBy (compare `on` head) bs
    cs = merge as' bs'
    merge [] _ = []
    merge _ [] = []
    merge r@([a,b]:as) s@([c,d]:bs) | b < c = merge as s
                                    | b > c = merge r bs
                                    | otherwise = [a,d] : merge as bs

extension :: [(Channels, ([InterestPoint], [(Segment, [Int])]))] -> [[Int]]
extension xs = debug "E" length $ foldl1' extend $ debug "M" (map length) $ (take 5) $ ms
  where
    ms = map (map snd . snd . snd) xs :: [[[Int]]]

-}

