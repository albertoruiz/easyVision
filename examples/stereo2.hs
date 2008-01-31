-- 3D reconstruction from point correspondences

module Main where

import Vision
import Numeric.LinearAlgebra
import Data.List(genericLength)
import System(system)
import System
import Graphics.Plot
import Text.Printf(printf)

matrix m = fromLists m :: Matrix Double
vector v = fromList v :: Vector Double
vmap = liftVector

disp n = putStrLn . format "  " (printf $ "%."++ show n++"f")

mean l = sum l / genericLength l
         
prep = (++"e\n") . unlines . map (unwords . (map show))         
         
gnuplot command = do
    writeFile "gnuplotcommand" $ command ++ "; pause -1 'Press return to continue...'"
    system "gnuplot -persist gnuplotcommand"
    system "rm gnuplotcommand"

plotpoints (x1,y1) (x2,y2) title l = do
    let rg a b = "["++show a++":"++show b++"]"
    gnuplot $ "set size square; plot "++rg x1 x2++" "++rg y1 y2++" \"-\" with points title '"++title++"'\n"
              ++ prep l

centerFlip xwidth [x, y] = [-(x-c)/c,-(y-c)/c] where c = fromIntegral xwidth / 2

flipx = \[x,y]->[-x,y]
    
shcam cam pts = (c,p) where 
    (h,f) = toCameraSystem cam
    t1 = h <> diag (vector [1,1,1,3])
    c = ht t1 (cameraOutline 1)
    t2 = t1 <> diag (vector [1,1,1,f])
    p = ht t2 (map (++[f]) pts)
    
correctFundamental :: Matrix Double -> Matrix Double
correctFundamental f = f' where
    (u,s,v) = svd f
    s1:s2:_ = toList s
    f' = u <> diag (fromList [s1,s2,0.0]) <> trans v



stereo xwidth datafile = do    
    correspondences <- readMatrix `fmap` readFile datafile
    let raw  = toLists $ takeColumns 2 correspondences
    let raw'  = toLists $ dropColumns 2 correspondences
    plotpoints (0,xwidth) (xwidth,0) "first view" raw
    plotpoints (0,xwidth) (xwidth,0) "second view" raw'
    let pts = map (centerFlip xwidth) raw
    let pts' = map (centerFlip xwidth) raw'
    plotpoints (1,-1) (-1,1) "first view" pts
    plotpoints (1,-1) (-1,1) "second view" pts'

    disp 3 $ takeRows 4 (matrix pts <|> matrix pts')

    let f = estimateFundamental pts' pts
    putStrLn "OK Fundamental matrix:"
    disp 8 (normat f)
    putStr "mean epipolar distance: "
    print $ mean $ epipolarQuality f pts' pts


    let f = estimateFundamentalRaw raw' raw
    putStrLn "Raw fundamental matrix:"
    disp 8 (normat f)
    putStr "mean epipolar distance: "
    print $ mean $ epipolarQuality f raw' raw

    let f = estimateFundamentalRaw pts' pts
    putStrLn "fundamental matrix with normalized points:"
    disp 3 (normat f)
    putStr "mean epipolar distance: "
    print $ mean $ epipolarQuality f pts' pts
    let (_,s,_) = svd f
    print s

    let f = correctFundamental $ estimateFundamentalRaw pts' pts
    putStrLn "corrected fundamental matrix with normalized points:"
    disp 3 (normat f)
    putStr "mean epipolar distance: "
    print $ mean $ epipolarQuality f pts' pts

    let fs = linspace 500 (0.1,5)
    mplot [fs, vmap (\x -> qualityOfEssential (kgen x <> f <> kgen x)) fs]
    
    let (e,df,err) = estimateEssential 0.1 f
    putStr "Estimated common f using equalization: "
    print df
    putStr "with quality: "
    print err
    putStr "Estimated fs using equalization: "
    let (_,(f1,f2),err) = estimateEssential' (0.1,0.1) f
    print (f1,f2)
    putStrLn "with quality: "
    print $ err

    putStr "Estimated f using Bougnoux's method: "
    let (f1,f2) = (bougnoux f, bougnoux (trans f))
    print (f1,f2)
    putStrLn "with quality: "
    print $ qualityOfEssential (kgen f2 <> f <> kgen f1)
    
    putStr "Estimated common f using Sturm's method: "
    let fs = sturm f
    print fs
    putStr "with qualities: "
    print $ map (\x -> qualityOfEssential (kgen x <> f <> kgen x)) fs

    putStrLn "Essential matrix: "
    disp 3 (normat e)
    putStrLn "candidate cameras:" 
    mapM_ (disp 3) $ camerasFromEssential e
    let m = kgen df <> cameraAtOrigin
    putStrLn "first camera: "
    disp 8 m
    let ms = map (kgen df <>) (camerasFromEssential e)
    let m' = selectCamera (head pts) (head pts') m ms
    putStrLn "second camera: "
    disp 3 m'
    let (_,_,c) = factorizeCamera m'
    let x = triangulate [(m, pts), (m', pts')] 
    let (mo,v)   = shcam m  pts
    let (mo',v') = shcam m' pts'                           
    let sp = [head mo, head x, head mo']              
    mapM_ print (take 4 x)

    gnuplot $ "set size square; set view 72,200; set ticslevel 0;"
            ++"set xlabel 'x'; set ylabel 'y'; set zlabel 'z';"
            ++"splot [-1.2:1.2] [-1.2:1.2] [-0.2:2.2] "
            ++"\"-\" title '3D reconstruction' with points 30, "
            ++"\"-\" title 'first camera' with lines 1, \"-\" title 'second camera' with lines 3,"
            ++"\"-\" title 'first image' with dots 1, \"-\" title 'first image' with dots 3,"
            ++"\"-\" title 'sample point' with lines 31\n"
            ++ prep x
            ++ prep mo
            ++ prep mo'
            ++ prep v
            ++ prep v'
            ++ prep sp
   

{-
rectif datafile = do
    correspondences <- readMatrix `fmap` readFile datafile
    let raw  = toLists $ takeColumns 2 correspondences
    let raw'  = toLists $ dropColumns 2 correspondences
    let pts = map flipx raw
    let pts' = map flipx raw' 
    let f = estimateFundamental pts' pts
    putStrLn "Fundamental matrix:"
    disp 8 (normat f)
    putStr "mean epipolar distance: "
    print $ mean $ epipolarQuality f pts' pts
      
    let (ep,ep') = epipoles f
    plotpoints $ (flipx $ toList $ inHomog ep) : raw
    plotpoints $ (flipx $ toList $ inHomog ep') : raw'
    
    let (h, h') = stereoRectifiers f pts pts'
    let rec = ht h pts
    let rec' = ht h' pts'
    let ax = matrix (map flipx rec)
    let ax' = matrix (map flipx rec')
    let auxmat = toLists $ ax <|> ax'- ax
    writeFile "dispar.txt" $ unlines $ map unwords $ map (map show) auxmat
    gnuplot "set size square; plot [-120:120] [-120:120] 'dispar.txt' with vectors"
    system "rm dispar.txt"
-}
    
main = do
    stereo 640 "corrs.txt"
    --rectif "correspondences.txt"
