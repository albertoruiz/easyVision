-- 3D reconstruction from point correspondences

module Main where

import Vision
import GSL 
import Data.List(genericLength)
import System(system)
import Debug.Trace(trace)

mean l = sum l / genericLength l
         
prep = (++"e\n") . unlines . map (unwords . (map show))         
         
gnuplot command = do
    writeFile "gnuplotcommand" $ command ++ "; pause -1 'Press return to continue...'"
    system "gnuplot gnuplotcommand" 
    system "rm gnuplotcommand"
    
plotpoints l = do
    gnuplot $ "set size square; plot [-500:500] [-500:500]  \"-\" with points\n"
              ++ prep l
    
flipx = \[x,y]->[-x,y]
    
shcam cam pts = (c,p) where 
    (h,f) = toCameraSystem cam
    t1 = h <> diag (realVector [1,1,1,3])
    c = ht t1 (cameraOutline 1)
    t2 = t1 <> diag (realVector [1,1,1,f])
    p = ht t2 (map (++[f]) pts)
    
    
stereo datafile = do    
    correspondences <- fromFile datafile
    gnuplot $ "set size square; plot [-120:120] [-120:120] '"++datafile++"' using 1:2 with points title 'first view'"
    gnuplot $ "set size square; plot [-120:120] [-120:120] '"++datafile++"' using 3:4 with points title 'second view'"
    let raw  = toList $ takeColumns 2 correspondences
    let raw'  = toList $ dropColumns 2 correspondences
    let pts = map flipx raw
    let pts' = map flipx raw' 
    let f = estimateFundamental pts' pts
    putStrLn "Fundamental matrix:"
    disp 8 (normat f)
    putStr "mean epipolar distance: "
    print $ mean $ epipolarQuality f pts' pts
    let fs = linspace 500 (50,200)
    mplot [fs, vmap (\x -> qualityOfEssential (kgen x <> f <> kgen x)) fs]
    let (e,df,err) = estimateEssential 170.0 f
    putStr "Estimated common f using equalization: "
    print df
    putStr "with quality: "
    print err
    putStr "Estimated fs using equalization: "
    let (_,(df,df'),err) = estimateEssential' (170.0,170.0) f
    print (df,df')
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
    disp 8 (normat e)
    putStrLn "candidate cameras:" 
    print $ camerasFromEssential e
    let m = kgen df <> cameraAtOrigin
    putStrLn "first camera: "
    print m
    let ms = map (kgen df <>) (camerasFromEssential e)
    let m' = selectCamera (head pts) (head pts') m ms
    putStrLn "second camera: "
    print m'
    let (_,_,c) = factorizeCamera m'
    let x = triangulate [(m, pts), (m', pts')] 
    let (mo,v)   = shcam m  pts
    let (mo',v') = shcam m' pts'                           
    let sp = [head mo, head x, head mo']              
                  
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
   

rectif datafile = do
    correspondences <- fromFile datafile
    let raw  = toList $ takeColumns 2 correspondences
    let raw'  = toList $ dropColumns 2 correspondences
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
    let ax = realMatrix (map flipx rec)
    let ax' = realMatrix (map flipx rec')
    let auxmat = toList $ ax <|> ax'- ax
    writeFile "dispar.txt" $ unlines $ map unwords $ map (map show) auxmat
    gnuplot "set size square; plot [-120:120] [-120:120] 'dispar.txt' with vectors"
    system "rm dispar.txt"
    
    
main = do
    stereo "correspondences.txt"
    rectif "correspondences.txt"
