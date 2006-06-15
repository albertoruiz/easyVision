-- 3D reconstruction from point correspondences

module Main where

import Vision
import GSL 
--import Stat
import Data.List(genericLength)
import System(system)

mean l = sum l / genericLength l
         
gnuplot command = do
    writeFile "gnuplotcommand" $ command ++ "; pause -1 'Press return to continue...'"
    system "gnuplot gnuplotcommand" 
    system "rm gnuplotcommand"
    
main = do    
    let datafile = "correspondences.txt"
    correspondences <- fromFile datafile
    gnuplot $ "set size square; plot [-120:120] [-120:120] '"++datafile++"' using 1:2 with points title 'first view'"
    gnuplot $ "set size square; plot [-120:120] [-120:120] '"++datafile++"' using 3:4 with points title 'second view'"
    let rawleft  = toList $ takeColumns 2 correspondences
    let rawright  = toList $ dropColumns 2 correspondences
    let leftpts = map (\[x,y]->[-x,y]) rawleft
    let rightpts = map (\[x,y]->[-x,y]) rawright 
    let f = estimateFundamental leftpts rightpts
    putStrLn "Fundamental matrix:"
    disp 8 (normat f)
    putStr "mean epipolar distance: "
    print $ mean $ epipolarQuality f leftpts rightpts
    let fs = linspace 500 (50,200)
    hplot [fs, vmap (qualityOfInducedEssential f) fs]
    let (e,df,err) = estimateEssential 170.0 f
    putStr "Estimated f: "
    print df
    putStr "with quality: "
    print err
    putStrLn "Essential matrix: "
    disp 8 (normat e)
    let ms = camerasFromEssential e
    mapM_ (print.normat) ms
    let m2 = ms!!3
    let (_,r,c) = factorizeCamera (m2)
    print r
    print c
    let x = triangulate [(kgen df <> cameraOrigin, leftpts), 
                         (kgen df <> m2,           rightpts)]               
    writeFile "points3D.txt" $ unlines $ map unwords $ map (map show) ([0,0,0]: toList c :x)
    gnuplot $ "set size square; set view 72,200; set ticslevel 0;"
            ++"set xlabel 'x'; set ylabel 'y'; set zlabel 'z'; "
            ++"splot [-1.2:1.2] [-1.2:1.2] 'points3D.txt' with points title '3D reconstruction'"

