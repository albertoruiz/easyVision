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
    let raw  = toList $ takeColumns 2 correspondences
    let raw'  = toList $ dropColumns 2 correspondences
    let pts = map (\[x,y]->[-x,y]) raw
    let pts' = map (\[x,y]->[-x,y]) raw' 
    let f = estimateFundamental pts' pts
    putStrLn "Fundamental matrix:"
    disp 8 (normat f)
    putStr "mean epipolar distance: "
    print $ mean $ epipolarQuality f pts' pts
    let fs = linspace 500 (50,200)
    hplot [fs, vmap (qualityOfInducedEssential f) fs]
    let (e,df,err) = estimateEssential 170.0 f
    putStr "Estimated f: "
    print df
    putStr "with quality: "
    print err
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
    writeFile "points3D.txt" $ unlines $ map unwords $ map (map show) ([0,0,0]: toList c :x)
    gnuplot $ "set size square; set view 72,200; set ticslevel 0;"
            ++"set xlabel 'x'; set ylabel 'y'; set zlabel 'z'; "
            ++"splot [-1.2:1.2] [-1.2:1.2] 'points3D.txt' with points title '3D reconstruction'"
    