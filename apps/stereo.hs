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
    correspondences <- fromFile "correspondences.txt"
    gnuplot "plot [-120:120] [-120:120] 'correspondences.txt' using 1:2 with points title 'first view'"
    gnuplot "plot [-120:120] [-120:120] 'correspondences.txt' using 3:4 with points title 'second view'"
    let leftpts  = toList $ takeColumns 2 correspondences
    let rightpts = toList $ dropColumns 2 correspondences
    let f = estimateFundamental leftpts rightpts
    disp 8 (normat f)
    print $ mean $ epipolarQuality f leftpts rightpts
    let fs = linspace 500 (50,200)
    hplot [fs, vmap (qualityOfInducedEssential f) fs]
    let (e,df,err) = estimateEssential 170.0 f
    print df
    print err
    disp 8 (normat e)
    print $ norm $ flatten $ (normat e)
    let ms = camerasFromEssential e
    mapM_ (print.normat) ms
    let x = triangulate [(kgen df <> cameraOrigin, leftpts), 
                         (kgen df <> ms!!3, rightpts)]
    print $ head x 
    writeFile "points3D.txt" $ unlines $ map unwords $ map (map show) x
    gnuplot $ "set xlabel 'x'; set ylabel 'y'; set zlabel 'z'; set view 75,160;"
            ++"splot 'points3D.txt' with points title '3D reconstruction'"

