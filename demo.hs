module Main where

import GSL

x = linspace 1000 (0,2*pi)

main = do
    -- hplot [x, sin x + 0.2 * sin (10*x)]
    let f k = 0.2 * fromIntegral k
    -- plotOpenGL $ \k -> (x, cos x + 0.2 * cos (5*x- f k))
    let w k = fromIntegral k / 100
    let rg = linspace 60 (-2,2)
    let (x,y) = meshdom rg rg
    let r2 = x*x+y*y
    meshOpenGL $ \k -> exp (-r2) * cos (w k *r2)
    
