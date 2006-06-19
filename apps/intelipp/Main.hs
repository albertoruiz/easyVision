import Ipp
import Typical
import Draw
import Camera 

import System.Environment(getArgs)
     
testImage (r,c) = do 
    w <- img 4 1 r c
    set32f 0.0 w (fullroi w)
    set32f 0.5 w $ ROI {r1=50, c1=50, r2 = 250, c2=250}  
    let roi = ROI {r1=100, c1=100, r2 = 200, c2=200}  
    ippiImageJaehne_32f_C1R // dst w roi // checkIPP "ippiSetImageJanehne" [w]
    return w
    

secondOrder image = do
    gx  <- filterSobelVert image
    gy  <- filterSobelHoriz image
    gxx <- filterSobelVert gx
    gyy <- filterSobelHoriz gy
    gxy <- filterSobelHoriz gx
    return (gx,gy,gxx,gyy,gxy)
     
hessian image = do
    (gx,gy,gxx,gyy,gxy) <- secondOrder image
    ab <- gxx |*| gyy
    cc <- gxy |*| gxy
    h  <- ab  |-| cc
    return h

times 0 f = return
times n f = g where
    g x = do
        v <- f x >>= times (n-1) f
        return v

visor cam k = do
    im  <- grab cam
    imf <- scale8u32f 0 1 im  >>= gauss 55
    gx  <- filterSobelVert imf
    gy  <- filterSobelHoriz imf
    agx <- abs32f gx
    agy <- abs32f gy
    g   <- add32f agx agy >>= gauss 55
    mg  <- filterMax32f 3 g >>= filterMax32f 3
    lm  <- compare32f 2 mg g
    return mg

visor' cam k = do
    im  <- grab cam
    imf <- scale8u32f 0 0.5 im >>= (2 `times` gauss 55)
    h   <- hessian imf  >>= localMax >>= thresholdVal32f 0.3 0.0 0 >>= thresholdVal32f 0.3 1 4
    return h
    
visor'' cam k = do
    im  <- grab cam
    imf <- scale8u32f 0 1 im >>= (3 `times` gauss 55)
    h   <- hessian imf >>= abs32f >>= sqrt32f
    return h
        
    
main = do
    args <- getArgs
    cam@(_,_,(h,w)) <- openCamera (args!!0) 1 (288,384)
    imageShow (w,h) (visor'' cam)
    
    

pyr im k = do
    let roi = ROI {r1=150, c1=20, r2 = 290, c2=290}  
    r <- copy32f im
    (ippiFilterGauss_32f_C1R // src im roi // dst r roi) 33 // checkIPP "gauss" [im,r] --3x3 mask (or 55 (5x5))
    ippiCopy_32f_C1R // src r roi // dst im roi // checkIPP "copy" [im,r]
    if k == 1000 then error "OK"
                else return r
    
main' = do
    w <- testImage (300,500)
    imageShow (300,300) (pyr w) 
