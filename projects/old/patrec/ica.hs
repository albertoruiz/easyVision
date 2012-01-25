import Numeric.LinearAlgebra
import Util.Stat
import Util.ICA
import Util.Misc(vec,Vec,Mat,degree,loadAudio,savePlayAudio,debug)
import Graphics.Plot(mplot)
import EasyVision (prepare,scatterPlot,Size(..))
import Graphics.UI.GLUT(mainLoop)
import Data.Colour.Names as Col
import System.Process(system)
import Data.Function(on)
import Data.List(sortBy)

noisy sigma v = v + scalar sigma * randomVector 777 Gaussian (dim v)

t = linspace 1000 (0,2*pi) :: Vec

s1 = sin (2*t)
s2 = cos (3*t) + 0.5 * cos (5*t)


s = fromColumns [s1,s2]

rot alpha w = w <> (2><2) [cos alpha, -sin alpha, sin alpha, cos alpha]

negentropy met w alpha = dif met . head . toColumns $ rot alpha w
    where dif (f,k) x = (sumElements (f x) / fromIntegral (dim x) - k)^2

kurt2 = ((** 4),3)
expq = (\x -> exp (-x^2/2), 1/sqrt 2)
cosq5 = ((\x -> 1/a * log (cosh (a*x))), 0.67223) where a = 5
cosq1 = ((\x -> 1/a * log (cosh (a*x))), 0.374567) where a = 1

qua met w = mapVector (negentropy met w) (linspace 180 (0,179*degree))

dif (f,k) x = (sumElements (f x) / fromIntegral (dim x) - k)**2


study m1 m2 = do
    let m = fromColumns [m1,m2]
        w = whitenedData $ stat m
    let ica = fst . debug "err: " snd . icaTrans 50 (ica2 0.01) $ w
    print $ ica
    --error "ya"
    prepare
    --mplot [t,s1,s2]
    --mplot [t,m1,m2]
    --scw "true" s
    scw "whitened" w
    let qua1 = qua kurt2 w
        qua2 = qua expq w
        best = fromIntegral (maxIndex qua1)
        sol = rot (best*degree) w
    mplot [ qua1 ]
--    mplot [ qua2 ]
--    mplot [ qua cosq5 w]
--    mplot [ qua cosq1 w]
    scw ("sol emp " ++ show best) $ sol
    mplot $ (linspace (dim m1) (0,1)) : toColumns sol
    let solok = w <> trans ica
    scw ("sol ica ") $ solok
    mapM_ (print . dif kurt2) (toColumns solok)
    mainLoop
    return (toColumns solok)

main = do
    let m1 = noisy 0.1 $ s1 + 0.5 * s2
        m2 = noisy 0.1 $ s2 - 0.3 * s1
    --mplot [t,s1,s2]
    --mplot [t,m1,m2]
    study m1 m2
    return ()


async = do
    let m1 = subVector  0 950 $ noisy 0.1 $ s1 + 0.5 * s2
        m2 = subVector 30 950 $ noisy 0.1 $ s2 - 0.3 * s1
        t' = subVector  0 950 t
    mplot [t ,s1,s2]
    mplot [t',m1,m2]
    --scw "true" s
    study m1 m2
    return ()


audio = do
    r1 <- loadAudio "ica/mix1.wav"
    r2 <- loadAudio "ica/mix2.wav"
    let [[t,a1],[_,a2]] = map toColumns [r1,r2]
    print (dim a1,dim a2)
    [s1,s2] <- study a1 a2
    let o1 = fromColumns [t,s1]
        o2 = fromColumns [t,s2]
--    saveMatrix "ica/s1.dat" "%f" o1
--    saveMatrix "ica/s2.dat" "%f" o2
    savePlayAudio 8000 o1 "ica/s1.dat"
    savePlayAudio 8000 o2 "ica/s2.dat"
    return ()


audiostereo = do
    r <- loadAudio "ica/albertos.wav"
    let [t,a1,a2] = toColumns r
    print (dim a1,dim a2)
    [s1,s2] <- study a1 a2
    let o1 = fromColumns [t,s1]
        o2 = fromColumns [t,s2]
--    saveMatrix "ica/s1.dat" "%f" o1
--    saveMatrix "ica/s2.dat" "%f" o2
    savePlayAudio 44100 o1 "ica/s1.dat"
    savePlayAudio 44100 o2 "ica/s2.dat"
    return ()

-- play -r 8000 ica/s1.dat -v 0.5
-- play ica/mix1.wav
---------------------------------------------------------------------------

-- show 2D points in a matrix n x 2
scw tit p = do
    let p' = map (\x->(x,"x")) (toRows p)
--    prepare
    scatterPlot tit (Size 400 400) p' (0,1) colors (return ())
--    mainLoop

colors = [red,blue,lightgreen]++repeat Col.lightgray

