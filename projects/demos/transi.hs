import EasyVision.GUI
import ImagProc
import ImagProc.Camera(openYUV4Mpeg)
import Contours.Base(setRegion)
import Graphics.UI.GLUT.Callbacks
import Control.Applicative((<$>))
import System.Environment(getArgs)
import Control.Monad(when)
import System.Directory(doesFileExist)
import Data.List(zipWith4)
import Numeric.LinearAlgebra(linspace, toList)
import System.Process(system)

fps = 25

-- TO DO: pipe

editR2 sz@(Size h w) dir sv xs = editor upds save "transition" xs sh
  where
    ar = fromIntegral w / fromIntegral h
    sh k ((img,(p,q)),f) = Draw [ Draw img, lineWd 3, color red, drReg p, color blue, drReg q
                                 , text (Point 0.9 0) (show (1+k) ++ " " ++ f)]

    upds = [ updateItem (key (Char 'a')) initial
           , updateItem (key (Char 'a')) final ]

    initial r _ ((im, (_,q)),f) = ((im, (fixAR ar r,q)),f)
    final   r _ ((im, (p,_)),f) = ((im, (p,fixAR ar r)),f)
    
    save = [(ctrlS, \_roi _pt (_k,xs) -> do
                writeFile (dir ++ "/regions.txt") (show. map fsv $ xs)
                putStrLn "Saved!")
           , (f5, \_roi _pt (_k,xs) -> do
                putStrLn "Creating video... "
                mapM_ sv (concatMap (segment ar sz (9*fps) . fst) xs)
                putStrLn "Encoding video... "
                system $ "mencoder "++dir++"/video.y4m -vc rawi420 -fps "++show fps ++ " -ovc lavc -o "++dir++"/video.avi"
                putStrLn "Ok!")
           ]

    ctrlS = kCtrl . key . Char $ '\DC3'
    f5 = key (SpecialKey KeyF5)
    fsv = \((im,x),f)->(f,x)

main = do
    dir:_ <- getArgs
    sz <- findSize
    x <- readFolderIM dir
    let fn = dir ++ "/regions.txt"
    ok <- doesFileExist fn
    when (not ok) $ writeFile fn "[]"
    s <- read <$> readFile fn
    let g (im, f) = ((rgb im, maybe def id (lookup f s) ),f)
        y = map g x
    sv <- openYUV4Mpeg sz (dir++"/video.y4m") Nothing
    runIt $ editR2 sz dir sv y

def = ( (Point 0.6 0.6, Point 0.1 0.1)
      , (Point (-0.1) (-0.1), Point (-0.6) (-0.6)))

drReg (Point x1 y1, Point x2 y2) = (Draw . Closed) [ Point x1 y1, Point x2 y1
                                                   , Point x2 y2, Point x1 y2]

path :: Int -> (WinRegion, WinRegion) -> [WinRegion]
path n ((Point x1 y1, Point x2 y2),(Point x3 y3 , Point x4 y4)) = r 
 where
   f = toList . linspace n
   as = f (x1,x3)
   bs = f (y1,y3)
   cs = f (x2,x4)
   ds = f (y2,y4)
   r = zipWith4 (\a b c d -> (Point a b, Point c d)) as bs cs ds

segment ar sz n (img,ab) = map f (path n ab ++ replicate fps (snd ab))
  where
    f = toYUV . resize sz .  flip setRegion img . fixAR ar

fixAR ar (Point x1 y1, Point x2 y2) = (Point a1 b1, Point a2 b2)
  where
    w = ar*h
    h = abs (y2-y1)/2
    cx = (x1+x2)/2
    cy = (y1+y2)/2
    a1 = cx-w
    a2 = cx+w
    b1 = cy-h
    b2 = cy+h

