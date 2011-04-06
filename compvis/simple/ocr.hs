import EasyVision
import Graphics.UI.GLUT hiding (Point,Size)
import Data.Colour.Names as Col hiding (gray)
import System.Process(system,readProcessWithExitCode)
import System.IO(openTempFile)
import Foreign(unsafePerformIO)

----------------------------------------------------------------------

main = runFPS 10
    $   camera
    >>= regionMarker (mpSize 20) rgb 
    >>= rectifyRegion gray 400 .@. winAspectRatioParam ~> snd.snd
    >>= ocrWindow autoBinarize
    >>= timeMonitor

----------------------------------------------------------------------

espeak :: String -> IO ()
espeak text = do
    _ <- readProcessWithExitCode "espeak" (words "--stdin -v en") text
    return ()

tesseract :: ImageGray -> String
tesseract im = unsafePerformIO $ do
    (f,h) <- openTempFile "." "ocr"
    saveGray (f++".tif") im
    system $ "tesseract "++f++".tif "++f++" -l eng 2> /dev/null"
    s <- readFile $ f++".txt"
    system $ "rm -f "++f++" "++f++".*" 
    return s

----------------------------------------------------------------------
    
autoBinarize img = binarize8u (otsuThreshold img) img

----------------------------------------------------------------------

ocrWindow :: (x -> ImageGray) -> IO x -> IO (IO (x, String))
ocrWindow sel = clickStatusWindow "Tesseract OCR" (mpSize 10) "" f g (const espeak) where
    f x _ = tesseract (sel x)
    g x s = do
        let x' = sel x
        drawImage' x'
        pixelCoordinates (size x')
        setColor' blue
        text2D 20 20 (concatMap h s)
    h '\n' = " <CR> "
    h x = [x]       

