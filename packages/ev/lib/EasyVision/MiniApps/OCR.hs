-----------------------------------------------------------------------------
{- |
Module      :  EasyVision.MiniApps.OCR
Copyright   :  (c) Alberto Ruiz 2011
License     :  GPL

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional


Interface to tesseract OCR using system calls.

-}
----------------------------------------------------------------------

module EasyVision.MiniApps.OCR (
    tesseract,
    ocrWindow,
    espeak
)where

----------------------------------------------------------------------

import ImagProc                         (ImageGray,saveGray,size)
import EasyVision.GUI                   (drawImage',setColor',text2D,pixelCoordinates)
import ImagProc.Camera                  (mpSize)
import EasyVision.MiniApps.Combinators  (clickStatusWindow)
import Data.Colour.Names                
import System.Process                   (system,readProcessWithExitCode)
import System.IO                        (openTempFile)
import System.IO.Unsafe                 (unsafePerformIO)

----------------------------------------------------------------------

espeak :: String -> IO ()
espeak text = do
    _ <- readProcessWithExitCode "espeak" (words "--stdin -v en") text
    return ()

----------------------------------------------------------------------

tesseract :: ImageGray -> String
tesseract im = unsafePerformIO $ do
    (f,_h) <- openTempFile "." "ocr"
    saveGray (f++".tif") im
    _ <- system $ "tesseract "++f++".tif "++f++" -l eng 2> /dev/null"
    s <- readFile $ f++".txt"
    _ <- system $ "rm -f "++f++" "++f++".*" 
    return s

----------------------------------------------------------------------

ocrWindow :: (x -> ImageGray) -> IO x -> IO (IO (x, String))
ocrWindow sel = clickStatusWindow "Tesseract OCR" (mpSize 10) "" f g (const espeak) where
    f x _ = tesseract (sel x)
    g x s = do
        let x' = sel x
            s' = concatMap h s
        drawImage' x'
        pixelCoordinates (size x')
        setColor' red; text2D 20 20 s'
    h '\n' = " <CR> "
    h x = [x]       

