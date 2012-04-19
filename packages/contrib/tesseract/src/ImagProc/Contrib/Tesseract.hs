-----------------------------------------------------------------------------
{- |
Module      :  ImagProc.Contrib.Tesseract
Copyright   :  (c) Alberto Ruiz 2011
License     :  GPL

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional


Interface to tesseract OCR (currently using temp files and system calls.)

-}
----------------------------------------------------------------------

module ImagProc.Contrib.Tesseract (
  tesseract
-- , ocrWindow
-- ,  espeak
)
where

----------------------------------------------------------------------

import ImagProc                         (ImageGray,saveGray)
import System.FilePath.Posix
import System.Directory
import System.IO.Temp
import System.IO.Unsafe                 (unsafePerformIO)
import System.Process                   (system)
----------------------------------------------------------------------
{-
espeak :: String -> IO ()
espeak text = do
    _ <- readProcessWithExitCode "espeak" (words "--stdin -v en") text
    return ()
-}
----------------------------------------------------------------------

tesseract :: ImageGray -> String
tesseract im = 
  unsafePerformIO $ withSystemTempFile "ocr.tif" $ 
  (\f _h  -> do 
      let f' = replaceExtension f "txt"
          fbase = dropExtension f
      saveGray f im
      _ <- system $ "tesseract " ++ f ++ " " 
           ++ fbase ++ " -l eng   2> kkk.txt"
      s <- readFile $ f'
      removeFile f
      removeFile f'
      return s
  )     

----------------------------------------------------------------------
{-
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
-}

