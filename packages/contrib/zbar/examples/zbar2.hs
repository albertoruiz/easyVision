import Vision.GUI            (runITrans)
import Control.Arrow         (arr)
import ImagProc              (grayscale)
import ImagProc.Camera       (readImages)
import ImagProc.Contrib.ZBar (zbar, Barcode(..))
import System.Environment    (getArgs)
import Data.List             (intercalate)

main = getArgs >>= readImages >>= runITrans (arr f) >>= mapM_ putStrLn

f = intercalate ", " . map g . zbar . grayscale
  where
    g bc = bcType bc ++ ": " ++ bcValue bc

