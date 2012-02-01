import Vision.GUI
import ImagProc
import ImagProc.Camera
import System.Environment

f = sum8u . grayscale

main = getArgs >>= readImages >>= runITrans (arr f) >>= print

