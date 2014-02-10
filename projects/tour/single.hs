import Vision.GUI
import Image.Processing
import Image.Capture
import System.Environment

f = sumPixels . grayscale . channelsFromRGB

main = getArgs >>= readImages >>= runITrans (arr f) >>= print

