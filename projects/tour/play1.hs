import EasyVision.GUI
import ImagProc

main = run p

p = observe "RGB" rgb .> grayscale >>> observe "inverted" notI

