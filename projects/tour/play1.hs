import EasyVision.GUI
import ImagProc

main = run camera p

p = observe "RGB" rgb .> grayscale >>> observe "inverted" notI

