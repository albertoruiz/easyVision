import EasyVision.GUI
import ImagProc

main = run $ arr grayscale >>> observe "invert" notI

