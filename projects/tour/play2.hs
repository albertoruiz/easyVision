import EasyVision.GUI
import ImagProc

main = run camera $ arr grayscale >>> observe "invert" notI

