import EasyVision
 
main = run $ camera ~> grayscale >>= observe "invert" notI

