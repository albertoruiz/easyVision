import EasyVision
 
main = run $ camera ~> rgb >>= observe "RGB" id

