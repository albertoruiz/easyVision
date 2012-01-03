import EasyVision
 
main = run $ camera >>= observe "RGB" rgb >>= timeMonitor

