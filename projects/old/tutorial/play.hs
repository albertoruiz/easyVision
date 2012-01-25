import EasyVision

main = run $ camera >>= observe "Video" rgb
