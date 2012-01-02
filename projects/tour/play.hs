import EasyVision

pipeline = camera >>= observe "image" rgb

main = run pipeline
