import EasyVision

c = camera >>= observe "image" rgb

main = run c
