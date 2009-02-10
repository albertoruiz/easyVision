import EasyVision
import Tutorial(camera, observe, run)

drift r (a:b:xs) = a : drift r ((r .* a |+| (1-r).* b):xs)

main = run $ camera ~> float.gray ~~> drift 0.9 >>= observe "drift" id
