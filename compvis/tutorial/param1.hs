import EasyVision
import Tutorial(run, camera, observe)

smooth cam = do
    o <- createParameters [("sigma",realParam 3 0 20)]
    return $ do
        x <- cam
        sigma <- getParam o "sigma"
        return (gaussS sigma x)

main = run $ camera ~> float . gray >>= smooth >>= observe "gauss" id
