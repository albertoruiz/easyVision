import EasyVision
import Graphics.UI.GLUT
import System(getArgs)

main = do
    sz <- findSize
    file:_ <- getArgs
    catalog <- readCatalog (file++".yuv") sz (file++".labels") Nothing id
    n <- getOption "--goto" 1
    prepare
    catalogBrowser n catalog file sz
    mainLoop
