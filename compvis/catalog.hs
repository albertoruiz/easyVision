import EasyVision
import Graphics.UI.GLUT

main = do
    sz <- findSize
    Just file <- getRawOption "--catalog"
    catalog <- readCatalog (file++".avi") sz (file++".labels") Nothing id
    n <- getOption "--goto" 1
    prepare
    catalogBrowser n catalog "catalog" sz
    mainLoop
