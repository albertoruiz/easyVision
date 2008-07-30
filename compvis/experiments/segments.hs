import EasyVision
import Graphics.UI.GLUT
import ImagProc.C.Burns

main = do
    sz <- findSize
    prepare

    (cam,ctrl) <- getCam 0 sz
                  >>= withChannels >>= withPause

    e <- evWindow () "segments" sz Nothing (const (kbdcam ctrl))

    op <- createParameters    [("radius",intParam 4 0 10),
                               ("width",realParam 1.5 0 5),
                               ("median",intParam 5 3 5),
                               ("high",intParam 40 0 255),
                               ("low",intParam 20 0 255),
                               ("postproc",intParam 1 0 1),
                               ("method",intParam 0 0 1)]

    bu <- createParameters [
              ("min line length",realParam 10 0 20),
              ("min gradient",intParam 25 0 100),
              ("buckets",intParam 7 4 10)]

    launch $ do

        radius <- getParam op "radius"
        width  <- getParam op "width"
        median <- getParam op "median"
        high   <- getParam op "high" :: IO Int
        low    <- getParam op "low"  :: IO Int
        postp  <- getParam op "postproc"
        let pp = if postp == (0::Int) then False else True

        minlen <- getParam bu "min line length"
        mingrad <- getParam bu "min gradient"
        buckets <- getParam bu "buckets"

        method <- getParam op "method"

        orig <- cam

        let img = gray orig
            segs = if method == (0::Int)
                    then segments radius width median (fromIntegral high) (fromIntegral low) pp img
                    else burns_line_extraction img buckets mingrad minlen
        inWin e $ do
            drawImage img
            setColor 1 0 0
            pointCoordinates (size img)
            renderPrimitive Lines $ mapM_ vertex segs




