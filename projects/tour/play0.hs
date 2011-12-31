import EasyVision
 
main = run $ camera >>= f >>= timeMonitor
 
f :: IO Channels -> IO (IO ImageRGB)
f cam = return $ do
    x <- cam
    let r = rgb x
    print (size r)
    return r

