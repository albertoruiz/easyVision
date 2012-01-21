import Vision.GUI
import ImagProc
 
main = run $ transUI f >>> freqMonitor
 
f :: VC Channels ImageRGB
f cam = return $ do
    x <- cam
    let r = rgb x
    print (size r)
    return r

