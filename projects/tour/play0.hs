import Vision.GUI
import Image.Processing
 
main = run $ transUI f >>> freqMonitor
 
f :: VCN Channels (Image RGB)
f = return . adaptMb $ \cam -> do
    x <- cam
    let r = rgb x
    print (size r)
    return r

