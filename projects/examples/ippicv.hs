import Vision.GUI.Simple
import Image
import Image.Devel
import Image.Processing.IPPICV

main = run $ observe "source" (float2gray . resize32f (Size 200 600) . gray2float . rgbToGray . rgb)

