
import Classifier
import Classifier.ToyProblems
import Util.Stat
import Classifier.Regression(msError)
import Util.ICA
import Util.Misc(debug,diagl,vec)
import Util.Gaussian(mixturePDF,findMixture)

import Numeric.LinearAlgebra
import EasyVision hiding (whitener)
import Data.Colour.Names as Col
import Graphics.UI.GLUT hiding (Size,scale)
import Data.Maybe(maybe)
import System.Random(randomIO)
import Text.Printf(printf)
import Control.Monad((>=>))
import qualified GP
import Graphics.Plot

---------------------------------------------------------------------------

colors = [red,blue,orange,green]++repeat Col.lightgray

runIt f = prepare >> f >> mainLoop

scw title p = scatterPlot title (Size 400 400) p (0,1) colors (return ())

scwc title p clasif = scatterPlot title (Size 400 400) p (0,1) colors (drawRegion clasif p colors)

scwm title p met = scwc title p (mode . met p)

scwme title evi p met = scwc title p (maybe "REJECT" id . reject evi . met p)

scw3 title p = scatterPlot3D title 400 p (0,1,2) colors (return ())

rawmnist = loadExamples "../../../data/mnist.txt"

---------------------------------------------------------------------------

main = test sshape


test x = do
    seed <- randomIO
    let p = addNoise seed 0.1 $ x 200
        xy = GP.matData p
        sigmas = [0.1,0.2 .. 2]
        f s = GP.lik xy s 0.1
        liks = vec (map f sigmas)
    mplot [vec sigmas, liks]
    let sigma = sigmas!!maxIndex liks
    print sigma
    runIt $ do
        -- scwm "Old Kernel" p (multiclass $ GP.kmse (10*eps) (gaussK 1))
        -- scwm "gp" p (multiclass $ GP.gp' 0.1 (gaussK 1))
        scwm "Gaussian Process" p (multiclass $ GP.gp1 0.1 (GP.gaussK sigma))

