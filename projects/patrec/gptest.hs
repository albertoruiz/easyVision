
import Classifier
import Classifier.ToyProblems
import Numeric.LinearAlgebra
import Util.Misc(vec)
import Vision.GUI
import System.Random(randomIO)
import Text.Printf(printf)
import qualified Util.GP as GP
import Util.ScatterPlot
import Graphics.Plot


---------------------------------------------------------------------------


scatterPlots name exs mets = browser name xs (const id)
  where
    xs = map f mets
    f (met, name) = scatter exs (0,1) [] (windowTitle name $ drawDecisionRegion 71 exs [] met)



scwc title p clasifs = scatterPlots title p clasifs

scwm title p met = scwc title p [(mode . met p, "gp")]

scwme title evi p met = scwc title p [(maybe "REJECT" id . reject evi . met p, "gp")]


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

