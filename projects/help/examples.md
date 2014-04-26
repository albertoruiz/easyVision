examples

# hVision examples


## projects/examples

**grid**: manipulation of the live video stream using standard list functions.

**gradient**: show the image gradient as a vector field.

**warp**: apply a synthetic rotation with interactive parameters to the input stream.

**imagproc**: interactive illustration of several image processing functions.

**keypoints**: extraction of interest points based on Hessian.

**hessharr**: illustration of intermediate steps for interest point detection.

**resize**: illustration of resize and resizeFull.

**mirror**: "mirror" effect (vertical reflection of the left part of the image).

**crosscorr**: image matching of user-selected regions in live video using cross-correlation.

**otsu**:  illustration of Otsu's method for automatic thresholding.

**trail**: "ghost" effect using arrow notation and recursive do.

**twist**: linear color transformation.

**color**: image matching using intersection of color histograms.

**chroma**: masking images using UV color space on static image.

**chroma2**: same as *chroma*, on a progressively zoomed image.

**frames**: create a list of progressively zoomed images from a given image.

**domain**: image domain transformation using lookup images.

**opencv1**: test of opencv's functions: *canny* and *hough*.

**opencv2**: opencv's face detector.

**opencv3**: compare opencv's *findHomography* with normalized linear estimation.

**st**: explicit pixel manipulation using the ST monad.

**glyph**: extraction of letter fragments.

**zcontours**: efficient contour extraction.

**match2d**: compare similar, affine, and projective estimates of the transformation between two sets of 2D points.

**trazo**: online affine alignment of mouse-written strokes. Press 'a' to add prototype, 'x' to clear the screen.

**ransac**: demonstration of ransac estimation of line and circle models from 2D samples with lots of outliers.

**spline**: interactive spline demo.

**pose**: camera pose estimation and augmented reality from a planar reference. Run as:

<pre id="samp">
    ./pose '../../data/videos/rot4.avi -loop 0'
</pre>

**cameracontrol**: interactive control of webcam parameters: exposure, focus, etc.

**matrix**: Usage of matrix functions on images:

HSFILE(../tour/matrix.hs)

**cinematic**: compute very simple inverse cinematics.


## projects/vision/geom

See the Makefile or run "make" and "make demo" to run these programs with interesting command line arguments.

**horizon**: click points of rectangle to compute the horizon of the plane. 

**horizon2**: detect a rectangle in a video and show the horizon.

**conic**: click points and see the intersection line-conic.

**crossratio**: predict position of equispaced points in image

**linemodels**: click points and show the estimated line using different linear costs.

**rectify**: click points in the image of a square to rectify the plane.

**synthcam**: create a camera matrix from a projection center and a target point and show in 3D.

**resection, resection2, resection4**: interactive 3D illustration of camera estimation from 3D-2D correspondences

**stereo**: interactive 3D illustration of a stereo geometry.

**multiview**: experiments with the multiview tensors.

## projects/patrec

**democlass**: display the decision regions of simple classifiers in toy 2D problems.

**scatters**: several views of the MNIST handwritten digits database in PCA and MDF space.

**demotest**: 3D representation of PCA for a part of the MNIST database and other experiments.

**mnist**: see the principal modes of variation in the mnist database.

**probability**: solve typical problems using the probability monad.

**bayesgauss**: illustration of bayesian inference for multivariate gaussian distributions. 

**gptest**: example of regression using Gaussian processes.

**seecov**: click points and see the level curves of the gaussian model.

**em**: 2D example of estimation of a gaussian mixture model using EM.

**seemix**: click points and see the mixture model estimated using EM.

**median**: illustration of Weiszfeld's iteration for the geometric median.

**cs**: simple example of L1 minimization and compressed sensing.

**graph**: experiments in spectral graph matching.

**pls**: experiment in partial least squares regression and related methods.

**ukf**: check UKF (unscented Kalman filter) functions.

**ferns**: experiment with naive bayes and ferns.

