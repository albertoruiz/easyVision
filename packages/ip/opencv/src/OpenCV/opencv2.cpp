#ifdef OPENCV24
#include "opencv2/nonfree/features2d.hpp"
#endif

#ifdef OPENCV3
#include "opencv2/objdetect.hpp"
#include "opencv2/calib3d.hpp"
#include "opencv2/imgcodecs.hpp"
#include "opencv2/videoio.hpp"
#include "opencv2/highgui.hpp"
#include "opencv2/imgproc.hpp"
#include "opencv2/features2d.hpp"
#include "opencv2/core/utility.hpp"
#include "opencv2/videoio/videoio_c.h"
#include "opencv2/highgui/highgui_c.h"
#else
#include <cv.h>
#endif

#include <cstdio>

using namespace std;
using namespace cv;

extern "C" {

#include "wrappers.h"

void * initCascade(char * path) {

        CascadeClassifier * cascade = new CascadeClassifier;
        if(!cascade->load(path))
        { printf("error loading cascade!!\n");
            exit(1);
        }
        return cascade;
}


void cascadeDetect(CascadeClassifier * cascade,
    GIMS(char,t),
    int fmax, int* fn, TRect* res) {

    IPL(t,8,1)

    Mat frame;
    frame = cvarrToMat(ipl_t);

    //equalizeHist( frame, frame);

    vector<Rect> faces;

    cascade->detectMultiScale( frame, faces,
    1.1, 2, 0
    |fmax>1?0:CV_HAAR_FIND_BIGGEST_OBJECT
    //|CV_HAAR_DO_ROUGH_SEARCH
    //|CV_HAAR_SCALE_IMAGE
    ,
    Size(30, 30) );

    int i = 0;

    *fn = MIN(fmax,faces.size());

    for(i=0; i< *fn; i++)
    { res[i].c =faces[i].x;
      res[i].r =faces[i].y;
      res[i].w =faces[i].width;
      res[i].h =faces[i].height;
    }

    cvReleaseImageHeader(&ipl_t);
}

////////////////////////////////////////////////////////////////////////////////

int cPNP(int rk, int ck, double*pk,
         int rv, int cv, double*pv,
         int rp, int cp, double*pp,
         int rr, int cr, double*pr) {

    cv::Mat cameraMatrix( 3,3,CV_64F,pk);
    cv::Mat  imagePoints(rv,2,CV_64F,pv);
    cv::Mat objectPoints(rp,3,CV_64F,pp);

    cv::Mat distCoeffs(4,1,cv::DataType<double>::type);
    distCoeffs.at<double>(0) = 0;
    distCoeffs.at<double>(1) = 0;
    distCoeffs.at<double>(2) = 0;
    distCoeffs.at<double>(3) = 0;

    cv::Mat rvec(3,1,cv::DataType<double>::type);
    cv::Mat tvec(3,1,cv::DataType<double>::type);

    cv::solvePnP(objectPoints, imagePoints, cameraMatrix, distCoeffs, rvec, tvec);

    int r,c;
    for (r=0; r<2; r++) {
        for (c=0; c<3; c++) {
            pr[r*cr+c] = r==0? rvec.at<double>(c): tvec.at<double>(c);
        }
    }
    return 0;
}

int cFindHomography(int code, double th,
    int rv, int cv, double*pv,
    int rp, int cp, double*pp,
    int rr, int cr, double*pr,
    int nmask, unsigned char *pmask) {

    cv::Mat  imagePoints(rv,2,CV_64F,pv);
    cv::Mat objectPoints(rp,2,CV_64F,pp);

    cv::Mat h(3,3,CV_64F,pr);
    cv::Mat mask(nmask,1,CV_8U,pmask);

    int method;
    switch(code) {
        case  1: method = CV_RANSAC; break;
        case  2: method = CV_LMEDS;  break;
        default: method = 0;
    }

    h = cv::findHomography(objectPoints,imagePoints,method,th,mask);

    int r,c;
    for (r=0; r<3; r++) {
        for (c=0; c<3; c++) {
            pr[r*3+c] = h.at<double>(r,c);
        }
    }

    return 0;
}

////////////////////////////////////////////////////////////////////////////////

typedef struct { double x, y; } TPoint;

void surf( GIMS(char,t),
           int fmax, int* fn, TPoint* res) {

    double minHessian = 400;
    
#ifndef OPENCV3
    static SurfFeatureDetector detector( minHessian );
#endif
    
    IPL(t,8,1)

    Mat frame;
    frame = cvarrToMat(ipl_t);

    std::vector<KeyPoint> keypoints;

#ifndef OPENCV3
    detector.detect( frame, keypoints );
#endif

    int i = 0;

    *fn = MIN(fmax,keypoints.size());

    double h2 = theight/2;
    double w2 = twidth/2;

    for(i=0; i< *fn; i++)
    { res[i].x = (-keypoints[i].pt.x+w2)/w2;
      res[i].y = (-keypoints[i].pt.y+h2)/w2;
    }

    cvReleaseImageHeader(&ipl_t);
}

////////////////////////////////////////////////////////////////////////////////


}

