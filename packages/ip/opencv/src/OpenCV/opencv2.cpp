#ifdef OPENCV24
#include "opencv2/nonfree/features2d.hpp"
#endif

#ifdef OPENCV3
#include "opencv2/opencv.hpp"
#include "opencv2/xfeatures2d.hpp"
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

    cv::Mat h(rr,cr,CV_64F,pr);
    cv::Mat mask(nmask,1,CV_8U,pmask);
    cv::Scalar zero = 0;

    int method;
    switch(code) {
        case  1: method = CV_RANSAC; break;
        case  2: method = CV_LMEDS;  break;
        default: method = 0;
    }

    mask=zero;

    if (code==3) {
        h = cv::estimateRigidTransform(objectPoints,imagePoints,false);
    } else {
        h = cv::findHomography(objectPoints,imagePoints,method,th,mask);
        if (countNonZero(mask) < 4) return 0;
    }

    int r,c;
    for (r=0; r<rr; r++) {
        for (c=0; c<cr; c++) {
            pr[r*cr+c] = h.at<double>(r,c);
        }
    }

    return 0;
}

////////////////////////////////////////////////////////////////////////////////

int handleError(int status, const char* func_name,
                const char* err_msg, const char* file_name,
                int line, void* userdata ) {
    //Do nothing -- will suppress console output
    return 0;   //Return value is not used
}

////////////////////////////////////////////////////////////////////////////////


#define ATM(m,c,i,j) (m[(i)*c+(j)])
#define COPYM(DST,SRC,R,C) { int r, c; for (r=0; r<R; r++) for (c=0; c<C; c++) cvSetReal2D(DST,r,c, ATM(SRC,C,r,c)); }

double cFindTransformECC(int code, int maxCount, double epsilon,
                         GIMS(char,s), GIMS(char,d),
                         int r1, int c1, double * h1,
                         int r2, int c2, double* h2)
{
    IPL(s,8,1)
    IPL(d,8,1)

    CvMat* h = cvCreateMat(r1, c1, CV_32F);
    COPYM(h,h1,r1,c1);
    Mat hm = cvarrToMat(h);

    Mat sframe;
    sframe = cvarrToMat(ipl_s);
    Mat dframe;
    dframe = cvarrToMat(ipl_d);

    double cc = 0;

#ifdef OPENCV3

    int method;
    switch(code) {
        case  0: method = MOTION_TRANSLATION; break;
        case  1: method = MOTION_EUCLIDEAN;  break;
        case  2: method = MOTION_AFFINE; break;
        default: method = MOTION_HOMOGRAPHY;
    }

    cv::redirectError(handleError);
    try {
      cc = cv::findTransformECC(sframe, dframe, hm, method,TermCriteria (TermCriteria::COUNT+TermCriteria::EPS,maxCount,epsilon));
    }
    catch(...) {
       cc = 0;
    }
    cv::redirectError(NULL);

#else
    printf("sorry, findTransformECC requires OpenCV 3.0\n");
    exit(0);
#endif


    int r,c;
    for (r=0; r<r2; r++) {
        for (c=0; c<c2; c++) {
            h2[r*c2+c] = hm.at<float>(r,c);
        }
    }

    cvReleaseImageHeader(&ipl_s);
    cvReleaseImageHeader(&ipl_d);

    return cc;
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

#define DYN_VECT(type,name) int* n##name, type** pp##name
#define CREATE_DYN_VECT(type,name,size) *n##name = (size); type *p##name = (type*)malloc(*n##name * sizeof(type)); *pp##name = p##name;
#define FILL_DYN_VECT(name,exprk) for(int k=0; k<*n##name; k++) {p##name[k] = (exprk);}

/*
int c_vVector (int m, int* n, double** pp) {
    *n = 2*m;
    double* p = (double*)malloc(*n * sizeof(double));
    *pp = p;
    for(int j=0; j<*n; j++) {
        p[j] = j;
    }
    return 0;
}
*/

int c_vVector (int m, DYN_VECT(double,res)) {
    CREATE_DYN_VECT(double,res,2*m)
    FILL_DYN_VECT(res,2*k)
    return 0;
}

////////////////////////////////////////////////////////////////////////////////

int c_sift( int maxk, double ct, double et, GIMS(char,t), DYN_VECT(double,locs), DYN_VECT(float,descs)) {

#ifndef OPENCV3
    CREATE_DYN_VECT(double,locs,4*0)
    CREATE_DYN_VECT(float,descs,128*0)
    return 0;
#else

    IPL(t,8,1)
    Mat frame;
    frame = cvarrToMat(ipl_t);
    std::vector<KeyPoint> keypoints;
    cv::Ptr<Feature2D> f2d = xfeatures2d::SIFT::create(maxk,3,ct,et);
    Mat features;

    f2d->detectAndCompute( frame, noArray(), keypoints, features );
    int tot = keypoints.size();
    CREATE_DYN_VECT(double,locs,4*tot)

    double h2 = theight/2;
    double w2 = twidth/2;

    for(int i=0; i< tot; i++) {
        plocs[4*i+0] = (-keypoints[i].pt.x+w2)/w2;
        plocs[4*i+1] = (-keypoints[i].pt.y+h2)/w2;
        plocs[4*i+2] = (keypoints[i].size)/w2;
        plocs[4*i+3] = keypoints[i].angle;
    }
    
    //cout << features.type() << endl;
    //cout << features.row(0) << endl;

    CREATE_DYN_VECT(float,descs,128*tot)
    for(int i=0; i< tot; i++) {
        for(int j=0; j<128; j++) {
            pdescs[128*i+j] = features.at<float>(i,j);
        }
    }
    
    cvReleaseImageHeader(&ipl_t);
    return 0;

#endif
}

////////////////////////////////////////////////////////////////////////////////

int c_match(int code, double th,
    int rv, int cv, float*pv,
    int rp, int cp, float*pp,
    int nr, int* pr) {

    cv::Mat mv(rv,cv,CV_32F,pv);
    cv::Mat mp(rp,cp,CV_32F,pp);

    BFMatcher matcher(NORM_L2, true);
    std::vector< DMatch > matches;

    int method;
    switch(code) {
        default: method = 0;
    }

    matcher.match( mv, mp, matches);

//---------------------------------------------------------------
    
    double max_dist = 0; double min_dist = 10000;

    //-- Quick calculation of max and min distances between keypoints
    for( int i = 0; i < matches.size(); i++ ) {
        double dist = matches[i].distance;
        if( dist < min_dist ) min_dist = dist;
        if( dist > max_dist ) max_dist = dist;
    }

    // printf("-- Max dist : %f \n", max_dist );
    // printf("-- Min dist : %f \n", min_dist );

    std::vector< DMatch > good_matches;

    for( int i = 0; i < matches.size(); i++ ) {
        // if( matches[i].distance <= max(2*min_dist, 0.02))
        if(matches[i].distance <= th)
            good_matches.push_back( matches[i]);
    }

    // cout << th << " " << rv << " " << rp << " " << matches.size() << " " << good_matches.size() << endl;



    for (int k=0; k<nr; k++) {
        pr[k] = -1;
    }

    for (int k=0; k<good_matches.size(); k++) {
        //cout << k << " " << good_matches[k].queryIdx << " " << good_matches[k].trainIdx << endl;
        pr[good_matches[k].queryIdx] = good_matches[k].trainIdx;
    }

    return 0;
}

////////////////////////////////////////////////////////////////////////////////

#define COPYMat(TY,DST,SRC,R,C) { int r, c; for (r=0; r<R; r++) for (c=0; c<C; c++) DST.at<TY>(r,c)=ATM(SRC,C,r,c); }

int opencv_warp8u3_bis(int fill, unsigned char g, int r, int c, double*p, GIMS(char,s), GIMS(char,d)) {

    IPL(s,8,3)
    IPL(d,8,3)

    Mat ms; ms = cvarrToMat(ipl_s);
    Mat md; md = cvarrToMat(ipl_d);

    Mat h(3, 3, CV_32F);
    COPYMat(float,h,p,3,3);

    cout << "hi" << endl;

    cv::warpPerspective(ms,md,h,md.size(),cv::INTER_LANCZOS4,cv::BORDER_TRANSPARENT);

    cvReleaseImageHeader(&ipl_s);
    cvReleaseImageHeader(&ipl_d);
    
    return 0;

}


}

