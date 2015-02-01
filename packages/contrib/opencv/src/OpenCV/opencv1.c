#define CV_NO_BACKWARD_COMPATIBILITY

#include <cv.h>
#include <stdio.h>
#include "wrappers.h"

void opencv_canny(IMGSZ(s),IMGSZ(d)) {

    IPL(s,1)
    IPL(d,1)

    cvSmooth( ipl_s , ipl_d, CV_GAUSSIAN, 5, 0,0,0);
    cvCanny( ipl_d, ipl_d, 10,100,3);

    cvReleaseImageHeader(&ipl_s);
    cvReleaseImageHeader(&ipl_d);
}

////////////////////////////////////////////////////////////////////////////////



void opencv_warp8u(int g, int r, int c, double*p, IMGSZ(s),IMGSZ(d)) {
#define AT(i,j) (p[(i)*c+(j)]) 

    IPL(s,1)
    IPL(d,1)

    CvMat* h = cvCreateMat(3, 3, CV_32F);
    cvSetReal2D(h, 0, 0, AT(0,0));
    cvSetReal2D(h, 0, 1, AT(0,1));
    cvSetReal2D(h, 0, 2, AT(0,2));
    cvSetReal2D(h, 1, 0, AT(1,0));
    cvSetReal2D(h, 1, 1, AT(1,1));
    cvSetReal2D(h, 1, 2, AT(1,2));
    cvSetReal2D(h, 2, 0, AT(2,0));
    cvSetReal2D(h, 2, 1, AT(2,1));
    cvSetReal2D(h, 2, 2, AT(2,2));

    cvWarpPerspective(ipl_s, ipl_d, h, CV_INTER_LINEAR+CV_WARP_FILL_OUTLIERS, cvScalarAll(g) );

    cvReleaseImageHeader(&ipl_s);
    cvReleaseImageHeader(&ipl_d);

#undef AT
}

//---------------------------------------------------

// FIXME in wrappers.h and update here
#undef IPL
#define IPL(X,S,C) IplImage * ipl_##X = cvCreateImageHeader(cvSize(X##width,X##height), S, C ); \
                 ipl_##X->imageData = X##p; \
                 ipl_##X->widthStep = X##step; \
                 cvSetImageROI(ipl_##X,cvRect(X##c1,X##r1,X##c2-X##c1+1,X##r2-X##r1+1));

void opencv_warp32f(float g, int r, int c, double*p, IMGSZ(s),IMGSZ(d)) {
#define AT(i,j) (p[(i)*c+(j)]) 

    IPL(s,32,1)
    IPL(d,32,1)

    CvMat* h = cvCreateMat(3, 3, CV_32F);
    cvSetReal2D(h, 0, 0, AT(0,0));
    cvSetReal2D(h, 0, 1, AT(0,1));
    cvSetReal2D(h, 0, 2, AT(0,2));
    cvSetReal2D(h, 1, 0, AT(1,0));
    cvSetReal2D(h, 1, 1, AT(1,1));
    cvSetReal2D(h, 1, 2, AT(1,2));
    cvSetReal2D(h, 2, 0, AT(2,0));
    cvSetReal2D(h, 2, 1, AT(2,1));
    cvSetReal2D(h, 2, 2, AT(2,2));

    cvWarpPerspective(ipl_s, ipl_d, h, CV_INTER_LINEAR+CV_WARP_FILL_OUTLIERS, cvScalarAll(g) );

    cvReleaseImageHeader(&ipl_s);
    cvReleaseImageHeader(&ipl_d);

#undef AT
}

//-----------------------------------------------

void opencv_warp8u3(float g, int r, int c, double*p, IMGSZ(s),IMGSZ(d)) {
#define AT(i,j) (p[(i)*c+(j)]) 

    IPL(s,8,3)
    IPL(d,8,3)

    CvMat* h = cvCreateMat(3, 3, CV_32F);
    cvSetReal2D(h, 0, 0, AT(0,0));
    cvSetReal2D(h, 0, 1, AT(0,1));
    cvSetReal2D(h, 0, 2, AT(0,2));
    cvSetReal2D(h, 1, 0, AT(1,0));
    cvSetReal2D(h, 1, 1, AT(1,1));
    cvSetReal2D(h, 1, 2, AT(1,2));
    cvSetReal2D(h, 2, 0, AT(2,0));
    cvSetReal2D(h, 2, 1, AT(2,1));
    cvSetReal2D(h, 2, 2, AT(2,2));

    cvWarpPerspective(ipl_s, ipl_d, h, CV_INTER_LINEAR+CV_WARP_FILL_OUTLIERS, cvScalarAll(g) );

    cvReleaseImageHeader(&ipl_s);
    cvReleaseImageHeader(&ipl_d);

#undef AT
}


////////////////////////////////////////////////////////////////////////////////

void hough(IMGSZ(t),int fmax, int* fn, TSegment* res) {

    IPL(t,8,1)

    CvMemStorage* storage = cvCreateMemStorage(0);
    CvSeq* lines = 0;

    lines = cvHoughLines2( ipl_t, storage, CV_HOUGH_PROBABILISTIC, 1, CV_PI/180, 50, 50, 10 );

    *fn = MIN(fmax,lines->total);

    double h2 = theight/2;
    double w2 = twidth/2;

    int i;
    for(i=0; i< *fn; i++) {
      CvPoint* line = (CvPoint*)cvGetSeqElem(lines,i);
      res[i].x1 = (-line[0].x + w2)/w2;
      res[i].y1 = (-line[0].y + h2)/w2;
      res[i].x2 = (-line[1].x + w2)/w2;
      res[i].y2 = (-line[1].y + h2)/w2;
    }

    cvReleaseImageHeader(&ipl_t);
    cvReleaseMemStorage(&storage);
}

