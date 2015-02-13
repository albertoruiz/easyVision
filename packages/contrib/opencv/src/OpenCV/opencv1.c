#define CV_NO_BACKWARD_COMPATIBILITY

#include <cv.h>
#include <highgui.h>
#include <stdio.h>
#include <unistd.h>
#include "wrappers.h"

int opencv_canny(IMGSZ(s),IMGSZ(d)) {

    IPL(s,8,1)
    IPL(d,8,1)

    cvSmooth( ipl_s , ipl_d, CV_GAUSSIAN, 5, 0,0,0);
    cvCanny( ipl_d, ipl_d, 10,100,3);

    cvReleaseImageHeader(&ipl_s);
    cvReleaseImageHeader(&ipl_d);
    
    return 0;
}

////////////////////////////////////////////////////////////////////////////////

#define ATM(m,c,i,j) (p[(i)*c+(j)])
#define COPYM(DST,SRC,R,C) { int r, c; for (r=0; r<R; r++) for (c=0; c<C; c++) cvSetReal2D(DST,r,c, ATM(SRC,C,r,c)); }


int opencv_warp8u(unsigned char g, int r, int c, double*p, IMGSZ(s),IMGSZ(d)) {

    IPL(s,8,1)
    IPL(d,8,1)

    CvMat* h = cvCreateMat(3, 3, CV_32F);
    COPYM(h,p,3,3);

    cvWarpPerspective(ipl_s, ipl_d, h, CV_INTER_LINEAR+CV_WARP_FILL_OUTLIERS, cvScalarAll(g) );

    cvReleaseImageHeader(&ipl_s);
    cvReleaseImageHeader(&ipl_d);
    
    return 0;

}

//---------------------------------------------------


int opencv_warp32f(float g, int r, int c, double*p, IMGSZ(s),IMGSZ(d)) {

    IPL(s,32,1)
    IPL(d,32,1)

    CvMat* h = cvCreateMat(3, 3, CV_32F);
    COPYM(h,p,3,3);

    cvWarpPerspective(ipl_s, ipl_d, h, CV_INTER_LINEAR+CV_WARP_FILL_OUTLIERS, cvScalarAll(g) );

    cvReleaseImageHeader(&ipl_s);
    cvReleaseImageHeader(&ipl_d);
    
    return 0;

}

//-----------------------------------------------

int opencv_warp8u3(unsigned char g, int r, int c, double*p, IMGSZ(s),IMGSZ(d)) {

    IPL(s,8,3)
    IPL(d,8,3)

    CvMat* h = cvCreateMat(3, 3, CV_32F);
    COPYM(h,p,3,3);

    cvWarpPerspective(ipl_s, ipl_d, h, CV_INTER_LINEAR+CV_WARP_FILL_OUTLIERS, cvScalarAll(g) );

    cvReleaseImageHeader(&ipl_s);
    cvReleaseImageHeader(&ipl_d);
    
    return 0;

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

////////////////////////////////////////////////////////////////////////////////

void * openOCVC(int videodevice, int* width, int* height, int* fps){

    CvCapture* capture = cvCreateCameraCapture(videodevice);
    
    cvSetCaptureProperty(capture,CV_CAP_PROP_FRAME_WIDTH,*width);
    cvSetCaptureProperty(capture,CV_CAP_PROP_FRAME_HEIGHT,*height);
    
    *width = cvGetCaptureProperty(capture,CV_CAP_PROP_FRAME_WIDTH);
    *height = cvGetCaptureProperty(capture,CV_CAP_PROP_FRAME_HEIGHT);

    // magic to avoid strange v4l2 error "timer expired"
    usleep(5000000);
    cvQueryFrame(capture);
    return capture;
}

int grabOCVC(CvCapture * v, IMGSZ(d)) {
    
    IPL(d,8,3)
    IplImage* frame;
    
    frame = cvQueryFrame(v);
    cvCvtColor(frame,ipl_d,CV_BGR2RGB);
    
    return 0;
}

