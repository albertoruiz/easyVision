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

void hough(IMGSZ(t),int fmax, int* fn, TSegment* res) {

    IPL(t,1)

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

