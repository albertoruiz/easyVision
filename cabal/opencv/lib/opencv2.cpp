#define CV_NO_BACKWARD_COMPATIBILITY

#include "cv.h"

#include <cstdio>

#include <ipp.h>

using namespace std;
using namespace cv;

extern "C" {

void * initCascade(char * path) {

        CascadeClassifier * cascade = new CascadeClassifier;
        if(!cascade->load(path))
        { printf("error loading cascade!!\n");
            exit(1);
        }
        return cascade;
}


void cascadeDetect(CascadeClassifier * cascade,
    void * pSrc, int height, int width, int sstep,
    int sr1, int sr2, int sc1, int sc2,
    int fmax, int* fn, IppiRect* res) {

    IplImage * src = cvCreateImageHeader(cvSize(width,height), 8, 1 );
    src->imageData = (char*)pSrc;
    cvSetImageROI(src,cvRect(sc1,sr1,sc2-sc1+1,sr2-sr1+1));

    //printf("%d\n", src->widthStep);

    Mat frame;
    frame = src;

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
    { res[i].x =faces[i].x;
      res[i].y =faces[i].y;
      res[i].width =faces[i].width;
      res[i].height =faces[i].height;
    }

    cvReleaseImageHeader(&src);
}

}
