#define CV_NO_BACKWARD_COMPATIBILITY

#include <cv.h>
#include <stdio.h>
#include <ipp.h>

void testImage8u(void * pSrc,
            int height, int width, int sstep, 
            int sr1, int sr2, int sc1, int sc2) {

    printf("image from Haskell C!\n");

    printf("%d\n",(sr2-sr1+1)*(sc2-sc1+1));

    IplImage * img = cvCreateImageHeader(cvSize(width,height), 8, 1 );
    img->imageData = pSrc;
    cvSetImageROI(img,cvRect(sc1,sr1,sc2-sc1+1,sr2-sr1+1));

    cvSmooth( img, img, CV_GAUSSIAN, 5, 0,0,0);
    cvCanny( img, img, 10,100,3);

}

//////////////////////////////////////////////////////////////////////////


void hough(void * pSrc,
           int height, int width, int sstep,
           int sr1, int sr2, int sc1, int sc2,
           int fmax, int* fn, IppiRect* res) {

    IplImage * src = cvCreateImageHeader(cvSize(width,height), 8, 1 );
    src->imageData = pSrc;
    cvSetImageROI(src,cvRect(sc1,sr1,sc2-sc1+1,sr2-sr1+1));


    CvMemStorage* storage = cvCreateMemStorage(0);
    CvSeq* lines = 0;

    //cvSmooth( src, src, CV_GAUSSIAN, 5, 0,0,0);
    //cvCanny( src, src, 50, 200, 3 );

    lines = cvHoughLines2( src, storage, CV_HOUGH_PROBABILISTIC, 1, CV_PI/180, 50, 50, 10 );

    int i = 0;

//    for( i = 0; i < lines->total; i++ )
//    {
//        CvPoint* line = (CvPoint*)cvGetSeqElem(lines,i);
//        cvLine( src, line[0], line[1], CV_RGB(128,128,128), 3, CV_AA, 0 );
//    }

    *fn = MIN(fmax,lines->total);

    //printf("%d\n",*fn);

    for(i=0; i< *fn; i++)
    { CvPoint* line = (CvPoint*)cvGetSeqElem(lines,i);
      res[i].x =line[0].x;
      res[i].y =line[0].y;
      res[i].width =line[1].x;
      res[i].height =line[1].y;
    }

    cvReleaseImageHeader(&src);
    cvReleaseMemStorage(&storage);
}

