//#define CV_NO_BACKWARD_COMPATIBILITY

//#include "cv.h"
//#include "highgui.h"


#include <cv.h>
#include <highgui.h>
#include <stdio.h>

void opencv_test() {
    printf("Hello from opencv in C!\n");
    
    IplImage* img = cvLoadImage("/home/brutus/Desktop/projects/opencvkk/lena.jpg",0);

//    IplImage* img = cvCreateImage(cvGetSize(img1),IPL_DEPTH_8U,1);

//    cvSmooth( img1, img, CV_GAUSSIAN, 5, 0,0,0);
    cvNamedWindow( "Example1", CV_WINDOW_AUTOSIZE );
    cvShowImage( "Example1", img );
    cvWaitKey(0);
    cvReleaseImage( &img );
    cvDestroyWindow( "Example1" );   
}

void testImage8u(void * pSrc,
            int height, int width, int sstep, 
            int sr1, int sr2, int sc1, int sc2) {

    printf("image from Haskell C!\n");

    printf("%d\n",(sr2-sr1+1)*(sc2-sc1+1));

    IplImage * img = cvCreateImageHeader(cvSize(640,480), 8, 1 );
    img->imageData = pSrc;

    cvSmooth( img, img, CV_GAUSSIAN, 5, 0,0,0);
    cvCanny( img, img, 10,100,3);

}

//////////////////////////////////////////////////////////////////////////


void houghTest(void * pSrc,
            int height, int width, int sstep, 
            int sr1, int sr2, int sc1, int sc2) {

    IplImage * src = cvCreateImageHeader(cvSize(640,480), 8, 1 );
    src->imageData = pSrc;

    IplImage* dst;
    IplImage* color_dst;
    CvMemStorage* storage = cvCreateMemStorage(0);
    CvSeq* lines = 0;
    int i;

    if( !src )
        return; // -1;

    //dst = cvCreateImage( cvGetSize(src), 8, 1 );
    //color_dst = cvCreateImage( cvGetSize(src), 8, 3 );

    cvCanny( src, src, 50, 200, 3 );
    //cvCvtColor( dst, color_dst, CV_GRAY2BGR );
#if 0
    lines = cvHoughLines2( dst, storage, CV_HOUGH_STANDARD, 1, CV_PI/180, 100, 0, 0 );

    for( i = 0; i < MIN(lines->total,100); i++ )
    {
        float* line = (float*)cvGetSeqElem(lines,i);
        float rho = line[0];
        float theta = line[1];
        CvPoint pt1, pt2;
        double a = cos(theta), b = sin(theta);
        double x0 = a*rho, y0 = b*rho;
        pt1.x = cvRound(x0 + 1000*(-b));
        pt1.y = cvRound(y0 + 1000*(a));
        pt2.x = cvRound(x0 - 1000*(-b));
        pt2.y = cvRound(y0 - 1000*(a));
        cvLine( color_dst, pt1, pt2, CV_RGB(255,0,0), 3, CV_AA, 0 );
    }
#else
    lines = cvHoughLines2( src, storage, CV_HOUGH_PROBABILISTIC, 1, CV_PI/180, 50, 50, 10 );
    for( i = 0; i < lines->total; i++ )
    {
        CvPoint* line = (CvPoint*)cvGetSeqElem(lines,i);
        cvLine( src, line[0], line[1], CV_RGB(128,128,128), 3, CV_AA, 0 );
    } 
#endif

    //cvCvtColor( color_dst, src, CV_BGR2GRAY );

    //cvNamedWindow( "Source", 1 );
    //cvShowImage( "Source", src );

    //cvNamedWindow( "Hough", 1 );
    //cvShowImage( "Hough", color_dst );

    //cvWaitKey(0);

    //return 0;
}

