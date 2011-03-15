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

//////////////////////////////////////////////////////////////////////


void testMSER(int delta,
              int minArea,
              int maxArea,
              double maxVariation,
              double minDiversity,
              int maxEvolution,
              double areaThreshold,
              double minMargin,
              int edgeBlurSize,
              void * pSrc,
              int height, int width, int sstep, 
              int sr1, int sr2, int sc1, int sc2) {

    //printf("testing MSER!\n");
    
    //printf("%d %d %d %d %d\n",sr1,sr2,sc1,sc2,(sr2-sr1+1)*(sc2-sc1+1));
    
    IplImage * img = cvCreateImageHeader(cvSize(width,height), 8, 1 );
    img->imageData = (char*)pSrc;
    cvSetImageROI(img,cvRect(sc1,sr1,sc2-sc1+1,sr2-sr1+1));

    CvSeq* contours;
    CvMemStorage* storage= cvCreateMemStorage();
    

	CvMSERParams params = // cvMSERParams();
	   cvMSERParams(delta, minArea, maxArea, maxVariation, minDiversity);
	                //maxEvolution, areaThreshold, minMargin, edgeBlurSize );

    //printf("%d\n",delta);
    //printf("%d\n",edgeBlurSize);
	double t = (double)cvGetTickCount();
	cvExtractMSER( img, NULL, &contours, storage, params );
	t = cvGetTickCount() - t;
    
	//printf( "MSER extracted %d contours in %g ms.\n", contours->total, t/((double)cvGetTickFrequency()*1000.) );
    
    
    uchar* rsptr = (uchar*)img->imageData;
    cvSet(img, cvScalar(255));
    
    //printf("clear\n");
    
	// draw mser with different color
	for ( int i = contours->total-1; i >= 0; i-- )
	{
		CvSeq* r = *(CvSeq**)cvGetSeqElem( contours, i );
		//printf("l: %d\n",r->total);
		for ( int j = 0; j < r->total; j++ )
		{
			CvPoint* pt = CV_GET_SEQ_ELEM( CvPoint, r, j );
			//printf("%d %d\n",pt->x,pt->y);
			int idx = pt->x+pt->y*img->widthStep;
			rsptr[idx] = 255-rsptr[idx];
		}
	}
    //printf("mark\n");
    cvReleaseMemStorage(&storage);
    cvReleaseImageHeader(&img);
    //printf("release\n");
}


}

