#define SRC void*,int
#define VSIZE double
#define DST void*,int,VSIZE

int ippiSet_32f_C1R(float,DST);
int ippiImageJaehne_32f_C1R(DST);
int ippiFilterGauss_32f_C1R(SRC,DST,int);
int ippiFilterSobelVert_32f_C1R(SRC,DST);
int ippiFilterSobelHoriz_32f_C1R(SRC,DST);
int ippiCopy_32f_C1R(SRC,DST);
int ippiCopy_32f_C1MR(SRC,DST,SRC);
int ippiCopy_8u_C1R(SRC,DST);
int ippiCopy_8u_C3R(SRC,DST);
int ippiScale_8u32f_C1R(SRC,DST,float,float);
int ippiScale_32f8u_C1R(SRC,DST,float,float);
int ippiAbs_32f_C1R(SRC,DST);
int ippiAdd_32f_C1R(SRC,SRC,DST);
int ippiSub_32f_C1R(SRC,SRC,DST);
int ippiMul_32f_C1R(SRC,SRC,DST);
int ippiFilterMax_32f_C1R(SRC,DST,double,double);
int ippiCompare_32f_C1R(SRC,SRC,DST,int);
int ippiThreshold_Val_32f_C1R(SRC,DST,float,float,int);
int ippiSqrt_32f_C1R(SRC,DST);
int ippiMinMax_32f_C1R(DST,float*,float*);
int ippiMaxIndx_32f_C1R(DST,float*,int*,int*);
int ippiMulC_32f_C1R(SRC,float,DST);
int ippiRGBToGray_8u_C3C1R(SRC,DST);
int ippiYUV420ToRGB_8u_P3C3R(void*,int*,DST);
int ippiIntegral_8u32f_C1R(SRC,DST,float);
int ippiCannyGetSize(VSIZE,int*);
int ippiCanny_32f8u_C1R(SRC,SRC,DST,float,float,void*);
int ippiFilterMedian_8u_C1R(SRC,DST,VSIZE,VSIZE);
int ippiHistogramRange_8u_C1R(DST,int*, int*, int);

int mycvOpenCamera(char *filename);
void mycvSetModeCamera(int camera,int mode,int rows,int cols);
char* mycvGetFrameCamera(int camera,int *step_bytes);

int auxWarpPerspective_32f_C1R(void * pSrc, int sstep, int sh, int sw,
                               int sr1, int sr2, int sc1, int sc2,
                               void * pDst, int dstep,
                               int dr1, int dr2, int dc1, int dc2,
                               const double *h, int interp);

const char* ippGetStatusString(int StsCode);

int getPoints32f(float * pSrc, int sstep, int sr1, int sr2, int sc1, int sc2,
                 int max, int* tot, int* hp);

int auxResize_32f_C1R(void * pSrc, int sstep, int sh, int sw,
                      int sr1, int sr2, int sc1, int sc2,
                      void * pDst, int dstep,
                      int dr1, int dr2, int dc1, int dc2,
                      int interp);
