#define SRC void*,int
#define VSIZE double
#define DST void*,int,VSIZE
#define VPOINT double

int ippiSet_32f_C1R(float,DST);
int ippiSet_8u_C1R(unsigned char,DST);
int ippiSet_8u_C3R(unsigned char*,DST);
int ippiImageJaehne_32f_C1R(DST);
int ippiFilterGauss_32f_C1R(SRC,DST,int);
int ippiFilterLaplace_32f_C1R(SRC,DST,int);
int ippiFilterSobelVert_32f_C1R(SRC,DST);
int ippiFilterSobelHoriz_32f_C1R(SRC,DST);
int ippiCopy_32f_C1R(SRC,DST);
int ippiCopy_32f_C1MR(SRC,DST,SRC);
int ippiCopy_8u_C1R(SRC,DST);
int ippiCopy_8u_C3C1R(SRC,DST);
int ippiCopy_8u_C1C3R(SRC,DST);
int ippiCopy_8u_C3R(SRC,DST);
int ippiScale_8u32f_C1R(SRC,DST,float,float);
int ippiScale_32f8u_C1R(SRC,DST,float,float);
int ippiAbs_32f_C1R(SRC,DST);
int ippiAdd_32f_C1R(SRC,SRC,DST);
int ippiSub_32f_C1R(SRC,SRC,DST);
int ippiSub_8u_C1RSfs(SRC,SRC,DST,int);
int ippiMul_32f_C1R(SRC,SRC,DST);
int ippiAbsDiff_8u_C1R(SRC,SRC,DST);
int ippiAbsDiff_32f_C1R(SRC,SRC,DST);
int ippiSum_8u_C1R(DST,double*);
int ippiSum_32f_C1R(DST,double*);
int ippiFilterMax_32f_C1R(SRC,DST,double,double);
int ippiCompare_32f_C1R(SRC,SRC,DST,int);
int ippiThreshold_Val_32f_C1R(SRC,DST,float,float,int);
int ippiThreshold_Val_8u_C1R(SRC,DST,unsigned char,unsigned char,int);
int ippiSqrt_32f_C1R(SRC,DST);
int ippiMinMax_32f_C1R(DST,float*,float*);
int ippiMaxIndx_32f_C1R(DST,float*,int*,int*);
int ippiMaxIndx_8u_C1R(DST,unsigned char*,int*,int*);
int ippiMulC_32f_C1R(SRC,float,DST);
int ippiRGBToGray_8u_C3C1R(SRC,DST);
int ippiYUV420ToRGB_8u_P3C3R(void*,int*,DST);
int ippiRGBToYUV420_8u_C3P3R(void*,int,void*,int*,VSIZE);
int ippiYUV420ToRGB_8u_P3R(void*,int*,DST);
int ippiRGBToHSV_8u_C3R(SRC,DST);
int ippiHSVToRGB_8u_C3R(SRC,DST);
int ippiIntegral_8u32f_C1R(SRC,DST,float);
int ippiCannyGetSize(VSIZE,int*);
int ippiCanny_32f8u_C1R(SRC,SRC,DST,float,float,void*);
int ippiFilterMedian_8u_C1R(SRC,DST,VSIZE,VSIZE);
int ippiFilterHipass_8u_C1R(SRC,DST,int);
int ippiHistogramRange_8u_C1R(DST,int*, int*, int);

int ippiDilate3x3_8u_C1R(SRC,DST);
int ippiErode3x3_8u_C1R(SRC,DST);
int ippiDilate_8u_C1R(SRC,DST,char*,VSIZE,VSIZE);
int ippiErode_8u_C1R(SRC,DST,char*,VSIZE,VSIZE);
int ippiNot_8u_C1R(SRC,DST);
int ippiAnd_8u_C1R(SRC,SRC,DST);
int ippiOr_8u_C1R(SRC,SRC,DST);

int auxWarpPerspective_32f_C1R(void * pSrc, int sstep, int sh, int sw,
                               int sr1, int sr2, int sc1, int sc2,
                               void * pDst, int dstep,
                               int dr1, int dr2, int dc1, int dc2,
                               const double *h, int interp);

int auxWarpPerspective_8u_C1R(void * pSrc, int sstep, int sh, int sw,
                               int sr1, int sr2, int sc1, int sc2,
                               void * pDst, int dstep,
                               int dr1, int dr2, int dc1, int dc2,
                               const double *h, int interp);

int auxWarpPerspective_8u_C3R(void * pSrc, int sstep, int sh, int sw,
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

int auxResize_8u_C1R(void * pSrc, int sstep, int sh, int sw,
                      int sr1, int sr2, int sc1, int sc2,
                      void * pDst, int dstep,
                      int dr1, int dr2, int dc1, int dc2,
                      int interp);

int auxResize_8u_C3R(void * pSrc, int sstep, int sh, int sw,
                      int sr1, int sr2, int sc1, int sc2,
                      void * pDst, int dstep,
                      int dr1, int dr2, int dc1, int dc2,
                      int interp);

int auxDCTFwd_32f_C1R(float * pSrc, int sstep,
                      int sr1, int sr2, int sc1, int sc2,
                      float * pDst, int dstep,
                      int dr1, int dr2, int dc1, int dc2);

int auxDCTInv_32f_C1R(float * pSrc, int sstep,
                      int sr1, int sr2, int sc1, int sc2,
                      float * pDst, int dstep,
                      int dr1, int dr2, int dc1, int dc2);

int ippiFFTInitAlloc_R_32f(void** st, int orderx, int ordery, int flat, int alg);
int ippiFFTFree_R_32f(void* st);
int ippiFFTGetBufSize_R_32f(void* st, int* size);
int ippiFFTFwd_RToPack_32f_C1R(void*,int,void*,int,void*st,void*buf);
int ippiMagnitudePack_32f_C1R(SRC,DST);

int ippiDistanceTransform_3x3_8u32f_C1R(SRC, DST, float* pMetrics);
int ippiDistanceTransform_5x5_8u32f_C1R(SRC, DST, float* pMetrics);

int ippiFloodFillGetSize(VSIZE, int*);
int ippiFloodFill_8Con_8u_C1IR(DST, VPOINT, unsigned char, void*, void*);
int ippiFloodFillGetSize_Grad(VSIZE, int*);
int ippiFloodFill_Grad8Con_8u_C1IR(DST, VPOINT, unsigned char, unsigned char, unsigned char, void*, void*);

int lbp8u(int delta, unsigned char * pSrc, int sstep, int sr1, int sr2, int sc1, int sc2, int* histogram);
