const char* ippGetStatusString(int StsCode);

int ippSetNumThreads(int numThreads);

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
