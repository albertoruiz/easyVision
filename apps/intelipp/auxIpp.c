#include <ipp.h>

int auxWarpPerspective_32f_C1R(void * pSrc, int sstep, int sh, int sw,
                               int sr1, int sr2, int sc1, int sc2,
                               void * pDst, int dstep,
                               int dr1, int dr2, int dc1, int dc2,
                               const double *h, int interp)
{
    IppiSize srcSize = {sw,sh};
    IppiRect srcRoi = {sc1,sr1,sc2-sc1+1,sr2-sr1+1};
    IppiRect dstRoi = {dc1,dr1,dc2-dc1+1,dr2-dr1+1};
    const double coeff [3][3] = {{h[0],h[1],h[2]},
                                 {h[3],h[4],h[5]},
                                 {h[6],h[7],h[8]}};
    return ippiWarpPerspective_32f_C1R(pSrc,srcSize,sstep,srcRoi,
                                           pDst,dstep,dstRoi,coeff,interp);
    return 0;
}

void ippErrorMsg(int err) {
    printf("%s\n",ippGetStatusString(err)); 
    exit(err);
}
