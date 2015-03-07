#include<stdlib.h>
#include<stdio.h>
#include<math.h>

int getPoints32f(float * pSrc, int sstep, int sr1, int sr2, int sc1, int sc2,
                 int max, int* tot, int* hp) {
    int r,c;
    int pos = 0;
    int maxpos = 2*max;
    for (r=sr1; r<=sr2; r++) {
        for(c=sc1; c<=sc2; c++) {
            if(pos>=maxpos) {
                *tot = max*2;
                return 0;
            }
            if(*(pSrc+r*sstep/4+c) > 0.) {
                hp[pos] =  r;
                hp[pos+1] = c;
                pos+=2;
            }
        }
    }
    *tot = pos;
    return 0;
}

//---------------------------- Local binary patterns ---------------------

// fills a buffer int[255]

int lbp8u(int delta, unsigned char * pSrc, int sstep, int sr1, int sr2, int sc1, int sc2, int* histogram) {
    #define X(r,c) (*(pSrc+(r)*sstep+(c)))
    int r,c;
    int k;
    for(k=0; k<256; k++) {
        histogram[k]=0;
    }
    for (r=sr1+1; r<sr2; r++) {
        for(c=sc1+1; c<sc2; c++) {
            int x = X(r,c)+delta;
            int x0 = X(r-1,c-1) > x ? 1 : 0;
            int x1 = X(r-1,c)   > x ? 2 : 0;
            int x2 = X(r-1,c+1) > x ? 4 : 0;
            int x3 = X(r,c-1)   > x ? 8 : 0;
            int x4 = X(r,c+1)   > x ? 16: 0;
            int x5 = X(r+1,c-1) > x ? 32: 0;
            int x6 = X(r+1,c)   > x ? 64: 0;
            int x7 = X(r+1,c+1) > x ? 128:0;
            int val = x0+x1+x2+x3+x4+x5+x6+x7;
            histogram[val]++;
        }
    }
    return 0;
    #undef X
}

//---------------------------- simple color code ---------------------

// this test version overwrites the hsv with prototype color

#define H(V,D) else if (*h<=V) { *h=(D); *v=255; *s=255; }

int hsvcodeTest(int kb, int kg, int kw,
                unsigned char * pSrc, int sstep, int sr1, int sr2, int sc1, int sc2) {
    int r,c;
    for (r=sr1; r<=sr2; r++) {
        for(c=sc1; c<=sc2; c++) {
            unsigned char* h = pSrc + r*sstep + c*3;
            unsigned char* s = h+1;
            unsigned char* v = s+1;
            if(*v<kb) {                        // black
                *v = 0;
            } else if ((*s<kg) && (*v<kw)) {   // gray
                *v = 128;
                *s = 0;
            } else if ((*s<kg)) {              // white
                *v = 255;
                *s = 0;
            }
             H(12,0)     // red
             H(26,18)    // orange
             H(46,36)    // yellow
             H(116,80)   // green
             H(135,125)  // cyan ??
             H(200,168)  // blue
             H(230,215)  // magenta ??
             else { *h=0; *v=255; *s=255; }    // red
        }
    }
    return 0;
}

#undef H

// this version creates color codes

#define H(V,D) else if (*h<=V) { *d=(D); }

int hsvcode(int kb, int kg, int kw,
                unsigned char * pSrc, int sstep,
                unsigned char * pDst, int dstep,
                int sr1, int sr2, int sc1, int sc2) {
    int r,c;
    for (r=sr1; r<=sr2; r++) {
        for(c=sc1; c<=sc2; c++) {
            unsigned char* h = pSrc + r*sstep + c*3;
            unsigned char* s = h+1;
            unsigned char* v = s+1;
            unsigned char* d = pDst + r*dstep + c;
            if(*v<kb) {                        // black
                *d = 1;
            } else if ((*s<kg) && (*v<kw)) {   // gray
                *d = 0;
            } else if ((*s<kg)) {              // white
                *d = 2;
            }
             H(12,3)    // red
             H(26,4)    // orange
             H(46,5)    // yellow
             H(116,6)   // green
             H(135,7)   // cyan ??
             H(200,8)   // blue
             H(230,9)   // magenta ??
             else { *d=3; }    // red
        }
    }
    return 0;
}

#undef H

//------------ local max in scale space ----------------

int localMaxScale3(float * pSrc1, int sstep1,
               float * pSrc2, int sstep2,
               float * pSrc3, int sstep3,
               int sr1, int sr2, int sc1, int sc2,
               int max, int* tot, float thres, int* hp) {
#define X1(r,c) (*(pSrc1+(r)*sstep1/4+(c)))
#define X2(r,c) (*(pSrc2+(r)*sstep2/4+(c)))
#define X3(r,c) (*(pSrc3+(r)*sstep3/4+(c)))
    int r,c;
    int pos = 0;
    int maxpos = 2*max;
    for (r=sr1+1; r<sr2; r++) {
        for(c=sc1+1; c<sc2; c++) {
            if(pos>=maxpos) {
                *tot = maxpos;
                return 1;
            }
            //printf("%d %d \n",r,c);
            float x = X2(r,c);
            if( x > thres
             && x > X2(r-1,c-1)
             && x > X2(r-1,c)
             && x > X2(r-1,c+1)
             && x > X2(r,c-1)
             && x > X2(r,c+1)
             && x > X2(r+1,c-1)
             && x > X2(r+1,c)
             && x > X2(r+1,c+1)
             && x > X1(r-1,c-1)
             && x > X1(r-1,c)
             && x > X1(r-1,c+1)
             && x > X1(r,c-1)
             && x > X1(r,c)
             && x > X1(r,c+1)
             && x > X1(r+1,c-1)
             && x > X1(r+1,c)
             && x > X1(r+1,c+1)
             && x > X3(r-1,c-1)
             && x > X3(r-1,c)
             && x > X3(r-1,c+1)
             && x > X3(r,c-1)
             && x > X3(r,c)
             && x > X3(r,c+1)
             && x > X3(r+1,c-1)
             && x > X3(r+1,c)
             && x > X3(r+1,c+1)
             ||
                x < -thres
             && x < X2(r-1,c-1)
             && x < X2(r-1,c)
             && x < X2(r-1,c+1)
             && x < X2(r,c-1)
             && x < X2(r,c+1)
             && x < X2(r+1,c-1)
             && x < X2(r+1,c)
             && x < X2(r+1,c+1)
             && x < X1(r-1,c-1)
             && x < X1(r-1,c)
             && x < X1(r-1,c+1)
             && x < X1(r,c-1)
             && x < X1(r,c)
             && x < X1(r,c+1)
             && x < X1(r+1,c-1)
             && x < X1(r+1,c)
             && x < X1(r+1,c+1)
             && x < X3(r-1,c-1)
             && x < X3(r-1,c)
             && x < X3(r-1,c+1)
             && x < X3(r,c-1)
             && x < X3(r,c)
             && x < X3(r,c+1)
             && x < X3(r+1,c-1)
             && x < X3(r+1,c)
             && x < X3(r+1,c+1)
)

             {
                hp[pos] =  r;
                hp[pos+1] = c;
                pos+=2;
            }
        }
    }
    *tot = pos;
    return 0;
}
#undef X1
#undef X2
#undef X3

//------------ local max in scale space ----------------

int localMaxScale3Simplified
              (float * pSrc1, int sstep1,
               float * pSrc2, int sstep2,
               float * pSrc3, int sstep3,
               int sr1, int sr2, int sc1, int sc2,
               int max, int* tot, float thres, int* hp) {
#define X1(r,c) (*(pSrc1+(r)*sstep1/4+(c)))
#define X2(r,c) (*(pSrc2+(r)*sstep2/4+(c)))
#define X3(r,c) (*(pSrc3+(r)*sstep3/4+(c)))
    int r,c;
    int pos = 0;
    int maxpos = 2*max;
    for (r=sr1+1; r<sr2; r++) {
        for(c=sc1+1; c<sc2; c++) {
            if(pos>=maxpos) {
                *tot = maxpos;
                return 1;
            }
            //printf("%d %d \n",r,c);
            float x = X2(r,c);
            if( x > thres && x > X1(r,c) && x > X3(r,c) )
             {
                hp[pos] =  r;
                hp[pos+1] = c;
                pos+=2;
            }
        }
    }
    *tot = pos;
    return 0;
}
#undef X1
#undef X2
#undef X3


// for benchmarks
#define X(r,c) (*(pSrc+(r)*sstep/4+(c)))
double csum32f(float * pSrc, int sstep, int sr1, int sr2, int sc1, int sc2) {
    int r,c;
    double sum = 0;
    for (r=sr1; r<=sr2; r++)
        for(c=sc1; c<=sc2; c++)
            sum+=X(r,c);
    return sum;
}
#undef X

//---------------------------- histogram of local gradient orientations  ---------------------

inline
int getBin(int n, float a) {
 return (int)(.5 + n*(M_PI+a)/2/M_PI) % n;
// round ((fromIntegral n)*(pi + a)/2/pi) `rem` n
}

#define GETBIN(n,a) ((int)(.5 + (n)*(M_PI+(a))/2/M_PI) % (n))
#define GAUSS(sigma,rc,cc,r,c) exp( 0.5/sigma/sigma*((r-rc)*(r-rc)+(c-cc)*(c-cc)))

int histodir(float * pSrc1, int sstep1,
             float * pSrc2, int sstep2,
             float * pSrc3, int sstep3,
             int sr1, int sr2, int sc1, int sc2,
             double sigma, int rm, int cm,
             int n, double* histogram) {
    #define M(r,c) (*(pSrc1+(r)*sstep1/4+(c)))
    #define X(r,c) (*(pSrc2+(r)*sstep2/4+(c)))
    #define Y(r,c) (*(pSrc3+(r)*sstep3/4+(c)))
    int r,c;
    int k;
    for(k=0; k<n; k++) {
        histogram[k]=0;
    }
    for (r=sr1; r<=sr2; r++) {
        for(c=sc1; c<=sc2; c++) {
            int    bin = GETBIN(n, atan2 (Y(r,c),X(r,c)));
            double val = M(r,c)*GAUSS(sigma,rm,cm,r,c);
            histogram[bin]+=val;
        }
    }
    return 0;
    #undef M
    #undef X
    #undef Y
}

//---------------------------- 2D histogram --------------------------

int histogram2D (unsigned char * pSrc1, int sstep1,
                 unsigned char * pSrc2, int sstep2,
                 float * pDst , int sstepD,
                 int sr1, int sr2, int sc1, int sc2) {
    #define X(r,c) (*(pSrc1+(r)*sstep1/1+(c)))
    #define Y(r,c) (*(pSrc2+(r)*sstep2/1+(c)))
    #define Z(r,c) (pDst +(r)*sstepD/4+(c))
    int r,c;
    for (r=sr1; r<=sr2; r++) {
        for(c=sc1; c<=sc2; c++) {
            float* p = Z(Y(r,c),X(r,c));
            (*p)++;
        }
    }
    return 0;
    #undef X
    #undef Y
    #undef Z
}

int lookup2D (unsigned char * pSrc1, int sstep1,
                       unsigned char * pSrc2, int sstep2,
                       float         * pSrc3, int sstep3,
                       float         * pDst , int sstepD,
                       int sr1, int sr2, int sc1, int sc2) {
    #define X(r,c) (*(pSrc1+(r)*sstep1/1+(c)))
    #define Y(r,c) (*(pSrc2+(r)*sstep2/1+(c)))
    #define H(r,c) (*(pSrc3+(r)*sstep3/4+(c)))
    #define Z(r,c) (*(pDst +(r)*sstepD/4+(c)))
    int r,c;
    for (r=sr1; r<=sr2; r++) {
        for(c=sc1; c<=sc2; c++) {
            Z(r,c) = H(Y(r,c),X(r,c));
        }
    }
    return 0;
    #undef X
    #undef Y
    #undef H
    #undef Z
}


