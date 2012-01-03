#define IMG(X) unsigned char * X##pSrc, int X##sstep, int X##sr1, int X##sr2, int X##sc1, int X##sc2
#define IMF(X) float * X##pSrc, int X##sstep, int X##sr1, int X##sr2, int X##sc1, int X##sc2
#define P(X,r,c) (*(X##pSrc+(r)*X##sstep+(c)))
#define TRAV(X,D,r,c) for (r=X##sr1+D; r<=X##sr2-D; r++) for(c=X##sc1+D; c<=X##sc2-D; c++)
#define MAX(a,b) ((a)>(b)?(a):(b))
#define MIN(a,b) ((a)<(b)?(a):(b))

#include <stdio.h>
#include <math.h>

int prufun(IMG(src)) {
    int r,c;
    int k;
    int sum = 0;
    TRAV(src,0,r,c) {
        sum += P(src,r,c);
    }
    printf("%d\n",sum);
    return 0;
}

int customSum(IMG(src), int*result) {
    int r,c;
    int sum = 0;
    TRAV(src,0,r,c) {
        sum += P(src,r,c);
    }
    *result = sum;
    return 0;
}

int customInvert(IMG(src), IMG(dst)) {
    int r,c;
    TRAV(src,0,r,c) {
        P(dst,r,c) = 255 - P(src,r,c);
    }
    return 0;
}

int prucor(IMG(mask),IMG(src),IMG(hess), int th1, int th2, int max, int* tot, int* hp) {
    int r,c;
    int k;
    TRAV(src,1,r,c) {
        if (P(mask,r,c) > th1) {

            int y = P(src,r-1,c-1);
            int a = P(src,r-1,c);
            int u = P(src,r-1,c+1);
            int b = P(src,r,c-1);
            int x = P(src,r,c);
            int cc = P(src,r,c+1);
            int v = P(src,r+1,c-1);
            int d = P(src,r+1,c);
            int z = P(src,r+1,c+1);

            //int mx = MAX(MAX(MAX(MAX(y,a),MAX(u,b)),MAX(MAX(x,cc),MAX(v,d))),z);
            //int mn = MIN(MIN(MIN(MIN(y,a),MIN(u,b)),MIN(MIN(x,cc),MIN(v,d))),z);
            //int range = mx-mn;

            int gxx = y + z - 2*x;
            int gyy = u + v - 2*x;
            int gxy = a + d - b - cc;

            int h2 = gxx * gyy - gxy * gxy;
            int trz = gxx + gyy;
            double l1 = (trz+sqrt(trz*trz-4*h2))/2;
            double l2 = (trz-sqrt(trz*trz-4*h2))/2;
            double st = MIN(fabs(l1),fabs(l2));

            //double h2n = (double) h2 / (255*255);
            //double hn = h2n<0?sqrt(-h2n):0;
            //unsigned char h = hn*255;
            //printf("%d %f %d\n",range,h2n,h);

            //unsigned char h = h2<0 ? sqrt(-h2) : 0;
            unsigned char h = h2<0 ? st : 0;
            P(hess,r,c) = h>th2 ? h : 0;

        } else {
            P(hess,r,c) = 0;
        }
        //P(hess,r,c) = P(mask,r,c);
    }

    *tot = 0;
    TRAV(src,2,r,c) {
        if (    P(hess,r,c) > P(hess,r-1,c-1)
             && P(hess,r,c) > P(hess,r-1,c)
             && P(hess,r,c) > P(hess,r-1,c+1)
             && P(hess,r,c) > P(hess,r,c-1)
             && P(hess,r,c) > P(hess,r,c+1)
             && P(hess,r,c) > P(hess,r+1,c-1)
             && P(hess,r,c) > P(hess,r+1,c)
             && P(hess,r,c) > P(hess,r+1,c+1) ) {
                if (*tot < max) {
                    hp[3* *tot + 0] = r;
                    hp[3* *tot + 1] = c;
                    hp[3* *tot + 2] = P(hess,r,c);
                    *tot = (*tot)++;
                }
            }
    }

    return 0;
}
