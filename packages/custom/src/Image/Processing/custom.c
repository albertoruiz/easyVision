#include "wrappers.h"
#include <stdio.h>
#include <math.h>


int histogram3D(int n, int d, IM1(src), int vn, float*vp) {
    int r,c,k;
    int x,y,z;
    for (k=0;k<vn;k++) vp[k] = 0;

    TRAV(src,0,r,c) {
        x = PM(src,r,c,0) >> d;
        y = PM(src,r,c,1) >> d;
        z = PM(src,r,c,2) >> d;
        vp[x*n*n + y*n + z]++;
    }
    return 0;
}

