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

////////////////////////////////////////////////////////////////////////////////

int domainTrans32f(float w2, IMF(x),IMF(y),IMF(a),IMF(res)) {
    int r,c,nr,nc;
    TRAV(res,0,r,c) {
        nr = r + (int)(0.5+w2*PF(y,r,c));
        nc = c + (int)(0.5+w2*PF(x,r,c));
        if (nr >= ar1 && nr <= ar2 && nc >= ac1 && nc <= ac2) {
            PF(res,r,c) = PF(a,nr,nc);
        }
    }
    return 0;
}

