#include <stdio.h>
#include <stdlib.h>
#include "HTools_stub.h"

float sum8u(unsigned char * p, int r, int c) {
    float s = 0; int k;
    for (k = 0; k<r*c; k++) s += p[k];
    return s;
}

int main(int argc, char *argv[]) {
    hs_init(&argc,&argv);

    int rows = 480, cols = 640;
    unsigned char * pSrc = (unsigned char*)malloc(rows*cols*sizeof(unsigned char*));
    unsigned char * pDst = (unsigned char*)malloc(rows*cols*sizeof(unsigned char*));

    int k;
    // load image
    for (k = 0; k<rows*cols; k++) {
        pSrc[k] = k % 256;
    }

    // initialization
    void *x = hfunInit("data.txt");
    
    // working with precomputed auxiliary data
    // and different arguments
    for (k=0; k<=3; k++) {
        hfun(x,k,pSrc,rows,cols,pDst);
        printf("%f\n",sum8u(pDst,rows,cols));
    }

    free(pSrc);
    free(pDst);
    
    hs_exit();
    return 0;
}

