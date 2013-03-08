#include <stdio.h>
#include <stdlib.h>
#include "HTools_stub.h"

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
    
    hfun(pSrc,rows,cols, pDst); // result goes to different buffer
    
    // check result
    for (k = 0; k<10; k++) {
        printf("%i %i\n",k,pDst[k]);
    }

    hfun(pDst,rows,cols,pDst);  // result overwrites input buffer
    
    // check result
    for (k = 0; k<10; k++) {
        printf("%i %i\n",k,pDst[k]);
    }

    void *x = hfunparInit();        // initialization
    hfunpar(x,pSrc,rows,cols,pDst); // working with precomputed auxiliary data

    // check result (first row)
    for (k = 0; k<cols; k++) {
        printf("%i ",pDst[k]);
    }
    printf("\n");


    free(pSrc);
    free(pDst);
    
    hs_exit();
    return 0;
}

