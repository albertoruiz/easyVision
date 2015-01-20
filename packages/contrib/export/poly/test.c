// ghc test.c -o testc HTools.hs -I/home/brutus/apps/ghc/lib/ghc-7.8.3/include/ -no-hs-main

#include <stdio.h>
#include <stdlib.h>
#include "HTools_stub.h"

typedef struct { double x; double y; } TPoint;

int main(int argc, char *argv[]) {
    hs_init(&argc,&argv); // haskell init
    hfunInit(); // HTools init (in this case do nothing)

    // load test input
    int n = 4;
    TPoint cont[4] = {{0,0},{1,0},{1,1},{0,1}};
    
    // output
    int m;
    TPoint *res;

    hfun(n,cont,&m,&res);
    
    //printf("%p\n",&m);
    //printf("%p\n",&res);
    
    printf("%d\n", m);
    
    int k;
    for (k=0; k<m; k++) {
        printf("px=%f  py=%f\n",res[k].x,res[k].y);
    }
    
    free(res);
    
    hs_exit();
    return 0;
}

