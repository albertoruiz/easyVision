#include <stdio.h>
#include <stdlib.h>
#include "HTools_stub.h"

typedef struct { double x; double y; } TPoint;

int main(int argc, char *argv[]) {
    hs_init(&argc,&argv);

    // load test input
    int n1 = 5;
    TPoint proto[5] = {{0,0},{0.5,0},{1,0},{1,1},{0,1}};
    
    int n2 = 4;
    TPoint target[4] = {{1.1,0.1},{1.1,0.9},{-0.1,0.9},{-0.1,0.1}};
    
    // output
    int m;
    TPoint *res;
    double h[3][3];

    hfun(n1,proto,n2,target,&m,&res,h);
        
    printf("%d\n", m);
    int k;
    for (k=0; k<m; k++) {
        printf("px=%f  py=%f\n",res[k].x,res[k].y);
    }
    int j;
    for (j=0; j<3; j++) {
        for (k=0; k<3; k++) {
            printf("%f\n",h[j][k]);
         }
    }
    free(res);
    
    hs_exit();
    return 0;
}

