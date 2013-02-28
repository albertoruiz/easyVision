#include <stdio.h>
#include "HSTools_stub.h"

int main(int argc, char *argv[]) {
    hs_init(&argc,&argv);

    int r = hsfun(5);
    printf("hsFun = %d\n", r);

    int k;
    for (k=0; k<=5; k++) {
        double x = k;
        printf("hf2(%f)=%f\n",x,hf2(x));
    } 

    hs_exit();
    return 0;
}

