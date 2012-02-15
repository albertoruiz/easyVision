#include <stdio.h>
#include "Module_stub.h"

int main(int argc, char *argv[]) {
    hs_init(&argc,&argv);

    int r = hsfun(5);
    printf("hsFun = %d\n", r);

    hs_exit();
    return 0;
}

