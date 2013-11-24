#include "wrappers.h"
#include <stdio.h>
#include <math.h>

int customSum(int*result, IM1(src)) {
    int r,c;
    int sum = 0;
    TRAV(src,0,r,c) {
        sum += P(src,r,c);
    }
    *result = sum;
    return 0;
}

int customInvert(IM1(src), IM1(dst)) {
    int r,c;
    TRAV(src,0,r,c) {
        P(dst,r,c) = 255 - P(src,r,c);
    }
    return 0;
}

