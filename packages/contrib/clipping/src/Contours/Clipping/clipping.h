/*******************************************************
 *******************************************************
 ** Efficient clipping of arbitrary polygons
 ** Implementation by Adrián Amor Martínez
 ** Extracted from G. Greiner, K. Hormann
 ** ACM Transactions on Graphics
 *******************************************************
 *******************************************************/

#define STATUS_ENTRY 1
#define STATUS_EXIT 0

#define POLYGON_INTERIOR 1
#define POLYGON_EXTERIOR 0

#define POLYGON_INTERSECTION 0x1
#define POLYGON_UNION        0x2
#define POLYGON_DIFF_AB      0x4
#define POLYGON_XOREXT       0x8
#define POLYGON_DIFF_BA      0x16

#include <stdlib.h>

struct vertex {
    // coordinates
    double x;
    double y;

    // origin
    int o;

    // doubly linked list
    struct vertex *next, *prev;
    struct vertex *nextVertex;
    struct vertex *nextPoly;

    // used by the algorithm
    unsigned char intersect;
    unsigned char processed;
    char entry_exit;
    struct vertex *neighbour;

    double alpha,alpha_in_subject;
};


// Perform clipping of the polygon clip with nc points against
// a subject with ns points. Returns a set of nl polygons with specified lengths
// in an array of coordinates polys.
int clip(double *clipx, double *clipy, int nc,
         double *subjectx, double *subjecty, int ns,
         double **polysx, double **polysy, int **origin, int **ind0, int **ind1, double **alphas,
         int **lengths, int *nl, int*nlp, int *inside, int op);

