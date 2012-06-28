#include "clipping.h"
#include <stdio.h>

// Auxiliary max and min macros:
#define MIN(a,b) ( (a) < (b) ? (a) : (b) )
#define MAX(a,b) ( (a) > (b) ? (a) : (b) )

// Macro for fast discarding intersections:
#define DISCARD_INTER(px, py, qx, qy, minX, minY, maxX, maxY) \
          (( ((((px) < (minX)) << 3) | (((px) > (maxX)) << 2) | (((py) < (minY)) << 1) | ((py) > (maxY))) & \
             ((((qx) < (minX)) << 3) | (((qx) > (maxX)) << 2) | (((qy) < (minY)) << 1) | ((qy) > (maxY))) ) != 0 )

// Perturbation factor (relative to perturbed segment length):
#define EPSILON 0.00001

// Flags for the "mark entries" phase of the algorithm:
#define STATUS_ENTRY 1
#define STATUS_EXIT 0

// Flags to know if current vertex is inside or outside the polygon:
#define POLYGON_INTERIOR 1
#define POLYGON_EXTERIOR 0

// Possible cases for intersections of two segments:
#define NO_INTERSECTION        0
#define INTERNAL_INTERSECTION  1
#define BORDER_INTERSECTION    2
#define PARALLEL_CASE          3

// Main data structure used by the algorithm (node to store every input or newly generated vertex):
struct vertex {
    // X Y coordinates (original and saved):
    double x, y, x_saved, y_saved;

    // Pointers to (doubly) link input and generated vertices in input polygons:
    struct vertex *next, *prev;

    // Pointers to (simply) link vertices in output polygons:
    struct vertex *nextVertex;
    struct vertex *nextPoly;

    // Pointer to analogous intersection node in the other polygon:
    struct vertex *neighbour;

    // Origin of each vertex (ORIGIN_A, ORIGIN_B, ORIGIN_INT_ENTERS_A or ORIGIN_INT_ENTERS_B):
    int origin;

    // Boolean flags internally used by the algorithm:
    unsigned char intersect;
    unsigned char processed;
    unsigned char entry_exit;
    unsigned char perturbed;

    // Integer indexes and double values to precisely locate each node in the corresponding input polygon:
    double alpha, alphaA, alphaB;
    int ind0, ind1, ind0A, ind1A, ind0B, ind1B;
};


// Macro to set all the value fields in a node:
#define SET_NODE(current,P0,P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14,P15,P16,P17,P18,P19,P20,P21,P22) \
    current->x          = P0;  \
    current->y          = P1;  \
    current->x_saved    = P2;  \
    current->y_saved    = P3;  \
    current->next       = P4;  \
    current->prev       = P5;  \
    current->nextVertex = P6;  \
    current->nextPoly   = P7;  \
    current->neighbour  = P8;  \
    current->origin     = P9;  \
    current->intersect  = P10; \
    current->processed  = P11; \
    current->entry_exit = P12; \
    current->perturbed  = P13; \
    current->alpha      = P14; \
    current->alphaA     = P15; \
    current->alphaB     = P16; \
    current->ind0       = P17; \
    current->ind1       = P18; \
    current->ind0A      = P19; \
    current->ind1A      = P20; \
    current->ind0B      = P21; \
    current->ind1B      = P22;


/**
  * Create a new linked list from floating points values pointed by polygon given by  @polyx and @polyy,
  * of size @n, and return it into a pointer pointed by @l. Origin polygon must be given by @origin.
  */
void createList(int origin, double *polyx, double *polyy, int n, struct vertex **l)
{
    int i;
    struct vertex *list = (struct vertex *) malloc (sizeof(struct vertex));
    struct vertex *prev = NULL;
    struct vertex *current = list;

    // Ignore last point which must be equal to the first point
    n = n-1;

    for (i=0; i<n-1; i++) {
        SET_NODE(current,
                 /*x*/ polyx[i], /*y*/ polyy[i], /*x_saved*/ polyx[i], /*y_saved*/ polyy[i],
                 /*next*/ (struct vertex *) malloc (sizeof(struct vertex)), /*prev*/ prev,
                 /*nextVertex*/ current->next, /*nextPoly*/ NULL, /*neighbour*/ NULL,
                 /*origin*/ origin, /*intersect*/ 0, /*processed*/ 0, /*entry_exit*/ 0, /*perturbed*/ 0,
                 /*alpha*/ 0.0, /*alphaA*/ 0.0, /*alphaB*/ 0.0,
                 /*ind0*/ -1, /*ind1*/ -1, /*ind0A*/ -1, /*ind1A*/ -1, /*ind0B*/ -1, /*ind1B*/ -1);
        prev = current;
        current = current->next;
    }

    SET_NODE(current,
             /*x*/ polyx[i], /*y*/ polyy[i], /*x_saved*/ polyx[i], /*y_saved*/ polyy[i],
             /*next*/ list, /*prev*/ prev,
             /*nextVertex*/ list, /*nextPoly*/ NULL, /*neighbour*/ NULL,
             /*origin*/ origin, /*intersect*/ 0, /*processed*/ 0, /*entry_exit*/ 0, /*perturbed*/ 0,
             /*alpha*/ 0.0, /*alphaA*/ 0.0, /*alphaB*/ 0.0,
             /*ind0*/ -1, /*ind1*/ -1, /*ind0A*/ -1, /*ind1A*/ -1, /*ind0B*/ -1, /*ind1B*/ -1);

    list->prev = current;
    *l = list;
}

/**
  * Delete linked list corresponding to an input polygon (linked by next).
  */
void deleteList(struct vertex *l)
{
    struct vertex *aux = l->next;
    l->prev->next = NULL;
    while (aux) {
        free(l);
        l = aux;
        aux = aux->next;
    }
    free(l);
}

/**
  * Delete linked list corresponding to output polygons (linked by nextPoly, and inside each polygon by next).
  */
void deletePolygons(struct vertex *poly)
{
    struct vertex *aux, *l;
    struct vertex *paux;

    paux = poly;
    while (paux) {
        l = paux;
        paux = paux->nextPoly;

        aux = l->next;
        while (aux) {
            free(l);
            l = aux;
            aux = aux->next;
        }
        free(l);
    }
}

/**
  * Tries to intersects a pair of segments given by their extremes (@p1,@p2) and (@q1,@q2). It returns one of the
  * possible cases PARALLEL_CASE, INTERNAL_INTERSECTION, BORDER_INTERSECTION or NO_INTERSECTION, depending on the
  * relative incidences of the input segments. It also returns the relative position of the intersection in
  * the input segments in the output parameters @alphaP and @alphaQ, which are both in the interval [0.0,1.0).
  */
int intersect(struct vertex *p1, struct vertex *p2,
    struct vertex *q1, struct vertex *q2, double *alphaP, double *alphaQ)
{
    double wec_p1, wec_p2, wec_q1, wec_q2;
    wec_p1 = (p1->x - q1->x)*(q1->y - q2->y) + (p1->y - q1->y)*(q2->x - q1->x);
    wec_p2 = (p2->x - q1->x)*(q1->y - q2->y) + (p2->y - q1->y)*(q2->x - q1->x);

    if (wec_p1 * wec_p2 <= 0)
    {
        wec_q1 = (q1->x - p1->x)*(p1->y - p2->y) + (q1->y - p1->y)*(p2->x - p1->x);
        wec_q2 = (q2->x - p1->x)*(p1->y - p2->y) + (q2->y - p1->y)*(p2->x - p1->x);

        if (wec_q1 * wec_q2 <= 0)
        {
            if( (wec_p1-wec_p2==0.0) || (wec_q1-wec_q2==0.0) ) {
                return PARALLEL_CASE;
            }
            *alphaP = wec_p1/(wec_p1 - wec_p2);
            *alphaQ = wec_q1/(wec_q1 - wec_q2);

            if ( (*alphaP != 0.0) && (*alphaP != 1.0) && (*alphaQ != 0.0) && (*alphaQ != 1.0) ) {
                return INTERNAL_INTERSECTION;
            } else {
                return BORDER_INTERSECTION;
            }
        }
    }

    return NO_INTERSECTION;
}

/**
  * Find and add intersection vertices between input polygons given by lists @lA and @lB. The function also
  * returns the total number of intersections found.
  */
int findIntersections(struct vertex *lA, struct vertex *lB)
{
    double alphaA, alphaB, x1, y1, x2, y2, x3, y3, x4, y4;
    int intersections = 0, result;
    struct vertex *aux, *v, *w, *sort;
    for (v = lA; v; v = v->nextVertex == lA ? NULL : v->nextVertex)
        for (w = lB; w; w = w->nextVertex == lB ? NULL : w->nextVertex)
            if (!DISCARD_INTER(v->x, v->y, v->nextVertex->x, v->nextVertex->y,
                               MIN(w->x, w->nextVertex->x), MIN(w->y, w->nextVertex->y),
                               MAX(w->x, w->nextVertex->x), MAX(w->y, w->nextVertex->y))
                && ((result = intersect(v, v->nextVertex, w, w->nextVertex, &alphaA, &alphaB)) != NO_INTERSECTION))
            {
                x1 = v->x;
                y1 = v->y;
                x2 = v->nextVertex->x;
                y2 = v->nextVertex->y;
                x3 = w->x;
                y3 = w->y;
                x4 = w->nextVertex->x;
                y4 = w->nextVertex->y;

                if(result == PARALLEL_CASE) {
                    // Perturb first segment, first extreme perpendicularly...
                    if(!v->perturbed) {
                        v->x_saved = v->x;
                        v->y_saved = v->y;
                        v->perturbed = 1;
                    }
                    v->x = x1 - EPSILON*(y2-y1);
                    v->y = y1 - EPSILON*(x1-x2);
                    x1 = v->x;
                    y1 = v->y;
                    // ... then intersect again ...
                    result = intersect(v, v->nextVertex, w, w->nextVertex, &alphaA, &alphaB);
                    // ... and if there is no intersection now, continue to next pair of segments.
                    // (otherwise, it will be a INTERNAL_INTERSECTION or a BORDER_INTERSECTION, that
                    // will be treated in the normal continuation code).
                    if(result == NO_INTERSECTION) {
                        continue;
                    }
                }

                // If it is a border intersection, perturb corresponding extreme to avoid it:
                if(result == BORDER_INTERSECTION) {
                    // Slightly shorten segment extreme when perturbing (so that no intersection will occur):
                    if(alphaA == 0.0) {
                        if(!v->perturbed) {
                            v->x_saved = v->x;
                            v->y_saved = v->y;
                            v->perturbed = 1;
                        }
                        v->x = (x1-x2)*(1.0-EPSILON) + x2;
                        v->y = (y1-y2)*(1.0-EPSILON) + y2;
                    } else if(alphaA == 1.0) {
                        if(!v->nextVertex->perturbed) {
                            v->nextVertex->x_saved = v->nextVertex->x;
                            v->nextVertex->y_saved = v->nextVertex->y;
                            v->nextVertex->perturbed = 1;
                        }
                        v->nextVertex->x = (x2-x1)*(1.0-EPSILON) + x1;
                        v->nextVertex->y = (y2-y1)*(1.0-EPSILON) + y1;
                    } else if(alphaB == 0.0) {
                        if(!w->perturbed) {                        
                            w->x_saved = w->x;
                            w->y_saved = w->y;
                            w->perturbed = 1;
                        }
                        w->x = (x3-x4)*(1.0-EPSILON) + x4;
                        w->y = (y3-y4)*(1.0-EPSILON) + y4;
                    } else if(alphaB == 1.0) {
                        if(!w->nextVertex->perturbed) {
                            w->nextVertex->x_saved = w->nextVertex->x;
                            w->nextVertex->y_saved = w->nextVertex->y;
                            w->nextVertex->perturbed = 1;
                        }
                        w->nextVertex->x = (x4-x3)*(1.0-EPSILON) + x3;
                        w->nextVertex->y = (y4-y3)*(1.0-EPSILON) + y3;
                    }
                    // As we always shorten corresponding segment when perturbing, no intersection will occur;
                    // thus, we can safely continue with next pair of segments.
                    continue;
                }

                // If we get here, it is an internal intersection (result == INTERNAL_INTERSECTION),
                // this is the "normal case":

                // Create and fill nodes corresponding to intersection points (one on each input polygon):
                struct vertex *i1 = (struct vertex *) malloc (sizeof(struct vertex));
                struct vertex *i2 = (struct vertex *) malloc (sizeof(struct vertex));

                double x_cut = w->x+alphaB*(w->nextVertex->x-w->x),
                       y_cut = w->y+alphaB*(w->nextVertex->y-w->y);

                SET_NODE(i1,
                 /*x*/ x_cut, /*y*/ y_cut, /*x_saved*/ x_cut, /*y_saved*/ y_cut,
                 /*next*/ NULL, /*prev*/ NULL,
                 /*nextVertex*/ NULL, /*nextPoly*/ NULL, /*neighbour*/ i2,
                 /*origin*/ ORIGIN_INT_ENTERS_B, /*intersect*/ 1, /*processed*/ 0, /*entry_exit*/ 0, /*perturbed*/ 0,
                 /*alpha*/ alphaA, /*alphaA*/ alphaA, /*alphaB*/ alphaB,
                 /*ind0*/ -1, /*ind1*/ -1, /*ind0A*/ -1, /*ind1A*/ -1, /*ind0B*/ -1, /*ind1B*/ -1);

                SET_NODE(i2,
                 /*x*/ x_cut, /*y*/ y_cut, /*x_saved*/ x_cut, /*y_saved*/ y_cut,
                 /*next*/ NULL, /*prev*/ NULL,
                 /*nextVertex*/ NULL, /*nextPoly*/ NULL, /*neighbour*/ i1,
                 /*origin*/ ORIGIN_INT_ENTERS_A, /*intersect*/ 1, /*processed*/ 0, /*entry_exit*/ 0, /*perturbed*/ 0,
                 /*alpha*/ alphaB, /*alphaA*/ alphaA, /*alphaB*/ alphaB,
                 /*ind0*/ -1, /*ind1*/ -1, /*ind0A*/ -1, /*ind1A*/ -1, /*ind0B*/ -1, /*ind1B*/ -1);

                // Insert intersection points into polygons, in the correct position according to its alpha values:
                sort = v;
                while (sort->next && sort->next->intersect
                    && sort->next->alpha < i1->alpha)
                    sort = sort->next;
                aux = sort->next;
                sort->next = i1;
                i1->next = aux;
                aux->prev = i1;
                i1->prev = sort;

                sort = w;
                while (sort->next && sort->next->intersect
                    && sort->next->alpha < i2->alpha)
                    sort = sort->next;
                aux = sort->next;
                sort->next = i2;
                i2->next = aux;
                aux->prev = i2;
                i2->prev = sort;

                // Count total number of intersections:
                intersections++;
            }

    return intersections;
}

/**
  * Determine whether point @p is inside polygon @polygon or not.
  */
int isInside(struct vertex *p, struct vertex *polygon)
{
    int oddNodes = 0;
    const double x = p->x, y = p->y;
    struct vertex *node1, *node2, *q;

    for (q = polygon; q; q = q->nextVertex == polygon ? NULL : q->nextVertex) {
        node1 = q;

        if (q->nextVertex)
            node2 = q->nextVertex;
        else
            node2 = polygon;

        if ((node1->y < y && node2->y >= y) || (node2->y < y && node1->y >= y)) {
            if (node1->x + (y - node1->y) / (node2->y - node1->y) * (node2->x - node1->x) < x)
                oddNodes = !oddNodes;
        }
    }

    return oddNodes;
}

/**
  * Mark intersection points in polygon @p depending on whether it's an entry or exit point with respect the
  * @interior_exterior of polygon @q. At the same time, it also fills the corresponding indexes in all the nodes.
  */
void markEntries(struct vertex *p, struct vertex *q, int interior_exterior)
{
    int status, ind0, ind1;
    struct vertex *pi;

    if (isInside(p, q))
        status = interior_exterior == POLYGON_INTERIOR ? STATUS_ENTRY : STATUS_EXIT;
    else
        status = interior_exterior == POLYGON_INTERIOR ? STATUS_EXIT : STATUS_ENTRY;

    p->ind0 = p->ind1 = ind0 = ind1 = 0;
    for (pi = p->next; pi != p; pi = pi->next) {
        if (pi->intersect) {
            pi->entry_exit = status;
            pi->processed = 0;
            status = !status;
            ind1++;
        } else {
            ind0++;
            ind1 = 0;
        }
        pi->ind0 = ind0;
        pi->ind1 = ind1;
    }
}

/**
  * Transfer indexes and alphas among corresponding intersection nodes in the other polygon. Function receives
  * @origin polygon and corresponding list of vertices @p as input parameters.
  */
void transferIndexesAndAlphas(int origin, struct vertex *p)
{
    struct vertex *pi;

    if(origin == ORIGIN_A) {
        p->ind0B = p->ind0;
        p->ind1B = p->ind1;
    } else if(origin == ORIGIN_B) {
        p->ind0A = p->ind0;
        p->ind1A = p->ind1;
    }

    for (pi = p->next; pi != p; pi = pi->next) {
        if(origin == ORIGIN_A) {
            if (pi->intersect) {
                pi->ind0A = pi->neighbour->ind0;
                pi->ind1A = pi->neighbour->ind1;
            }
            pi->ind0B = pi->ind0;
            pi->ind1B = pi->ind1;
        } else if(origin == ORIGIN_B) {
            if (pi->intersect) {
                pi->ind0B = pi->neighbour->ind0;
                pi->ind1B = pi->neighbour->ind1;
            }
            pi->ind0A = pi->ind0;
            pi->ind1A = pi->ind1;
        }
    }
}



/**
  * Adds a new polygon to a previous one pointed by @lastPoly with starting vertex @p.
  */
struct vertex * newPolygon(struct vertex *lastPoly, struct vertex *p)
{
    struct vertex *poly = (struct vertex *) malloc (sizeof(struct vertex));

    SET_NODE(poly,
     /*x*/ p->x, /*y*/ p->y, /*x_saved*/ p->x_saved, /*y_saved*/ p->y_saved,
     /*next*/ NULL, /*prev*/ NULL,
     /*nextVertex*/ NULL, /*nextPoly*/ NULL, /*neighbour*/ NULL,
     /*origin*/ p->origin, /*intersect*/ 0, /*processed*/ 0, /*entry_exit*/ 0, /*perturbed*/ p->perturbed,
     /*alpha*/ 0.0, /*alphaA*/ p->alphaA, /*alphaB*/ p->alphaB,
     /*ind0*/ p->ind0, /*ind1*/ p->ind1, /*ind0A*/ p->ind0A,
     /*ind1A*/ p->ind1A, /*ind0B*/ p->ind0B, /*ind1B*/ p->ind1B);

    if (lastPoly)
        lastPoly->nextPoly = poly;

    return poly;
}

/**
  * Adds new vertex to a previous one pointed by @last.
  */
void newVertex(struct vertex *last, struct vertex *p)
{
    struct vertex *point = last->next = (struct vertex *) malloc (sizeof(struct vertex));

    SET_NODE(point,
     /*x*/ p->x, /*y*/ p->y, /*x_saved*/ p->x_saved, /*y_saved*/ p->y_saved,
     /*next*/ NULL, /*prev*/ NULL,
     /*nextVertex*/ NULL, /*nextPoly*/ NULL, /*neighbour*/ NULL,
     /*origin*/ p->origin, /*intersect*/ 0, /*processed*/ 0, /*entry_exit*/ 0, /*perturbed*/ p->perturbed,
     /*alpha*/ 0.0, /*alphaA*/ p->alphaA, /*alphaB*/ p->alphaB,
     /*ind0*/ p->ind0, /*ind1*/ p->ind1, /*ind0A*/ p->ind0A,
     /*ind1A*/ p->ind1A, /*ind0B*/ p->ind0B, /*ind1B*/ p->ind1B);
}


/**
  * Performs real operation on input polygons @lA and @lB (once they have been correctly intersected and marked),
  * storing output polygons in @polygons structure, and returning the total number of found polygons, and total
  * number of output vertices in @total.
  */
int createClippedPolygon(struct vertex *lA, struct vertex *lB, struct vertex **polygons, int *total)
{
    struct vertex *iB = lB->next, *current, *poly = NULL, *first = NULL, *lastPoint;
    int npolys = 0, nvertex = 0;

    while (iB) {
        for (; iB != lB && !(iB->intersect && !iB->processed);
                iB = iB->next);

        if (iB == lB)
            break;

        iB->processed = 1;
        current = iB;

        if (first == NULL) {
            first = (struct vertex *) malloc (sizeof(struct vertex));

            SET_NODE(first,
             /*x*/ current->x, /*y*/ current->y, /*x_saved*/ current->x_saved, /*y_saved*/ current->y_saved,
             /*next*/ NULL, /*prev*/ NULL, /*nextVertex*/ NULL, /*nextPoly*/ NULL, /*neighbour*/ NULL,
             /*origin*/ current->origin, /*intersect*/ 0, /*processed*/ 0,
             /*entry_exit*/ 0, /*perturbed*/ current->perturbed,
             /*alpha*/ 0.0, /*alphaA*/ current->alphaA, /*alphaB*/ current->alphaB,
             /*ind0*/ current->ind0, /*ind1*/ current->ind1,
             /*ind0A*/ current->ind0A, /*ind1A*/ current->ind1A,
             /*ind0B*/ current->ind0B, /*ind1B*/ current->ind1B);

            poly = first;
        } else
            poly = newPolygon(poly, current);

        npolys++;
        nvertex++;

        lastPoint = poly;
        do {
            if (current->entry_exit == STATUS_ENTRY)
                do {
                    current = current->next;
                    newVertex(lastPoint, current);
                    lastPoint = lastPoint->next;
                    nvertex++;
                } while (!current->intersect);
            else
                do {
                    current = current->prev;
                    newVertex(lastPoint, current);
                    lastPoint = lastPoint->next;
                    nvertex++;
                } while (!current->intersect);

            current->processed = 1;
            current = current->neighbour;
            current->processed = 1;
        } while (! (poly->x == current->x && poly->y == current->y) );
    }

    if (polygons)
        *polygons = first;

    if (total)
        *total = nvertex;

    return npolys;
}


/**
  * Copy the polygons list structure into corresponding arrays of integer and double precision floats
  * containing the indexes and X and Y coordinates of each point from the resulting polygon. See doxygen
  * documentation on clip(...) function for details.
  */
void copy(struct vertex *polygons, int npolys, int nvertex, double **polysx, double **polysy, int **origin,
          int **ind0A, int **ind1A, int **ind0B, int **ind1B, double **alphaA, double **alphaB, int **lengths)
{
    int curlen, polycount = 0, vertexcount = 0;
    struct vertex *ipoly, *ivertex;

    *polysx  = (double *) malloc (nvertex * sizeof(double));
    *polysy  = (double *) malloc (nvertex * sizeof(double));
    *alphaA  = (double *) malloc (nvertex * sizeof(double));
    *alphaB  = (double *) malloc (nvertex * sizeof(double));
    *origin  = (int *)    malloc (nvertex * sizeof(int));
    *ind0A   = (int *)    malloc (nvertex * sizeof(int));
    *ind1A   = (int *)    malloc (nvertex * sizeof(int));
    *ind0B   = (int *)    malloc (nvertex * sizeof(int));
    *ind1B   = (int *)    malloc (nvertex * sizeof(int));
    *lengths = (int *)    malloc (npolys * sizeof(int));

    for (ipoly = polygons; ipoly; ipoly = ipoly->nextPoly) {
        (*lengths)[polycount] = 0;
        curlen = 0;
        for (ivertex = ipoly; ivertex; ivertex = ivertex->next) {
            (*lengths)[polycount]++;
            (*origin)[vertexcount] = ivertex->origin;
            if(ivertex->perturbed) {
                (*polysx)[vertexcount] = ivertex->x_saved;
                (*polysy)[vertexcount] = ivertex->y_saved;
            } else {
                (*polysx)[vertexcount] = ivertex->x;
                (*polysy)[vertexcount] = ivertex->y;
            }
            (*alphaA)[vertexcount] = ivertex->alphaA;
            (*alphaB)[vertexcount] = ivertex->alphaB;
            (*ind0A)[vertexcount]  = ivertex->ind0A;
            (*ind1A)[vertexcount]  = ivertex->ind1A;
            (*ind0B)[vertexcount]  = ivertex->ind0B;
            (*ind1B)[vertexcount]  = ivertex->ind1B;
            vertexcount++;
            curlen++;
        }
        // Last (repeated) vertex copies its origin label to first one (which was initially "flipped").
        (*origin)[vertexcount-curlen] = (*origin)[vertexcount-1];
        polycount++;
    }
}


/**
  * Performs clipping operation for input polygons A and B. See doxygen documentation in header file clipping.h
  * for details on input and output parameters.
  */
int clip(double *Ax, double *Ay, int nA, double *Bx, double *By, int nB, int op,
         double **polysx, double **polysy, int **lengths, int *nl, int*nlp, int *status,
         int **origin, int **ind0A, int **ind1A, int **ind0B, int **ind1B, double **alphaA, double **alphaB)

{
    struct vertex *lA, *lB, *polygons = NULL, *polygons2 = NULL, *auxpoly = NULL;
    int cIntExt = 0, sIntExt = 0, nvertex, nvertex2, npolys, npolys2, intersections;

    // 0. Create list of vertices for input polygons:
    createList(ORIGIN_A, Ax, Ay, nA, &lA);
    createList(ORIGIN_B, Bx, By, nB, &lB);

    // 1. Phase one of algorithm: find and insert all intersections in input polygons:
    intersections = findIntersections(lA, lB);

    //printf("%d intersections found\n",intersections);

    if(intersections == 0) {
        // If no intersections found, just determine in which of the special cases (A_INSIDE_B, B_INSIDE_A or
        // DISJOINT_POLYGONS) we are:
        if (isInside(lA,lB))
            *status = A_INSIDE_B;
        else if (isInside(lB,lA))
            *status = B_INSIDE_A;
        else
            *status = DISJOINT_POLYGONS;
    } else {
        // Otherwise, we are in the normal case, polygons do really intersect.

        // 2. Phase two of algorithm: mark entries in each input polygon according to asked operation:
        *status = POLYGONS_INTERSECT;
        switch(op) {
            case POLYGON_UNION:
                cIntExt = sIntExt = POLYGON_INTERIOR;
                break;
            case POLYGON_INTERSECTION:
                cIntExt = sIntExt = POLYGON_EXTERIOR;
                break;
            case POLYGON_DIFF_AB:
                cIntExt = POLYGON_EXTERIOR;
                sIntExt = POLYGON_INTERIOR;
                break;
            case POLYGON_XOREXT:
                cIntExt = POLYGON_EXTERIOR;
                sIntExt = POLYGON_INTERIOR;
                break;
            case POLYGON_DIFF_BA:
                cIntExt = POLYGON_INTERIOR;
                sIntExt = POLYGON_EXTERIOR;
                break;
            }
        markEntries(lA, lB, sIntExt);
        markEntries(lB, lA, cIntExt);

        // Transfer indexes and alphas among input polygons:
        transferIndexesAndAlphas(ORIGIN_A, lB);
        transferIndexesAndAlphas(ORIGIN_B, lA);

        // 3. Phase three of algorithm: create output polygons from marked input polygons:
        npolys = createClippedPolygon(lA, lB, &polygons, &nvertex);

        // For the XOR operation, we must repeat phases 2 and 3, with different sign (we computed A-B, now we
        // comput B-A)
        if(op == POLYGON_XOREXT && npolys > 0) {
            cIntExt = POLYGON_INTERIOR;
            sIntExt = POLYGON_EXTERIOR;
            markEntries(lA, lB, sIntExt);
            markEntries(lB, lA, cIntExt);
            transferIndexesAndAlphas(ORIGIN_A, lB);
            transferIndexesAndAlphas(ORIGIN_B, lA);
            npolys2 = createClippedPolygon(lA, lB, &polygons2, &nvertex2);

            // We join A-B and B-A lists, taking note of the number of "positive" (A-B) and negetive (B-A)
            // output polygons:
            *nlp = npolys;
            npolys += npolys2;
            nvertex += nvertex2;
            auxpoly = polygons;
            while(auxpoly->nextPoly)
                auxpoly = auxpoly->nextPoly;
            auxpoly->nextPoly = polygons2;
        } else {
            // For the normal case (with intersections!=0), only in the xorext case nlp differs from npolys:
            *nlp = npolys;
        }
    }

    if(*status == POLYGONS_INTERSECT) {
        // If the input polygons really intersected, we must copy output data from output polygons structure
        // to output arrays:
        copy(polygons, npolys, nvertex, polysx, polysy, origin,
             ind0A, ind1A, ind0B, ind1B, alphaA, alphaB, lengths);

        // Free memory for output polygons.
        deletePolygons(polygons);
    } else {
        // Otherwise, depending on the asked operation, and the relative position of input polygons A and B, we
        // directly copy polygon A, polygon B, both, or none of them to the output. We also take care of
        // correctly writing "positive" and "negative" polygons:
        int i, j, copyA = 0, copyB = 0, firstB = 0;
        switch(op) {
            case POLYGON_UNION:
                switch(*status) {
                    case A_INSIDE_B:
                        copyB = 1; firstB = 1; *nlp = 1;
                        break;
                    case B_INSIDE_A:
                        copyA = 1; *nlp = 1;
                        break;
                    case DISJOINT_POLYGONS:
                        copyB = copyA = 1; *nlp = 2;
                        break;
                }
                break;
            case POLYGON_INTERSECTION:
                switch(*status) {
                    case A_INSIDE_B:
                        copyA = 1; *nlp = 1;
                        break;
                    case B_INSIDE_A:
                        copyB = 1; firstB = 1; *nlp = 1;
                        break;
                    case DISJOINT_POLYGONS:
                        *nlp = 0;
                        break;
                }
                break;
            case POLYGON_DIFF_AB:
                switch(*status) {
                    case A_INSIDE_B:
                        *nlp = 0;
                        break;
                    case B_INSIDE_A:
                        copyA = copyB = 1; *nlp = 1;
                        break;
                    case DISJOINT_POLYGONS:
                        copyA = 1; *nlp = 1;
                        break;
                }
                break;
            case POLYGON_XOREXT:
                switch(*status) {
                    case A_INSIDE_B:
                        copyA = copyB = 1; firstB = 1; *nlp = 1;
                        break;
                    case B_INSIDE_A:
                        copyA = copyB = 1; *nlp = 1;
                        break;
                    case DISJOINT_POLYGONS:
                        copyA = copyB = 1; *nlp = 1;
                        break;
                }
                break;
            case POLYGON_DIFF_BA:
                switch(*status) {
                    case A_INSIDE_B:
                        copyA = copyB = 1; firstB = 1; *nlp = 1;
                        break;
                    case B_INSIDE_A:
                        *nlp = 0;
                        break;
                    case DISJOINT_POLYGONS:
                        copyB = 1; firstB = 1; *nlp = 1;
                        break;
                }
                break;
            }

        nvertex = 0;
        npolys = 0;
        if(copyA) {
            nvertex += nA;
            npolys += 1;
        }
        if(copyB) {
            nvertex += nB;
            npolys += 1;
        }

        *polysx  = (double *) ( (nvertex!=0) ? malloc (nvertex * sizeof(double)):NULL );
        *polysy  = (double *) ( (nvertex!=0) ? malloc (nvertex * sizeof(double)):NULL );
        *origin  = (int *)    ( (nvertex!=0) ? malloc (nvertex * sizeof(int))   :NULL );
        *ind0A   = (int *)    ( (nvertex!=0) ? malloc (nvertex * sizeof(int))   :NULL );
        *ind1A   = (int *)    ( (nvertex!=0) ? malloc (nvertex * sizeof(int))   :NULL );
        *ind0B   = (int *)    ( (nvertex!=0) ? malloc (nvertex * sizeof(int))   :NULL );
        *ind1B   = (int *)    ( (nvertex!=0) ? malloc (nvertex * sizeof(int))   :NULL );
        *alphaA  = (double *) ( (nvertex!=0) ? malloc (nvertex * sizeof(double)):NULL );
        *alphaB  = (double *) ( (nvertex!=0) ? malloc (nvertex * sizeof(double)):NULL );
        *lengths = (int *)    ( (npolys !=0) ? malloc (npolys  * sizeof(int))   :NULL );

        if(npolys == 1) {
            (*lengths)[0] = copyA?nA:nB;
        } else if(npolys == 2) {
            (*lengths)[0] = firstB?nB:nA;
            (*lengths)[1] = firstB?nA:nB;
        }

        if(copyA) {
            j = firstB?nB:0;
            for(i=0; i<nA; i++) {
                (*polysx)[j] = Ax[i];
                (*polysy)[j] = Ay[i];
                (*origin)[j] = 1;
                (*ind0A)[j]  = i;
                (*ind1A)[j]  = 0;
                (*ind0B)[j]  = -1;
                (*ind1B)[j]  = -1;
                (*alphaB)[j] = 0.0;
                (*alphaA)[j] = 0.0;
                j++;
            }
        }

        if(copyB) {
            j = firstB?0:nA;
            for(i=0; i<nB; i++) {
                (*polysx)[j] = Bx[i];
                (*polysy)[j] = By[i];
                (*origin)[j] = 2;
                (*ind0A)[j]  = -1;
                (*ind1A)[j]  = -1;
                (*ind0B)[j]  = i;
                (*ind1B)[j]  = 0;
                (*alphaB)[j] = 0.0;
                (*alphaA)[j] = 0.0;
                j++;
            }
        }
    }

    // Free lists of input polygons:
    deleteList(lA);
    deleteList(lB);

    *nl = npolys;

    // FIXME: By now we still do not check correctnes in input parameters.
    return 0;
}

/**
  * Read input polygons from stdin into corresponding arrays.
  */
void readFromStdin(double **Ax, double **Ay, int *lA, double **Bx, double **By, int *lB)
{
    int i, nread = scanf("%d %d\n", lA, lB);

    *Ax = (double *) malloc ((*lA)*sizeof(double));
    *Ay = (double *) malloc ((*lA)*sizeof(double));
    *Bx = (double *) malloc ((*lB)*sizeof(double));
    *By = (double *) malloc ((*lB)*sizeof(double));

    for (i = 0; i < (*lA) && nread == 2; i++)
        nread = scanf("%lf %lf", &(*Ax)[i], &(*Ay)[i]);

    for (i = 0; i < (*lB) && nread == 2; i++)
        nread = scanf("%lf %lf", &(*Bx)[i], &(*By)[i]);
}

/**
  * Main program to test the clip function.
  */
int main(void)
{
    double *Ax, *Ay, *Bx, *By, *polysx, *polysy, *alphaA, *alphaB;
    int lA, lB, status, nl, nlp, i, j, v, op, opind;
    int *origin, *ind0A, *ind1A, *ind0B, *ind1B, *lengths;
    char *msg;

    readFromStdin(&Ax, &Ay, &lA, &Bx, &By, &lB);

    for(opind=0; opind<5; opind++) {
        switch(opind) {
            case 0: msg = "POLYGON_UNION";        op = POLYGON_UNION;        break;
            case 1: msg = "POLYGON_INTERSECTION"; op = POLYGON_INTERSECTION; break;
            case 2: msg = "POLYGON_DIFF_AB";      op = POLYGON_DIFF_AB;      break;
            case 3: msg = "POLYGON_DIFF_BA";      op = POLYGON_DIFF_BA;      break;
            case 4: msg = "POLYGON_XOREXT";       op = POLYGON_XOREXT;       break;
        }
        clip(Ax, Ay, lA, Bx, By, lB, op,
             &polysx, &polysy, &lengths, &nl, &nlp, &status,
             &origin, &ind0A, &ind1A, &ind0B, &ind1B, &alphaA, &alphaB);

        v = 0;
        printf("---------OPERATION: %s---------\n",msg);
        for (i = 0; i < nl; i++) {
            printf("Polígono %d\n", i+1);
            printf("--------------------------\n");
            for (j = 0; j < lengths[i]; j++) {
                printf("x=%1.5f, y=%1.5f, o=%02d, alphaA=%1.5f, indA=(%+2d,%+2d), alphaB=%1.5f, indB=(%+2d,%+2d)\n",
                       polysx[v], polysy[v], origin[v],
                       alphaA[v], ind0A[v], ind1A[v],
                       alphaB[v], ind0B[v], ind1B[v]);
                v++;
            }
        }
        printf("nlp=%d status=%d\n",nlp,status);

        if(polysx != NULL) free(polysx);
        if(polysy != NULL) free(polysy);
        if(origin != NULL) free(origin);
        if(ind0A != NULL)  free(ind0A);
        if(ind1A != NULL)  free(ind1A);
        if(ind0B != NULL)  free(ind0B);
        if(ind1B != NULL)  free(ind1B);
        if(alphaA != NULL) free(alphaA);
        if(alphaB != NULL) free(alphaB);
    }

    free(Ax);
    free(Ay);
    free(Bx);
    free(By);

    return 0;
}
