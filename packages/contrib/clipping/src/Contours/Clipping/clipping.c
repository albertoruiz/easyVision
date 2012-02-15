#include "clipping.h"
#include <stdio.h>

#define DISCARD_INTER(px, py, qx, qy, minX, minY, maxX, maxY) \
                (( ((((px) < (minX)) << 3) | (((px) > (maxX)) << 2) | (((py) < (minY)) << 1) | ((py) > (maxY))) & \
                  ((((qx) < (minX)) << 3) | (((qx) > (maxX)) << 2) | (((qy) < (minY)) << 1) | ((qy) > (maxY))) ) != 0 )

#define MIN(a,b) ( (a) < (b) ? (a) : (b) )
#define MAX(a,b) ( (a) > (b) ? (a) : (b) )


/**
  * Create a new linked lyst from floating points values pointed by @polygon
  * of size @n and return it into a pointer pointed by @l
  */
void createList(double *polygon, int n, struct vertex **l)
{
    int i;
    struct vertex *list = (struct vertex *) malloc (sizeof(struct vertex));
    struct vertex *prev = NULL;
    struct vertex *current = list;

    for (i = 0; i < ((n-1) << 1); i+=2) {
        current->x = polygon[i];
        current->y = polygon[i+1];
        current->prev = prev;
        current->next = (struct vertex *) malloc (sizeof(struct vertex));
        current->nextVertex = current->next;
        prev = current;
        current = current->next;
    }

    current->x = polygon[i];
    current->y = polygon[i+1];
    current->prev = prev;
    current->next = list;
    current->nextVertex = list;
    list->prev = current;

    *l = list;
}

/**
  * Delete linked-list
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


// TODO: add perturbations in case alphaP or alphaQ = 0
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
            *alphaP = wec_p1/(wec_p1 - wec_p2);
            *alphaQ = wec_q1/(wec_q1 - wec_q2);
            return 1;
        }
    }

    return 0;
}

/**
  * Find and add intersection points between @lclip and @lsubject
  */
int findIntersections(struct vertex *lclip, struct vertex *lsubject)
{
    double a, b;
    int intersections = 0;
    struct vertex *aux, *v, *w, *sort;
    for (v = lsubject; v; v = v->nextVertex == lsubject ? NULL : v->nextVertex)
        for (w = lclip; w; w = w->nextVertex == lclip ? NULL : w->nextVertex)
            if (!DISCARD_INTER(v->x, v->y, v->nextVertex->x, v->nextVertex->y,
                                MIN(w->x, w->nextVertex->x), MIN(w->y, w->nextVertex->y), 
                                MAX(w->x, w->nextVertex->x), MAX(w->y, w->nextVertex->y))
                && intersect(v, v->nextVertex, w, w->nextVertex, &a, &b))
            {
                // create intersection points
                struct vertex *i1 = 
                    (struct vertex *) malloc (sizeof(struct vertex));
                struct vertex *i2 = 
                    (struct vertex *) malloc (sizeof(struct vertex));

                intersections++;

                i1->alpha = a;
                i2->alpha = b;
                i2->x = i1->x = w->x + b * (w->nextVertex->x - w->x);
                i2->y = i1->y = w->y + b * (w->nextVertex->y - w->y);

                i1->intersect = 1;
                i2->intersect = 1;
                i1->processed = 0;
                i2->processed = 0;
                
                // link intersection points
                i1->neighbour = i2;
                i2->neighbour = i1;

                // sort intersection points into polygons
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
            }

    return intersections;
}

/**
  * Determine whether point @p is inside of polygon @polygon or not
  */
int isInside(struct vertex *p, struct vertex *polygon)
{
    int oddNodes = 0;
    const double x = p->x;
    const double y = p->y;
    struct vertex *node1, *node2, *q;

    for (q = polygon; q; q = q->nextVertex == polygon ? NULL : q->nextVertex)
    {
        node1 = q;
        
        if (q->next)
            node2 = q->next;
        else
            node2 = polygon;

        if ((node1->y < y && node2->y >= y) ||
            (node2->y < y && node1->y >= y))
        {
            if (node1->x + (y - node1->y)/
                (node2->y - node1->y) * (node2->x - node1->x) < x)
                oddNodes = !oddNodes;
        }
    }

    return oddNodes;
}

/**
  * Mark intersection points in polygon @p 
  * depending on whether it's an entry or exit point
  * with respect to polygon @q
  */
void markEntries(struct vertex *p, struct vertex *q)
{
    int status;

    if (isInside(p, q))
        status = STATUS_EXIT;
    else
        status = STATUS_ENTRY;

    struct vertex *pi;
    for (pi = p->next; pi != p; pi = pi->next)
    {
        if (pi->intersect)
        {
            pi->entry_exit = status;
            status = !status;
        }
    }
}

/**
  * Adds a new polygon to a previous one pointed by @lastPoly
  * with starting vertex @p
  */
struct vertex * newPolygon(struct vertex *lastPoly, struct vertex *p)
{
    struct vertex *poly = (struct vertex *) malloc (sizeof(struct vertex));
    poly->x = p->x;
    poly->y = p->y;
    poly->nextPoly = NULL;
    
    if (lastPoly)
        lastPoly->nextPoly = poly;
    return poly;

}

/**
  * Adds new vertex to a previous one pointed by @last
  */
void newVertex(struct vertex *last, struct vertex *p)
{
    struct vertex *point = 
        last->next = (struct vertex *) malloc (sizeof(struct vertex));
    point->x = p->x;
    point->y = p->y;

    point->next = NULL;
    
}


/**
  * Intersect the subject polygon using clip polygon and store into
  * polygons structure, returns the total number of polygons.
  */
int createClippedPolygon(struct vertex *lclip, struct vertex *lsubject, 
            struct vertex **polygons, int *total)
{
    struct vertex *isubject = lsubject->next, *current;
    struct vertex *poly = NULL, *first = NULL;
    int npolys = 0;
    int nvertex = 0;

    while (isubject)
    {
        for (; isubject != lsubject && !(isubject->intersect && !isubject->processed); 
                isubject = isubject->next);

        if (isubject == lsubject) 
            break;
        
        isubject->processed = 1;
        current = isubject;
        
        if (first == NULL) 
        {
            first = (struct vertex *) malloc (sizeof(struct vertex));
            first->x = current->x;
            first->y = current->y;
            first->nextPoly = NULL;
            poly = first;
        }
        else
            poly = newPolygon(poly, current);
            
            
        npolys++;
        nvertex++;
        
        struct vertex *lastPoint = poly;
        
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
  * Copy the polygons structure into an array of double-precision floats
  * containing the coordinates of each point from the resulting clipped polygon
  * return parameters are:
  * @polys: array of vertex for all the polygons
  * @lengths: array of lengths for each polygon
  */
void copy(struct vertex *polygons, int npolys, int nvertex, double **polys, int **lengths)
{
    double *ps = (double *) malloc (nvertex * 2 * sizeof(double));
    int *ls = (int *) malloc (npolys * sizeof(int));
    int polycount = 0, vertexcount = 0;
    struct vertex *ipoly, *ivertex;

    for (ipoly = polygons; ipoly; ipoly = ipoly->nextPoly)
    {
        ls[polycount] = 0;
        for (ivertex = ipoly; ivertex; ivertex = ivertex->next)
        {
            ls[polycount]++;
            ps[vertexcount++] = ivertex->x;
            ps[vertexcount++] = ivertex->y;
        }
        polycount++;
    }
    *polys = ps;
    *lengths = ls;
}


// Perform clipping of the polygon clip with nc points against 
// a subject with ns points. Returns a set of nl polygons with specified lengths
// in an array of coordinates polys.
int clip(double *clip, int nc, double *subject, int ns, double **polys, int **lengths, int *nl)
{
    struct vertex *lclip, *lsubject;
    struct vertex *polygons = NULL;
    int nvertex, npolys;

    printf("entering clip\n");

    int k;
    printf("clip (%d)\n",nc);
    for (k=0; k<2*nc; k++) {
        printf("%f\n",clip[k]);
    }
    printf("subject (%d)\n",ns);
    for (k=0; k<2*ns; k++) {
        printf("%f\n",subject[k]);
    }

    //exit(0);
    // create data structures
    createList(clip, nc, &lclip);
    createList(subject, ns, &lsubject);

    printf("lists created\n");


    // phase one of the algorithm
    findIntersections(lclip, lsubject);


    printf("intersections found\n");

    // phase two of the algorithm
    markEntries(lclip, lsubject);
    markEntries(lsubject, lclip);

    printf("entries created\n");

    // phase three of the algorithm
    npolys = createClippedPolygon(lclip, lsubject, &polygons, &nvertex);

    printf("created clipped polygon\n");

    // copy polygons into polys array
    copy(polygons, npolys, nvertex, polys, lengths);
    *nl = npolys;

    printf("copied to output array\n");

    // free memory
    deleteList(lclip);
    deleteList(lsubject);
    deletePolygons(polygons);

    printf("memory freed and exit\n");

    return 0;
}

void readFromStdin(double **vclip, double **vsubject, int *lclip, int *lsubject)
{
    scanf("%d %d\n", lclip, lsubject);
    int i;
    double *clip = (double *) malloc ((*lclip) *2*sizeof(double));
    double *subject = (double *) malloc ((*lsubject) *2*sizeof(double));

    for (i = 0; i < (*lclip) << 1; i+=2)
        scanf("%lf %lf", &clip[i], &clip[i+1]);

    for (i = 0; i < (*lsubject) << 1; i+=2)
        scanf("%lf %lf", &subject[i], &subject[i+1]);

    *vclip = clip;
    *vsubject = subject;

}


int main_clipping(void)
{
    double *clipp, *subject;
    int lclip, lsubject;

    readFromStdin(&clipp, &subject, &lclip, &lsubject);

    double *polys;
    int *lengths, nl, i,j;
    clip(clipp, lclip, subject, lsubject, &polys, &lengths, &nl);
    int v = 0;
    for (i = 0; i < nl; i++) {
        printf("Polígono %d\n", i+1);
        printf("--------------------------\n");
        for (j = 0; j < lengths[i]; j++) {
            printf("x=%.5f, y=%.5f\n", polys[v], polys[v+1]);
            v+=2;
        }
    }
    
    free(clipp);
    free(subject);
    return 0;
}

