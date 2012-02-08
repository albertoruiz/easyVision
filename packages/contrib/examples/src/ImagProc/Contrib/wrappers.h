#define IMG(X) unsigned char * X##pSrc, int X##sstep, int X##sr1, int X##sr2, int X##sc1, int X##sc2
#define IMF(X) float * X##pSrc, int X##sstep, int X##sr1, int X##sr2, int X##sc1, int X##sc2
#define P(X,r,c) (*(X##pSrc+(r)*X##sstep+(c)))
#define TRAV(X,D,r,c) for (r=X##sr1+D; r<=X##sr2-D; r++) for(c=X##sc1+D; c<=X##sc2-D; c++)
#define MAX(a,b) ((a)>(b)?(a):(b))
#define MIN(a,b) ((a)<(b)?(a):(b))

