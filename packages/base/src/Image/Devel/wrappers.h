#define GIMG(T,X) T * X##pSrc, int X##sstep, int X##sr1, int X##sr2, int X##sc1, int X##sc2
#define IMG(X) GIMG(unsigned char,X)
#define IMC(X) IMG(X)
#define IMF(X) GIMG(float,X)
#define P(X,r,c) (*(X##pSrc+(r)*X##sstep+(c)))
#define PF(X,r,c) (*(X##pSrc+(r)*X##sstep/4+(c)))
#define P3(X,r,c,p) (*(X##pSrc+(r)*X##sstep+(3*(c))+(p)))
#define TRAV(X,D,r,c) for (r=X##sr1+D; r<=X##sr2-D; r++) for(c=X##sc1+D; c<=X##sc2-D; c++)
#define MAX(a,b) ((a)>(b)?(a):(b))
#define MIN(a,b) ((a)<(b)?(a):(b))

