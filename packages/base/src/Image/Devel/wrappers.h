#define GIMG(T,X) T * X##p, int X##step, int X##r1, int X##r2, int X##c1, int X##c2

#define IM1(X) GIMG(unsigned char,X)
#define P(X,r,c) (*(X##p+(r)*X##step+(c)))

#define IM2(X) IM1(X)
#define IM3(X) IM1(X)
#define PM(X,r,c,k) (*(X##p+(r)*X##step+(3*(c))+(k)))

#define IMF(X) GIMG(float,X)
#define PF(X,r,c) (*(X##p+(r)*X##step/4+(c)))

#define ROI(X,C) (X##p + X##r1*X##step/sizeof(*X##p) + X##c1*C)

#define TRAV(X,D,r,c) for (r=X##r1+D; r<=X##r2-D; r++) for(c=X##c1+D; c<=X##c2-D; c++)
#define TRAVR(X,r) for (r=X##r1; r<=X##r2; r++)
#define TRAVC(X,c) for(c=X##c1; c<=X##c2; c++)

#define MAX(a,b) ((a)>(b)?(a):(b))
#define MIN(a,b) ((a)<(b)?(a):(b))
#define CLIP(a) ((a)<0?0:(a)>255?255:(a))

