#define GIMG(T,X) T * X##p, int X##step, int X##r1, int X##r2, int X##c1, int X##c2
#define GIMS(T,X) T * X##p, int X##height, int X##width, int X##step, int X##r1, int X##r2, int X##c1, int X##c2

#define IMG(X) GIMG(unsigned char,X)
#define IMGSZ(X) GIMS(unsigned char,X)
#define P(X,r,c) (*(X##p+(r)*X##step+(c)))

#define IM1(X) IMG(X)
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

#define IPL(X,C) IplImage * ipl_##X = cvCreateImageHeader(cvSize(X##width,X##height), 8, C ); \
                 ipl_##X->imageData = X##p; \
                 ipl_##X->widthStep = X##step; \
                 cvSetImageROI(ipl_##X,cvRect(X##c1,X##r1,X##c2-X##c1+1,X##r2-X##r1+1));

typedef struct { double x1, y1, x2, y2; } TSegment;

typedef struct { int r, c, h, w; } TRect;

