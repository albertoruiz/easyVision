#define DVEC(A) int A##n, double*A##p
#define CVEC(A) int A##n, double*A##p
#define DMAT(A) int A##r, int A##c, double* A##p
#define CMAT(A) int A##r, int A##c, double* A##p
 
// const pointer versions for the parameters 
#define KDVEC(A) int A##n, const double*A##p
#define KCVEC(A) int A##n, const double*A##p
#define KDMAT(A) int A##r, int A##c, const double* A##p
#define KCMAT(A) int A##r, int A##c, const double* A##p 

int constant(double val, DVEC(r));

int vectorZip(int code, DVEC(a), DVEC(b), DVEC(r));        
        
int vector_scale(double alpha, DVEC(x), DVEC(r));
        
int vector_offset(double offs, DVEC(x), DVEC(r));

int toScalar(int code, KDVEC(x), DVEC(r));
/* norm2, absdif, maximum, posmax, etc. */

int vectorMap(int code, DVEC(x), DVEC(r));
/* sin cos tan etc. */
    
int multiplyR(DMAT(a),DMAT(b),DMAT(r)); 
int multiplyC(CMAT(a),CMAT(b),CMAT(r));
        
int luSolveR(DMAT(a),DMAT(b),DMAT(r)); 
int luSolveC(CMAT(a),CMAT(b),CMAT(r));
        
int take_diagonal(DMAT(a),DVEC(d));

int take_diagonalC(CMAT(a),CVEC(d));
            
int luRaux(DMAT(a),DVEC(b));

int luCaux(CMAT(a),CVEC(b));

int trans(DMAT(x),DMAT(t));

int transC(CMAT(x),CMAT(t));

int submatrixR(int r1, int r2, int c1, int c2, DMAT(x),DMAT(r));
                
int diagR(DVEC(d),DMAT(r));  
int diagC(CVEC(d),CMAT(r));   
    
int svd(DMAT(x),DMAT(u), DVEC(s),DMAT(v));

int eigensystemR(KDMAT(x),DVEC(l),DMAT(v));

int eigensystemC(KCMAT(x),DVEC(l),CMAT(v));

int qr(KDMAT(x),DMAT(q),DMAT(r));

int chol(KDMAT(x),DMAT(l));
    
int fft(int code, CVEC(a), CVEC(b));
    
int integrate_qng(double f(double, void*), double a, double b, double prec,
                   double *result, double*error);
    
int integrate_qags(double f(double,void*), double a, double b, double prec, int w, 
               double *result, double* error);

int polySolve(DVEC(a), CVEC(z));

int matrix_fscanf(char*filename, DMAT(a));

int minimize(double f(int, double*), double tolsize, int maxit, 
             DVEC(xi), DVEC(sz), DVEC(sol)); 

int mesh(KDMAT(x));
