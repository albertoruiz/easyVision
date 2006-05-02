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

int vectorZip(int code, KDVEC(a), KDVEC(b), DVEC(r));        
        
int vector_scale(double alpha, KDVEC(x), DVEC(r));
        
int vector_offset(double offs, KDVEC(x), DVEC(r));

int toScalar(int code, KDVEC(x), DVEC(r));
/* norm2, absdif, maximum, posmax, etc. */

int vectorMap(int code, KDVEC(x), DVEC(r));
/* sin cos tan etc. */
    
int multiplyR(KDMAT(a),KDMAT(b),DMAT(r)); 
int multiplyC(KCMAT(a),KCMAT(b),CMAT(r));
        
int luSolveR(KDMAT(a),KDMAT(b),DMAT(r)); 
int luSolveC(KCMAT(a),KCMAT(b),CMAT(r));
        
int take_diagonal(KDMAT(a),DVEC(d));

int take_diagonalC(KCMAT(a),CVEC(d));
            
int luRaux(KDMAT(a),DVEC(b));

int luCaux(KCMAT(a),CVEC(b));

int trans(KDMAT(x),DMAT(t));

int transC(KCMAT(x),CMAT(t));

int submatrixR(int r1, int r2, int c1, int c2, KDMAT(x),DMAT(r));
                
int diagR(KDVEC(d),DMAT(r));  
int diagC(KCVEC(d),CMAT(r));   
    
int svd(KDMAT(x),DMAT(u), DVEC(s),DMAT(v));

int eigensystemR(KDMAT(x),DVEC(l),DMAT(v));

int eigensystemC(KCMAT(x),DVEC(l),CMAT(v));

int QR(KDMAT(x),DMAT(q),DMAT(r));

int chol(KDMAT(x),DMAT(l));
    
int fft(int code, KCVEC(a), CVEC(b));
    
int integrate_qng(double f(double, void*), double a, double b, double prec,
                   double *result, double*error);
    
int integrate_qags(double f(double,void*), double a, double b, double prec, int w, 
               double *result, double* error);

int polySolve(KDVEC(a), CVEC(z));

int matrix_fscanf(char*filename, DMAT(a));

int minimize(double f(int, double*), double tolsize, int maxit, 
                 KDVEC(xi), KDVEC(sz), DMAT(sol));

int minimizeWithDeriv(double f(int, double*), void df(int, double*, double*),
                      double initstep, double minimpar, double tolgrad, int maxit, 
                      KDVEC(xi), DMAT(sol));

int mesh(KDMAT(x));

int deriv(int code, double f(double, void*), double x, double h, double * result, double * abserr);

double gsl_sf_erf(double);
double gsl_sf_erf_Z(double);

int gsl_sf_bessel_J0_e(double, double*); // hmmm...
int gsl_sf_exp_e10_e(double, double*);   // HMMMMM...
