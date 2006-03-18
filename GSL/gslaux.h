#define DVEC(A) int A##n, double*A##p
#define CVEC(A) int A##n, double*A##p
#define DMAT(A) int A##r, int A##c, double* A##p
#define CMAT(A) int A##r, int A##c, double* A##p 

int constant(double val, DVEC(r));

int vectorZip(int code, DVEC(a), DVEC(b), DVEC(r));        
        
int vector_scale(double alpha, DVEC(x), DVEC(r));
        
int vector_offset(double offs, DVEC(x), DVEC(r));

double toScalar(int code, DVEC(x));
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

int eigensystem(DMAT(x),DVEC(l),DMAT(v));

int eigensystemC(CMAT(x),DVEC(l),CMAT(v));

int qr(DMAT(x),DMAT(q),DMAT(r));

int chol(DMAT(x),DMAT(l));
    
int fft(int code, CVEC(a), CVEC(b));
    
int integrate_qng(double f(double, void*), double a, double b, double prec,
                   double *result, double*error);
    
int integrate_qags(double f(double,void*), double a, double b, double prec, int w, 
               double *result, double* error);

int polySolve(DVEC(a), CVEC(z));

int matrix_fscanf(char*filename, DMAT(a));
