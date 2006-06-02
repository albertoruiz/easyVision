
int ippiSet_32f_C1R(float,void*,int,double);
int ippiImageJaehne_32f_C1R(void*,int,double);
int ippiFilterGauss_32f_C1R(void*,int,void*,int,double,int);
int ippiCopy_32f_C1R(void*,int,void*,int,double);
int ippiCopy_8u_C1R(void*,int,void*,int,double);


int mycvOpenCamera(char *filename);
void mycvSetModeCamera(int camera,int mode,int rows,int cols);
char* mycvGetFrameCamera(int camera,int *step_bytes);
