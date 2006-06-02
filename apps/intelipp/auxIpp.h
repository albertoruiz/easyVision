
int ippiSet_32f_C1R(float,void*,int,double);
int ippiImageJaehne_32f_C1R(void*,int,double);
int ippiFilterGauss_32f_C1R(void*,int,void*,int,double,int);
int ippiFilterSobelVert_32f_C1R(void*,int,void*,int,double);
int ippiFilterSobelHoriz_32f_C1R(void*,int,void*,int,double);
int ippiCopy_32f_C1R(void*,int,void*,int,double);
int ippiCopy_8u_C1R(void*,int,void*,int,double);
int ippiScale_8u32f_C1R(void*,int,void*,int,double,float,float);
int ippiScale_32f8u_C1R(void*,int,void*,int,double,float,float);
int ippiAbs_32f_C1R(void*,int,void*,int,double);
int ippiAdd_32f_C1R(void*,int,void*,int,void*,int,double);
int ippiFilterMax_32f_C1R(void*,int,void*,int,double,double,double);
int ippiCompare_32f_C1R(void*,int,void*,int,void*,int,double,int);

int mycvOpenCamera(char *filename);
void mycvSetModeCamera(int camera,int mode,int rows,int cols);
char* mycvGetFrameCamera(int camera,int *step_bytes);
