#ifndef _MYCV_UTILS_H
#define _MYCV_UTILS_H

#ifdef __cplusplus
extern "C" {
#endif

/* Constantes: */
#define INT_INFINITY   2147483647
#define FLOAT_INFINITY 3.4E38
#define EPSILON        10E-6
#define PI             3.14159265358979323846

/* Macros funciones elementales: */
#define abs(a)  (((a)>0)?(a):(-(a)))
#define maximum(a,b)  (((a)>(b))?(a):(b))
#define minimum(a,b)  (((a)<(b))?(a):(b))
#define signo(v) (((v)>0)?1:-1)

/* Macros acceso a imagen, dado el puntero al comienzo, la posición (i,j) y el
 * step entre líneas: */
#define ACCESS_IMG_8U(img,i,j,step) \
         (*((unsigned char*)((img)+step*(i)+(j))))
#define ACCESS_IMG_8U_C2(img,i,j,k,step) \
 (*((unsigned char*)((img)+step*(i)+2*(j)+(k))))
#define ACCESS_IMG_8U_C3(img,i,j,k,step) \
 (*((unsigned char*)((img)+step*(i)+3*(j)+(k))))
#define ACCESS_IMG_8U_C4(img,i,j,k,step) \
 (*((unsigned char*)((img)+step*(i)+4*(j)+(k))))
#define ACCESS_IMG_32S_C1(img,i,j,step) \
        (*((int*)((img)+step*(i)+4*(j))))

/* Prototipos de funciones: */
/* ------------------------------------------------------------------------ */
/* Funciones para  devolver un número entero o float aleatorio, entre dos   */
/* valores dados (ambos incluídos):                                         */ 
/* ------------------------------------------------------------------------ */
int mycvIntRand(int min,int max);
float mycvFloatRand(float min,float max);

/* ------------------------------------------------------------------------ */
/*  Distancia de un punto (a,b) a la recta que pasa por (xi,yi) y (xf,yf):  */
/* ------------------------------------------------------------------------ */
float mycvDistPointLine(float xi,float yi, float xf, float yf,
                        float a, float b, int withsign);

/* ------------------------------------------------------------------------ */
/*   Distancia de un punto (a,b) al segmento que va de (xi,yi) a (xf,yf):   */
/* ------------------------------------------------------------------------ */
float mycvDistPointSegment(float xi,float yi, float xf, float yf,
                           float a, float b);

/* ------------------------------------------------------------------------ */
/* -----------  Distancia de un punto (a,b) al punto (x,y): --------------- */
/* ------------------------------------------------------------------------ */
float mycvDistPointPoint(float x,float y,float a, float b);

/* ------------------------------------------------------------------------ */
/* ------------------ Producto vectorial de 2 vectores 3D: ---------------- */
/* ------------------------------------------------------------------------ */
void mycvCrossProduct(float x1,float y1,float z1,
                  float x2,float y2,float z2,
                  float *xres,float *yres,float *zres);

/* ------------------------------------------------------------------------ */
/* -------------------- Punto de cruce de dos segmentos: ------------------ */
/* ------------------------------------------------------------------------ */
void mycvGetCrossPoint(float x1,float y1,float x2,float y2,
                   float x3,float y3,float x4,float y4,
                   float *xout,float *yout,float *zout);

/* ------------------------------------------------------------------------ */
/* -- Punto sobre el que cae la perpendicular de un punto a un segmento: -- */
/* ------------------------------------------------------------------------ */
void mycvPointPerpLine(float xi,float yi, float xf, float yf,
                       float a, float b, float *xout,float *yout);

/* ------------------------------------------------------------------------ */
/* ----- Punto imagen de una homografía aplicada a un punto de entrada. --- */
/* ------------------------------------------------------------------------ */
void mycvCalcImgHomog(float Homog[3][3],float point1[3],float point2[3]);
/* Análogo al anterior, pero directamente con puntos 2D, y devuelve FALSE si
 * no pudo realizarse la transformación (punto en el infinito). */
int transf_pto_homog(float homog[3][3],int img_to_wrld,
                     float x,float y,float *xo,float *yo,
                     int rows,int cols);

/* ------------------------------------------------------------------------ */
/* ----- Puntos de corte de un segmento con un rectángulo de clipping. ---- */
/* ------------------------------------------------------------------------ */
int mycvGetClipPoints(float x1in,float y1in,float x2in,float y2in,
                   float xclipmin,float yclipmin,float xclipmax,float yclipmax,
                   float *x1out,float *y1out,float *x2out,float *y2out);

/* Función que ajusta un cjto de puntos 2D a una línea con el método RANSAC. 
 * A la entrada, el parámetro support contiene el mínimo número de puntos
 * que serán considerados suficientes, y a la salida contendrá (en caso de
 * ajuste correcto) el tamaño real del conjunto de consenso. La función
 * devuelve un punto por el que pasa la recta, y un ángulo de orientación.
 */
/* OJO: PENDIENTE DE IMPLEMENTAR.*/
int mycvRANSACFitLine2D(float *points2D,int numpoints, 
                        int *support,float dist_thres,
                        float *x1,float *x2,float *angle);

/* Similar a la anterior, pero el primer punto se sabe que está en la recta.
 * Por eso se devuelve sólo un ángulo. Si el flag first0_max1 vale 0, devuel
 * ve la primera recta que encuentra que cumple la condición de soporte
 * mínimo. Si vale 1, entonces devuelve la de máximo soporte (es un poco
 * más lento): */
int mycvRANSACFitLine2D1Known(float *points2D,int numpoints,
                              int *support,float dist_thres,
                              float *angle,int first0_max1);

/* Mira si dos ángulos son similares hasta un umbral, teniendo en cuenta que
 * ang+PI = ang. */
int angulo_similar(float angle1,float angle2,float umbral);

/* Pasa un ángulo al rango (-180,180], o (-PI,PI], dependiendo del flag
 * "engrados". */
float norm_angle(float angle1,int engrados);

/* Devuelve el giro que debe darse para pasar de angle1 a angle2, en un
 * intervalo de (-180,180], o (-PI,PI] dependiendo del flag "engrados". 
 */
float incr_angle(float angle1,float angle2,int engrados);

/* Calcula la odometría incremental entre dos posiciones odométricas: */
void calc_odom_incr(float antodomx,float antodomy,float antodomtheta,
                    float odomx,float odomy,float odomtheta,
                    float *incodomx,float *incodomy,float *incodomtheta);

/* Proyecta un punto de 3D a 2D. Devuelve FALSE si el punto proyectado cae
 * en el infinito: */
int proyectar_punto(float x,float y,float z,float mat_proy[3][4],
                    float *xs, float *ys,int rows,int cols);

/* Corrige un punto 3D tras un movimiento odométrico: */                         void corrige_punto_3D_con_odom(float *x,float *y,float *z,
                               float incodomx,float incodomy,
                               float incodomtheta);

#ifdef __cplusplus
}
#endif

#endif /* _MYCV_UTILS_H */
