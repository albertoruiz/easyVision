#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#define TRUE  1
#define FALSE 0

#include "mycvUtils.h"

/* ------------------------------------------------------------------------ */
/* Funciones para  devolver un número entero o float aleatorio, entre dos   */
/* valores dados (ambos incluídos):                                         */ 
/* ------------------------------------------------------------------------ */
int mycvIntRand(int min,int max)
{
    if(min>=max) {
        printf("mycvIntRand: Parámetros incorrectos (min>=max)\n");
        exit(0);
    }
    return (min + (int)((max-min+1)*(rand()/(RAND_MAX+1.0))) );
}

float mycvFloatRand(float min,float max)
{
    if(min>=max) {
        printf("mycvFloatRand: Parámetros incorrectos (min>=max)\n");
        exit(0);
    }
    return (min + (float)((max-min)*(rand()/(RAND_MAX+1.0))) );
}


/* ------------------------------------------------------------------------ */
/*  Distancia de un punto (a,b) a la recta que pasa por (xi,yi) y (xf,yf):  */
/* ------------------------------------------------------------------------ */
float mycvDistPointLine(float xi,float yi, float xf, float yf,
                        float a, float b, int withsign)
{
   float dist;
   
   if((xf==xi)&&(yf==yi)) { /* No es una recta, sino un punto! */
       return FLOAT_INFINITY; 
   } else {
       dist = ((a-xi)*(yi-yf)+(b-yi)*(xf-xi))/
              sqrt((xf-xi)*(xf-xi)+(yf-yi)*(yf-yi));
       if(withsign) {
           return dist; 
       } else {
           return fabs(dist); 
        }
   }
}


/* ------------------------------------------------------------------------ */
/*   Distancia de un punto (a,b) al segmento que va de (xi,yi) a (xf,yf):   */
/* ------------------------------------------------------------------------ */
float mycvDistPointSegment(float xi,float yi, float xf, float yf,
                           float a, float b)
{    
    float d1_cuad,d2_cuad,d3_cuad,d3,d31,d31_cuad,result;
    
    d1_cuad = (xi-a)*(xi-a) + (yi-b)*(yi-b);
    d2_cuad = (xf-a)*(xf-a) + (yf-b)*(yf-b);
    d3_cuad = (xi-xf)*(xi-xf) + (yi-yf)*(yi-yf);
    d3 = sqrt(d3_cuad);
    if(d3<EPSILON) {
        return sqrt(d1_cuad);
    }
    d31_cuad = (-d2_cuad+d1_cuad+d3_cuad)*(-d2_cuad+d1_cuad+d3_cuad)/(4*d3_cuad);
    d31 = (-d2_cuad+d1_cuad+d3_cuad)/(2*d3);
    if(d31<=0){
        result = sqrt(d1_cuad);
    } else if (d3-d31<=0) {
        result = sqrt(d2_cuad);
    } else {
        result = sqrt(fabs(d1_cuad-d31_cuad));
    }
    return result;
} 

/* ------------------------------------------------------------------------ */
/* -----------  Distancia de un punto (a,b) al punto (x,y): --------------- */
/* ------------------------------------------------------------------------ */
float mycvDistPointPoint(float x,float y,float a, float b)
{
    return sqrt((x-a)*(x-a)+(y-b)*(y-b));    
}

/* ------------------------------------------------------------------------ */
/* ------------------ Producto vectorial de 2 vectores 3D: ---------------- */
/* ------------------------------------------------------------------------ */
void mycvCrossProduct(float x1,float y1,float z1,
                  float x2,float y2,float z2,
                  float *xres,float *yres,float *zres)
{
    *xres = y1*z2-y2*z1;
    *yres = x2*z1-x1*z2;
    *zres = x1*y2-x2*y1;
}

/* ------------------------------------------------------------------------ */
/* -------------------- Punto de cruce de dos segmentos: ------------------ */
/* ------------------------------------------------------------------------ */
void mycvGetCrossPoint(float x1,float y1,float x2,float y2,
                   float x3,float y3,float x4,float y4,
                   float *xout,float *yout,float *zout)
{
    float xint1,yint1,zint1,xint2,yint2,zint2,xint3,yint3,zint3;

    mycvCrossProduct(x1,y1,1.0,x2,y2,1.0,&xint1,&yint1,&zint1);
    mycvCrossProduct(x3,y3,1.0,x4,y4,1.0,&xint2,&yint2,&zint2);
    mycvCrossProduct(xint1,yint1,zint1,xint2,yint2,zint2,&xint3,&yint3,&zint3);
    if(fabs(zint3) > 0.0001) {
        *xout = xint3/zint3;
        *yout = yint3/zint3;
        *zout = 1.0;
    } else {
        //printf("WARNING: Cruce en el infinito\n");
        *xout = xint3;
        *yout = yint3;
        *zout = 0.0;
    }
}

/* ------------------------------------------------------------------------ */
/* -- Punto sobre el que cae la perpendicular de un punto a un segmento: -- */
/* ------------------------------------------------------------------------ */
void mycvPointPerpLine(float xi,float yi, float xf, float yf,
                       float a, float b, float *xout,float *yout)
{
   if((xf!=xi)||(yf!=yi)) { /* Chequeamos que sea una recta, no un punto. */
       *xout = (a*(xf-xi)*(xf-xi) + 
                (yf-yi)*(b*(xf-xi) + xi*yf -xf*yi))/
                ((xf-xi)*(xf-xi) + (yf-yi)*(yf-yi));
       *yout = (yf*((a-xi)*(xf-xi)+b*yf) - 
                ((a-xf)*(xf - xi) + 2*b*yf)*yi + b*yi*yi)/
                ((xf-xi)*(xf-xi) + (yf-yi)*(yf-yi));  
   }        
}

/* ------------------------------------------------------------------------ */
/* ----- Punto imagen de una homografía aplicada a un punto de entrada. --- */
/* ------------------------------------------------------------------------ */
void mycvCalcImgHomog(float Homog[3][3],float point1[3],float point2[3])
{
    int i,j;
    
    for(i=0;i<3;i++) {
        point2[i] = 0;
        for(j=0;j<3;j++) {
            point2[i] += Homog[i][j]*point1[j];
        }
    }
}    

/* Análogo al anterior, pero directamente con puntos 2D, y devuelve FALSE si
 * no pudo realizarse la transformación (punto en el infinito). */
int transf_pto_homog(float homog[3][3],int img_to_wrld,
                     float x,float y,float *xo,float *yo,
                     int rows, int cols)
{   
    float point1[3],point2[3];    

    if(img_to_wrld) {	
        point1[0] = x-cols/2;
	    point1[1] = rows/2-y;
    } else {
	    point1[0] = x;
	    point1[1] = y;    
    }	
    point1[2] = 1.0;
    mycvCalcImgHomog(homog,point1,point2);
    if(fabs(point2[2])<EPSILON) {
        return FALSE;
    }
    if(img_to_wrld) {
        *xo = point2[0]/point2[2];
        *yo = point2[1]/point2[2];
    } else {
        *xo = point2[0]/point2[2] + cols/2;
        *yo = -point2[1]/point2[2] + rows/2;    
    }

    return TRUE;    
}


/* ------------------------------------------------------------------------ */
/* ----- Puntos de corte de un segmento con un rectángulo de clipping. ---- */
/* ------------------------------------------------------------------------ */
int mycvGetClipPoints(float x1in,float y1in,float x2in,float y2in,
                   float xclipmin,float yclipmin,float xclipmax,float yclipmax,
                   float *x1out,float *y1out,float *x2out,float *y2out)
{
    float xcut,ycut,zcut;
    float xout[2],yout[2];
    int index;
    
    index = 0;
    /* Corte con línea superior del rectángulo: */
    mycvGetCrossPoint(x1in,y1in,x2in,y2in,
                  xclipmin,yclipmin,xclipmax,yclipmin,
                  &xcut,&ycut,&zcut);
    if((zcut!=0.0)&&(xclipmin<=xcut)&&(xcut<=xclipmax)) {
        xout[index] = xcut;
        yout[index] = ycut;
        index++;
    }

    /* Corte con línea inferior del rectángulo: */
    mycvGetCrossPoint(x1in,y1in,x2in,y2in,
                  xclipmin,yclipmax,xclipmax,yclipmax,
                  &xcut,&ycut,&zcut);
    if((zcut!=0.0)&&(xclipmin<=xcut)&&(xcut<=xclipmax)) {
        xout[index] = xcut;
        yout[index] = ycut;
        index++;
    }

    /* Corte con línea izquierda del rectángulo: */
    mycvGetCrossPoint(x1in,y1in,x2in,y2in,
                  xclipmin,yclipmin,xclipmin,yclipmax,
                  &xcut,&ycut,&zcut);
    if((zcut!=0.0)&&(yclipmin<=ycut)&&(ycut<=yclipmax)&&(index<2)) {
        xout[index] = xcut;
        yout[index] = ycut;
        index++;
    }

    /* Corte con línea derecha del rectángulo: */
    mycvGetCrossPoint(x1in,y1in,x2in,y2in,
                  xclipmax,yclipmin,xclipmax,yclipmax,
                  &xcut,&ycut,&zcut);
    if((zcut!=0.0)&&(yclipmin<=ycut)&&(ycut<=yclipmax)&&(index<2)) {
        xout[index] = xcut;
        yout[index] = ycut;
        index++;
    }

    if(index!=2) {
/*        printf("ERROR IMPOSIBLE en getClipPoints??!!\n");
        printf("x1in=%f, y1in=%f, x2in=%f, y2in=%f\n",x1in,y1in,x2in,y2in);
        printf("xclipmin=%f, yclipmin=%f, xclipmax=%f, yclipmax=%f\n",
               xclipmin,yclipmin,xclipmax,yclipmax);
        printf("x1out=%f, y1out=%f, x2out=%f, y2out=%f\n",
               *x1out,*y1out,*x2out,*y2out);
        printf("index=%d\n",index);       
        exit(1);*/
        return 0;
    } else {
        *x1out = xout[0];
        *y1out = yout[0];
        *x2out = xout[1];
        *y2out = yout[1];
        return 1;
    }    
}                   


/* Función que ajusta un cjto de puntos 2D a una línea con el método RANSAC. 
 * A la entrada, el parámetro support contiene el mínimo número de puntos
 * que serán considerados suficientes, y a la salida contendrá (en caso de
 * ajuste correcto) el tamaño real del conjunto de consenso. La función
 * devuelve un punto por el que pasa la recta, y un ángulo de orientación.
 */
int mycvRANSACFitLine2D(float *points2D,int numpoints, 
                        int *support,float dist_thres,
                        float *x1,float *x2,float *angle)
{ 
    /* OJO: PENDIENTE DE IMPLEMENTAR.*/
    printf("OJO: Función mycvRANSACFitLine2D pendiente de implementar.\n");
    exit(1);
    return 0;
}


/* Similar a la anterior, pero el primer punto se sabe que está en la recta.
 * Por eso se devuelve sólo un ángulo. Si el flag first0_max1 vale 0, devuel
 * ve la primera recta que encuentra que cumple la condición de soporte
 * mínimo. Si vale 1, entonces devuelve la de máximo soporte (es un poco
 * más lento): */
int mycvRANSACFitLine2D1Known(float *points2D,int numpoints,
                              int *support,float dist_thres,
                              float *angle,int first0_max1)
{
    int i,j,index,cur_supp,in_supp,max_supp;
    float a11,a12,a22,dist;

    if((numpoints<*support)||(numpoints<2)) {
        return 0;
    }    
    
    index = -1;
    max_supp = 0;
    in_supp = *support;
    for(i=1;i<numpoints;i++) {
        cur_supp = 1;
        for(j=1;j<numpoints;j++) {
            dist = mycvDistPointLine(points2D[0],points2D[1],
                                     points2D[2*i],points2D[2*i+1],
                                     points2D[2*j],points2D[2*j+1],0);
            if(dist<=dist_thres) {
                cur_supp++;
            }                         
            if(!first0_max1) {
                if(cur_supp>=in_supp) {
                    index = i;
                    goto sigue;
                }
            }
        }
        if((cur_supp>max_supp)&&(cur_supp>=in_supp)) {
            index = i;
            max_supp = cur_supp;
        }
    }

  sigue:    
    if(index==-1) { /* No hay soporte suficiente. */
        return 0;
    } else {
        a11 = 0.0;
        a12 = 0.0;
        a22 = 0.0;
        *support = 1;
        for(j=1;j<numpoints;j++) {
            dist = mycvDistPointLine(points2D[0],points2D[1],
                                     points2D[2*index],points2D[2*index+1],
                                     points2D[2*j],points2D[2*j+1],0);
            if(dist<=dist_thres) {
                a11 += (points2D[2*j]-points2D[0]) *
                       (points2D[2*j]-points2D[0]);
                a12 += (points2D[2*j]-points2D[0]) *
                       (points2D[2*j+1]-points2D[1]);              
                a22 += (points2D[2*j+1]-points2D[1]) *
                       (points2D[2*j+1]-points2D[1]);              
                (*support)++;
            }                         
        }     
        /* (No hace falta dividir los aij por N) */       
        if(fabs(a12)>EPSILON) {
            *angle = 
              atan2(1.0,(a11-a22+
                         sqrt(a11*a11+4*a12*a12-2*a11*a22+a22*a22))/(2*a12));
        } else {
            if(a11>=a22) {
                *angle = 0;
            } else {
                *angle = PI/2;
            }
        }    
        if(*angle < 0) {
            *angle += PI;        
        }
        return 1;
    }
}


int angulo_similar(float angle1,float angle2,float umbral)
{
    if((fabs(angle1-angle2)<umbral)||
       (fabs(fabs(angle1-angle2)-2*PI)<umbral)||
       (fabs(fabs(angle1-angle2)-PI)<umbral)) {
        return TRUE;
    } else {
        return FALSE;
    }
}

/* Pasa un ángulo al rango (-180,180], o (-PI,PI], dependiendo del flag
 * "engrados". */
float norm_angle(float angle1,int engrados)
{
    int temp;
    
    if(!engrados) {
        angle1 = angle1*180/M_PI;
    }
    if((angle1>=360.0)||(angle1<0.0)){
        temp = (int)floor(angle1/360.0);
        angle1 = angle1 - temp*360.0;
    } 
    if(angle1>180.0) {
        angle1 = angle1-360.0;
    }
    if(!engrados) {
        return angle1*M_PI/180;
    } else {
        return angle1;
    }
}

/* Devuelve el giro que debe darse para pasar de angle1 a angle2, en un
 * intervalo de (-180,180], o (-PI,PI] dependiendo del flag "engrados". 
 */
float incr_angle(float angle1,float angle2,int engrados)
{
    float incr;
    
    if(!engrados) {
        angle1 = angle1*180/M_PI;
        angle2 = angle2*180/M_PI;
    }    
    angle1 = norm_angle(angle1,TRUE);
    angle2 = norm_angle(angle2,TRUE);
    incr = angle2-angle1;
    incr = norm_angle(incr,TRUE);    
    if(!engrados) {
        return incr*M_PI/180;
    } else {
        return incr;
    }
}

/* Calcula la odometría incremental entre dos posiciones odométricas: */
void calc_odom_incr(float antodomx,float antodomy,float antodomtheta,
                    float odomx,float odomy,float odomtheta,
                    float *incodomx,float *incodomy,float *incodomtheta)
{                    
    float inc_x,inc_y,len,angulo_vectorxy_origen;
    float angulo_vectorxy_heading_1;    

    *incodomtheta = incr_angle(antodomtheta,odomtheta,0);
    inc_x = (odomx-antodomx);
    inc_y = (odomy-antodomy);
    len = sqrt(inc_x*inc_x+inc_y*inc_y);
    angulo_vectorxy_origen = atan2(inc_y,inc_x);    
    angulo_vectorxy_heading_1 = incr_angle(antodomtheta,angulo_vectorxy_origen,0);
    *incodomx = len*cos(angulo_vectorxy_heading_1);
    *incodomy = len*sin(angulo_vectorxy_heading_1);
}                               
/* El antiguo. Creo que funciona mal.
  void calc_odom_incr(float antodomx,float antodomy,float antodomtheta,
                    float odomx,float odomy,float odomtheta,
                    float *incodomx,float *incodomy,float *incodomtheta)
{                    
    float inc_x,inc_y,len,angulo_vectorxy_origen;
    float angulo_vectorxy_heading_1,angulo_hipotesis_vectorxy;    

    *incodomtheta = incr_angle(antodomtheta,odomtheta,FALSE);
    inc_x = (odomx-antodomx);
    inc_y = (odomy-antodomy);
    len = sqrt(inc_x*inc_x+inc_y*inc_y);
    angulo_vectorxy_origen = atan2(inc_y,inc_x);    
    angulo_vectorxy_heading_1 = incr_angle(antodomtheta,angulo_vectorxy_origen,FALSE);
    angulo_hipotesis_vectorxy = incr_angle(angulo_vectorxy_heading_1,antodomtheta,FALSE);
    *incodomx = len*sin(angulo_hipotesis_vectorxy);
    *incodomy = len*cos(angulo_hipotesis_vectorxy);
}  */


int proyectar_punto(float x,float y,float z,float mat_proy[3][4],
                    float *xs, float *ys,int rows,int cols)
{
    int i,j;
    float pin[4],pout[3];

    pin[0] = x; 
    pin[1] = y;
    pin[2] = z;
    pin[3] = 1.0;
    
    for(i=0;i<3;i++) {
        pout[i] = 0.0;
        for(j=0;j<4;j++) {
            pout[i] += mat_proy[i][j]*pin[j];
        }
    }

    if(fabs(pout[2])>EPSILON) {
        *xs = pout[0]/pout[2]+cols/2;    
        *ys = rows/2-pout[1]/pout[2];
        if((*xs>=0)&&(*xs<=cols)&&(*ys>=0)&&(*ys<=rows)) {
            return TRUE;
        }
    } 
    return FALSE;
}


void corrige_punto_3D_con_odom(float *x,float *y,float *z,
                               float incodomx,float incodomy,float incodomtheta)
{
    float nx,ny,nxr,nyr;

    /* Corregimos sólo las X e Y del punto (la Z no cambia): */
    nx = *x-incodomx;
    ny = *y-incodomy;
    nxr = cos(incodomtheta)*nx+sin(incodomtheta)*ny;
    nyr = -sin(incodomtheta)*nx+cos(incodomtheta)*ny;
    *x = nxr;
    *y = nyr;
}    
