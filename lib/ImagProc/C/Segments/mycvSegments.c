#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include <ipp.h>

#include "mycvUtils.h"
#include "mycvTablesEdgels.h"
#include "mycvRunnings.h"

#include "mycvSegments.h" 

/* Umbrales para el agrupado en el postprocesado de segmentos: */
#define THRES_ANGLE       M_PI/15
#define THRES_DIST_LINE   4 /*10*/
#define THRES_DIST_SEG    6 /*6*/

/* Constante auxiliar para ir generando sitio para segmentos de 1000 en 1000
 * durante la operaci� del algoritmo (pero luego se hace un realloc para
 * ajustar sin desperdiciar espacio):*/
#define CHUNK_segments 1000

#define TRUE  1
#define FALSE 0

typedef struct {
    float x1,y1,x2,y2; /* extremos 2D del segmento */
    unsigned char gray_izq,gray_der;
    unsigned char r_izq,r_der,g_izq,g_der,b_izq,b_der;    
    float length; /* Suma de longitudes de segmentos que lo forman. */
    float cx,cy,sxx,sxy,syy;
} TSegmentoGlobal; /* Tipo de datos interno para postprocesar segmentos. */

/* ----------------- Funciones auxiliares locales del m�ulo: --------------- */

/* Funci� para ordenar l�eas por nmero de puntos: */
int compara_num_points(const void *a, const void *b)
{
    int tam_a,tam_b;
    
    tam_a = ((TSegment*)(a))->num_points;
    tam_b = ((TSegment*)(b))->num_points;
    return (tam_b-tam_a);
}

/* Funci� para calcular saliencia alrededor de un punto, y el �gulo de
 * m�ima respuesta: */
inline void calc_resps(Ipp8u *img,int step,int yc,int xc,int user_radius,
                       float *f_resp,int *index,unsigned char thres_low)
{
    int k,l,m;
    unsigned char value;
    float max_resp;
    
    for(k=0;k<4*user_radius;k++) {
        f_resp[k]=0.0;
    }
    for(k=-user_radius;k<=user_radius;k++) {
        for(l=-user_radius;l<=user_radius;l++) {
            value = ACCESS_IMG_8U(img,yc+k,xc+l,step);
            /* S�o influyen pixels superiores al umbral inferior:*/
            if(value >= thres_low) {
                m = 0;
                while(tableEdgelLineMaskXY[k+user_radius]
                        [l+user_radius][m].index != -1) {
                    f_resp[tableEdgelLineMaskXY[k+user_radius]
                             [l+user_radius][m].index] += 
                      value *
                      tableEdgelLineMaskXY[k+user_radius]
                        [l+user_radius][m].value; 
                    m++;
                }
            }
        }
    }
    max_resp = f_resp[0];
    *index = 0;
    for(k=1;k<4*user_radius;k++) {
        if(f_resp[k]>max_resp) {
            max_resp = f_resp[k];
            *index = k;
        }
    }    
}


/* Funci� que calcula la mediana de una distribuci� de nivel de gris (0-255)
 * a partir del histograma. Funciona recorri�do el histograma linealmente 
 * hasta que se supera una acumulaci� parcial del 50%. Entonces se coge el
 * elemento que exactamente divide la distribuci� en dos masas iguales (pon-
 * derando, si hace falta). 
 * Los par�etros de entrada son la suma total de todas las posiciones del
 * histograma y el array de 256 posiciones con el histograma en s�*/
unsigned char median_from_histogram(int sum_total,int *histo)
{
    int ult_pos,k,median,acum;
    
    ult_pos = -1;
    if(sum_total > 0){
        k = 0; 
        acum = 0;               
        while (1) {
            if(k==256) {
                printf("PARADOJA 1!!!\n");
                exit(0);
            }
            acum += histo[k];
            if(acum >= sum_total/2+1) {
                break;
            }
            if(histo[k]>0) {
                ult_pos = k;
            }
            k++;
        }
        if(ult_pos==-1) { 
            /* S�o con el primer elemento positivo ya se supera la mitad de
             * la distribuci�: La mediana es justo esta posici�. */
            median = k;
        } else if(sum_total%2) { 
            /* Masa total es impar: la mediana cae justo en el lugar en el
             * que se ha detectado que se superaba el 50%. */
            median = k;                        
        } else { 
            /* Masa total es par: La mediana puede caer en el lugar o
             * a medias entre �te ltimo y el anterior. Depende de si
             * se super�el 50% justo en la ltima posici�, o en la
             * posici� intermedia entre �ta y la anterior. */
            if(acum==sum_total/2+1) {
                median = (k+ult_pos)/2;
            } else {
                median = k;                        
            }
        }
    } else {
        median = 0; /* Convenio para histogramas vac�s; */
    }
    
    return ((unsigned char) median);
}


/* Similar a la funci� anterior, pero en vez de la mediana devuelve la media,
 * ponderando cada posici� con su valor de histograma: */
unsigned char mean_from_histogram(int sum_total,int *histo)
{
    int i,sum,acum;

    if(sum_total==0) {
        return 0; /* Convenio id�tico al anterior, para histogramas vac�s. */
    }
    
    sum = 0;
    acum = 0;
    for(i=0;i<256;i++) {
         sum += i*histo[i];
         acum += histo[i];
    }
    
    if(sum_total != acum) {
        printf("PARADOJA3\n");
        exit(0);
    }
    
    return (unsigned char) (sum/acum);

}

/* ------------------- Funciones principales del m�ulo: -------------------- */

/* Funci� que calcula segmentos con precisi� subp�el, a partir de una 
 * imagen de entrada de 1 canal (gris), de tipo byte. Hay dos versiones, la
 * primera para poder definir todos los par�etros de operaci�, y una segunda
 * m� c�oda que utiliza un buen conjunto de par�etros por defecto. Calcula
 * tambi� un valor de gris mediano a ambos lados de cada segmento. */
void mycvSegmentsWithParms_8u_C1_C3(Ipp8u *imgIn,int stepIn, 
                                    int x1,int y1, int roiSize_w, int roiSize_h,
                                    TSegment **segments,int *num_segments,
                                    int user_radius,float user_width_edgel,
                                    int med_siz,
                                    unsigned char thres_high,
                                    unsigned char thres_low,
                                    int one_channel)
/*void mycvSegmentsWithParms(IplImage *imageIn,IplImage *imageInGray,
                                   TSegment **segments,int *num_segments,
                                   int user_radius,float user_width_edgel,
                                   unsigned char thres_high,
                                   unsigned char thres_low)*/
{
    IppiSize roiSize = {roiSize_w,roiSize_h};
    int i,j,k,l,y,x,xc,yc,x_desp,y_desp,last_x,last_y;
    int x2,y2,margin,count_added,stepHipass,stepSmooth,stepGray,oldStepIn=0;
    int index,new_index,num_points_line,dir,try_segments;
    int intwidthline,runLength,runWidth,width_side,width_cen;
    unsigned char value;
    int valuegray,value_r=0,value_g=0,value_b=0;
    int r_izq=0,r_der=0,g_izq=0,g_der=0,b_izq=0,b_der=0;
    struct {int x,y;} extr[2]; /* Extremos de cada cluster. */
    float f_resp[4*MAXRADIUS];
    float sum,sumx,sumy,sumxx,sumxy,sumyy;
    float x_1_f,y_1_f,x_2_f,y_2_f,x_cen_f,y_cen_f;
    float factor_perp,angle,a11,a12,a22,temp,cosangle,sinangle,desv_perp;
    TRunPos **runLine;
    int histo_izq[256],histo_der[256],count_histo_izq,count_histo_der;
    int histo_izq_r[256],histo_der_r[256];
    int histo_izq_g[256],histo_der_g[256];
    int histo_izq_b[256],histo_der_b[256];
    unsigned char gray_izq,gray_der;
    IppiSize auxRoiSize,medianMaskSize={med_siz,med_siz};
    IppiPoint medianMaskPoint={med_siz/2,med_siz/2};
    Ipp8u *imgSmooth,*imgHipass,*imgGray=NULL,*oldImgIn=NULL;    
    
    /* Control de errores: */
    if((user_radius>MAXRADIUS)||((user_radius<0))) {
        printf("ERROR: mycvSegments: radius out of bounds\n");
        exit(1);
    }
    
    /* Inicializamos las tablas de edgels (s�o consume tiempo si es la primera 
     * llamada, o si se cambian los valores del radio o anchura de edgel con 
     * respecto a llamadas anteriores): */
    mycvInitTablesEdgels(user_radius,user_width_edgel);
    user_radius = mycvGetRadiusEdgels();
    user_width_edgel = mycvGetWidthEdgels();
    
    /* Esquina inferior derecha del ROI: */
    x2 = x1+roiSize.width-1;
    y2 = y1+roiSize.height-1;        

    /* El margen ser�del triple de lo necesario, por seguridad (por si alguna
     * l�ea, localmente, poco a poco, se va haciendo curva, y su recorrido
     * a lo ancho es mayor que el radio de usuario (ver c�igo m� adelante): */
    margin = 3*user_radius; 

    /* OJO: FALTA comprobar si se da un roi con margen suficiente para no
     * salirnos en los filtros (med_size + 1(highpass)). 
     *
     *  ...
     *
     */
        
    /* Creamos las im�enes suavizada y de bordes, con margen suficiente.
     * Tambi� la de grises si es necesaria: */
    imgSmooth = ippiMalloc_8u_C1(roiSize.width + 2*margin,
                                 roiSize.height + 2*margin,
                                 &stepSmooth);
    imgHipass = ippiMalloc_8u_C1(roiSize.width + 2*margin,
                                 roiSize.height + 2*margin,
                                 &stepHipass);    
    if(!one_channel) {
        imgGray = ippiMalloc_8u_C1(roiSize.width + 2*margin,
                                   roiSize.height + 2*margin,
                                   &stepGray);
        auxRoiSize.width = roiSize.width + 2*margin;
        auxRoiSize.height = roiSize.height + 2*margin;
        ippiRGBToGray_8u_C3C1R((Ipp8u*)(imgIn)-margin*stepIn-3*margin,stepIn,
                               (Ipp8u*)(imgGray),stepGray,
                                auxRoiSize);
        /* Sustituimos la de entrada por la de grises, para que el resto del
         * c�igo valga para ambos tipos: */
        oldImgIn = imgIn;
        imgIn = imgGray + margin*stepGray + margin;
        oldStepIn = stepIn;
        stepIn = stepGray;
    }
    
    if((NULL == imgSmooth) || (NULL == imgHipass)){
        printf("mycvSegments: No se pueden crear las im�enes intermedias\n");
        exit(1);
    }

    /* Pasamos filtro de mediana y pasa-alta. Pero antes, si es imagen a color,
     * calculamos una de grises: */
    auxRoiSize.width = roiSize.width + 2;
    auxRoiSize.height = roiSize.height + 2;
    ippiFilterMedian_8u_C1R((Ipp8u*)(imgIn)-stepIn-1,stepIn,
                  	        (Ipp8u*)(imgSmooth)+(margin-1)*stepSmooth+margin-1,
                            stepSmooth,
                            auxRoiSize,medianMaskSize,medianMaskPoint);
    auxRoiSize.width = roiSize.width;
    auxRoiSize.height = roiSize.height;
    ippiFilterHipass_8u_C1R((Ipp8u*)(imgSmooth)+margin*stepSmooth+margin,
                            stepSmooth,
                  	        (Ipp8u*)(imgHipass)+margin*stepHipass+margin,
                            stepHipass,
                            roiSize,ippMskSize3x3);
                             
    /* Ponemos el bit menos significativo de los p�eles hipass a cero (marca 
     * de no visitado): */
    auxRoiSize.width = roiSize.width;
    auxRoiSize.height = roiSize.height;
    ippiAndC_8u_C1IR(0xfe,(Ipp8u*)(imgHipass+margin*stepHipass+margin),
                                   stepHipass,auxRoiSize);

    auxRoiSize.width = roiSize.width + 2*margin;
    auxRoiSize.height = roiSize.height + 2*margin;         
    /* Sitio para el array de l�eas (posiblemente despu� se aumentar�: */
    *num_segments = 0;
    try_segments = 1;
    *segments = (TSegment*)malloc(CHUNK_segments*sizeof(TSegment));

    /* Buscamos un pixel superior al umbral alto, y nos movemos alrededor
     * hasta encontrar un pixel sin vecinos mayores que � (siempre sobre
     * pixels an no visitados): */    
    for(i=margin;i<roiSize.height+margin;i++) {
        for(j=margin;j<roiSize.width+margin;j++){        
            if((ACCESS_IMG_8U(imgHipass,i,j,stepHipass) >= thres_high) && 
               !(ACCESS_IMG_8U(imgHipass,i,j,stepHipass)&0x01)) {
                yc = i;
                xc = j;
                /* Buscamos m�imo local (pixel que no tenga vecino mayor),
                 * movi�donos alrededor: */
again:          for(k=-1;k<=1;k++) {
                    for(l=-1;l<=1;l++) {
                        if((yc+k >= margin) && 
                           (yc+k < auxRoiSize.height - margin) &&
                           (xc+l >= margin) && 
                           (xc+l < auxRoiSize.width - margin)) {
                            if(!(ACCESS_IMG_8U(imgHipass,
                                               yc+k,xc+l,stepHipass)&0x01)) {
                                if(ACCESS_IMG_8U(imgHipass,
                                                 yc+k,xc+l,stepHipass) > 
                                   ACCESS_IMG_8U(imgHipass,yc,xc,stepHipass)) {
                                    yc = yc+k;
                                    xc = xc+l;
                                    goto again;
                                }                             
                            }
                        }    
                    }
                }                    

                /* Calculamos la direcci� de m�ima respuesta a m�caras
                 * lineales locales, centrados en este m�imo local: */
                calc_resps(imgHipass,stepHipass,yc,xc,user_radius,
                           f_resp,&index,thres_low);
                           
                /* Iniciamos los estad�ticos de la l�ea: */
                num_points_line = 0;
                sum = 0.0;
                sumx = 0.0;
                sumy = 0.0;
                sumxx = 0.0;
                sumyy = 0.0;
                sumxy = 0.0;

                /* Una vez tenemos la direcci� de m�ima respuesta en index,
                 * comenzamos a recorrer desde el centro, capturando pixels 
                 * en ambas direcciones, con la misma alineaci�. Cada pixel
                 * capturado es marcado (impar) para no ser visitado m�,
                 * y se tiene en cuenta (pesado con su valor) para calcular
                 * los momentos xx, yy y xy: */
                for(dir=0;dir<2;dir++) {
                    last_x = xc;
                    last_y = yc;
                    extr[dir].x = xc;
                    extr[dir].y = yc;
                    while(1) {
                        k = 0;
                        count_added = 0;
                        while(!tableEdgelLineIndex[index][dir][k].marcafin) {
                            y_desp = tableEdgelLineIndex[index][dir][k].i;
                            x_desp = tableEdgelLineIndex[index][dir][k].j;
                            y = extr[dir].y+y_desp;
                            x = extr[dir].x+x_desp;
                            value = ACCESS_IMG_8U(imgHipass,y,x,stepHipass);
                            if( (value >= thres_low) && (!(value&0x01)) ){
                                /* Estad�ticos: */
                                num_points_line++;
                                sum += value;
                                sumx += value*x;
                                sumy += value*y;
                                sumxx += value*x*x;
                                sumyy += value*y*y;
                                sumxy += value*x*y;
                                /* Marca de visitado: */
                                ACCESS_IMG_8U(imgHipass,y,x,stepHipass) |= 0x01;
                                /* Seguimos a�diendo: */
                                count_added++;
                                /* Ultimo a�dido: */
                                last_x = x;
                                last_y = y;
                            }    
                            k++;
                        }
                        /* Nuevo extremo: */
                        extr[dir].x = last_x; 
                        extr[dir].y = last_y;                       
                        
                        if(count_added < user_radius*2/3) { 
                            /* No hay suficiente soporte en esa direcci�;
                             * por aqu�hemos terminado. */
                            break; 
                        } else {
                            /* S�hab� algn soporte. Antes de seguir, vemos
                             * si la saliencia local sigue siendo en una 
                             * direcci� similar: (Aqu�usamos 2*user_radius/3, 
                             * que viene a ser unos 30 grados). Si no,
                             * tambi� cortamos: 
                             */
                            calc_resps(imgHipass,stepHipass,
                                       extr[dir].y,extr[dir].x,
                                       user_radius,f_resp,&new_index,thres_low);
                            if( (abs(new_index - index)>2*user_radius/3) &&
                                 (abs(new_index - index)<
                                  4*user_radius-2*user_radius/3) ) {
                                break;
                            }  
                        } 
                    }
                } /* End for(dir). */

                /* El cluster est�terminado. Calculamos todos sus 
                 * par�etros: */
                /* Centro del segmento: */
                x_cen_f = sumx/sum;
                y_cen_f = sumy/sum;

                /* Angulo: */
                a11 = sumxx/sum - x_cen_f*x_cen_f;
                a12 = sumxy/sum - x_cen_f*y_cen_f;
                a22 = sumyy/sum - y_cen_f*y_cen_f;
                temp = sqrt(a11*a11+4*a12*a12-2*a11*a22+a22*a22); 
                if(fabs(a12)>EPSILON) {                        
                    angle = atan2(1.0,
                      (a11-a22+temp) / (2*a12));
                } else {
                    if(a11>=a22) {
                        angle = 0;
                    } else {
                        angle = PI/2;
                    }
                }    
                if(angle < 0) {
                    angle += PI;        
                }                          
                cosangle = cos(angle);
                sinangle = sin(angle);

                /* Desviaci� t�ica en la direcci� perpendicular: */
                desv_perp = sqrt(fabs(a11+a22-temp)/2.0);

                /* Extremos sub_pixel (por proyecci�): */
                temp = ((float)extr[0].x-x_cen_f)*cosangle + 
                       ((float)extr[0].y-y_cen_f)*sinangle;
                x_1_f = temp*cosangle + x_cen_f;
                y_1_f = temp*sinangle + y_cen_f;
                temp = ((float)extr[1].x-x_cen_f)*cosangle + 
                       ((float)extr[1].y-y_cen_f)*sinangle;
                x_2_f = temp*cosangle + x_cen_f;
                y_2_f = temp*sinangle + y_cen_f; 

                /* El ancho del recorrido de la l�ea es el radio de la 
                 * m�cara, como m�imo, o dos veces la desviaci� t�ica 
                 * en direcci� perpendicular al segmento, si este ltimo
                 * valor es mayor: */
                factor_perp = 1.0/maximum(fabs(sinangle),fabs(cosangle));
                intwidthline = 
                    (int)rint(factor_perp*maximum(desv_perp*2,user_radius));
                /* Por seguridad (para evitar accesos fuera del buffer):*/
                if(intwidthline > margin) {
                    intwidthline = margin;
                }

                /* Creamos el recorrido de la l�ea: */
                runLine = createRunLine((int)rint(x_1_f),(int)rint(y_1_f),
                                        (int)rint(x_2_f),(int)rint(y_2_f),
                                        intwidthline,&runWidth,&runLength);

                /* Recorriendo los alrededores de la l�ea eliminamos
                 * posibles restos no capturados de la l�ea, al mismo
                 * tiempo que calculamos el histograma de grises a un
                 * lado y otro del segmento. */

                /* Dividimos el recorrido (a lo ancho del segmento)
                 * en 3 �eas: Izquierda, central y derecha. Las �eas
                 * laterales tendr� de ancho (int) runWidth/3, y la
                 * central el mismo valor m� el resto (0,1,2). La zona 
                 * central se elimina del muestreo de grises, por resultar
                 * a veces inestable.
                 */
                width_side = runWidth/3;
                width_cen = runWidth/3 + runWidth%3;

                /* Inicio de los histogramas izquierdo y derecho, y del
                 * conteo total de ambos (masa total): */
                for(k=0;k<256;k++) {
                    histo_izq[k] = 0;
                    histo_der[k] = 0;
                    if(!one_channel) {
                        histo_izq_r[k] = 0;
                        histo_der_r[k] = 0;
                        histo_izq_g[k] = 0;
                        histo_der_g[k] = 0;
                        histo_izq_b[k] = 0;
                        histo_der_b[k] = 0;
                    }
                    count_histo_izq = 0;
                    count_histo_der = 0;
                }

                /* Recorrido transversal del segmento (primero a lo ancho, 
                 * luego a lo largo): */ 
                for(k=0;k<runLength;k++) {
                    for(l=0;l<runWidth;l++) {
                        y = runLine[l][k].y;
                        x = runLine[l][k].x;
                        /* Si el punto en el recorrido del entorno rectangular
                         * del segmento cae dentro de los l�ites, tomamos
                         * nota del valor de gris; en otro caso, colocamos
                         * una marca de -1: */
                        if((x-margin>=0)&&(x-margin<=x2-x1)&&
                           (y-margin>=0)&&(y-margin<=y2-y1)) {
                           valuegray = 
                            (int)ACCESS_IMG_8U(imgIn,y-margin,x-margin,stepIn);
                            if(!one_channel) {
                                value_r = 
                                   (int)ACCESS_IMG_8U_C3(oldImgIn,y-margin,
                                                         x-margin,0,oldStepIn);
                                value_g = 
                                   (int)ACCESS_IMG_8U_C3(oldImgIn,y-margin,
                                                         x-margin,1,oldStepIn);
                                value_b = 
                                   (int)ACCESS_IMG_8U_C3(oldImgIn,y-margin,
                                                         x-margin,2,oldStepIn);
                            }                                               
                        } else {
                           valuegray = -1;                        
                        }
                        if(l<width_side) { 
                            /* Area izquierda: */
                            if(valuegray!=-1) {
                                histo_izq[valuegray]++;
                                count_histo_izq++;
                                if(!one_channel) {                            
                                    histo_izq_r[value_r]++;
                                    histo_izq_g[value_g]++;
                                    histo_izq_b[value_b]++;
                                }
                            }
                        } else if(l<width_side+width_cen) {
                            /* Marca de visitado: */
                            ACCESS_IMG_8U(imgHipass,y,x,stepHipass) |= 0x01;
                        } else {
                            /* Area derecha: */
                            if(valuegray!=-1) {
                                histo_der[valuegray]++;
                                count_histo_der++;
                                if(!one_channel) {
                                    histo_der_r[value_r]++;
                                    histo_der_g[value_g]++;
                                    histo_der_b[value_b]++;
                                }
                            }                            
                        }
                    }
                }

                /* C�culo de la mediana de gris de los lados: .*/
                gray_izq = median_from_histogram(count_histo_izq,histo_izq);
                gray_der = median_from_histogram(count_histo_der,histo_der);

                if(!one_channel) {
                    r_izq = median_from_histogram(count_histo_izq,histo_izq_r);
                    r_der = median_from_histogram(count_histo_der,histo_der_r);
                    g_izq = median_from_histogram(count_histo_izq,histo_izq_g);
                    g_der = median_from_histogram(count_histo_der,histo_der_g);
                    b_izq = median_from_histogram(count_histo_izq,histo_izq_b);
                    b_der = median_from_histogram(count_histo_der,histo_der_b);
                }
                                    
                /* Liberamos el recorrido: */
                freeRunLine(runLine,intwidthline); 

                /* Y escribimos los campos de la l�ea: */
                (*segments)[(*num_segments)].num_points = num_points_line;
                (*segments)[(*num_segments)].cx = 
                      (x_1_f + x_2_f)/2 - margin + x1;
                (*segments)[(*num_segments)].cy = 
                      (y_1_f + y_2_f)/2 - margin + y1;
                (*segments)[(*num_segments)].angle = 
                    atan2(y_2_f-y_1_f,x_2_f-x_1_f) + PI;
                (*segments)[(*num_segments)].desv_perp = desv_perp;
                (*segments)[(*num_segments)].length = 
                    sqrt( (x_1_f-x_2_f)*(x_1_f-x_2_f) +
                          (y_1_f-y_2_f)*(y_1_f-y_2_f) );
                (*segments)[(*num_segments)].x1 = x_1_f - margin + x1;
                (*segments)[(*num_segments)].y1 = y_1_f - margin + y1;
                (*segments)[(*num_segments)].x2 = x_2_f - margin + x1;
                (*segments)[(*num_segments)].y2 = y_2_f - margin + y1;
                (*segments)[(*num_segments)].gray_izq = gray_izq;
                (*segments)[(*num_segments)].gray_der = gray_der;

                if(!one_channel) {
                    (*segments)[(*num_segments)].r_izq = r_izq;
                    (*segments)[(*num_segments)].r_der = r_der;
                    (*segments)[(*num_segments)].g_izq = g_izq;
                    (*segments)[(*num_segments)].g_der = g_der;
                    (*segments)[(*num_segments)].b_izq = b_izq;
                    (*segments)[(*num_segments)].b_der = b_der;
                } else {
                    (*segments)[(*num_segments)].r_izq = gray_izq;
                    (*segments)[(*num_segments)].r_der = gray_der;
                    (*segments)[(*num_segments)].g_izq = gray_izq;
                    (*segments)[(*num_segments)].g_der = gray_der;
                    (*segments)[(*num_segments)].b_izq = gray_izq;
                    (*segments)[(*num_segments)].b_der = gray_der;
                }
                
                /* Incrementamos las l�eas, y si es necesario, reservamos
                 * m� memoria: */
                (*num_segments)++;
                if((*num_segments) == try_segments*CHUNK_segments) {
                    try_segments++;
                    *segments = (TSegment*) realloc(*segments,
                                 try_segments*CHUNK_segments*sizeof(TSegment));
                }
                /* Para seguir por donde �amos, antes de quedarnos con el
                 * m�imo local: 
                 */
                j--; 
            } 
        }
    } 

    /* En el array segments tenemos todas las lineas encontradas. Las ordenamos 
     * por nmero de pixels. */
    qsort(*segments,*num_segments,sizeof(TSegment),compara_num_points);
    
    /* Eliminamos espacio sobrante: */
    *segments = (TSegment*) realloc(*segments,(*num_segments)*sizeof(TSegment)); 

    /* Finalmente, borramos las imagenes intermedias: */
    ippiFree(imgSmooth);
    ippiFree(imgHipass);
    if(!one_channel) {
        ippiFree(imgGray);
    }

}                     


/* Funci� con par�etros por defecto: */
/*inline void mycvSegments(IplImage *imageIn,IplImage *imageInGray,
                                 TSegment **segments,int *num_segments)
{
    mycvSegmentsWithParms(imageIn,imageInGray,segments,num_segments,
                                  5,1.5,20,10);
}*/




/* Funci� que a�de a un segmento global (representado por un centro (cx,cy) y
 * una matriz de covarianza (sxx,sxy,syy) acumulados, con una longitud (lenght)
 * tambi� acumulada, un nuevo segmento (x1b,y1b)-(x2b,y2b). (Es una funci�
 * auxiliar, no exportada) */
void unir_a_segmento_global(TSegmentoGlobal *seg_global,
			    float x1b,float y1b,float x2b,float y2b)
{
    float cxa,cya,cxb,cyb,cxr,cyr,la,lb,x1a,y1a,x2a,y2a;
    float sxxa,sxya,syya,sxxb,sxyb,syyb,sxxr,sxyr,syyr;
    float temp,angle,cosangle,sinangle,dist,distmax,xp[4],yp[4];
    int i,j,imax,jmax,inter;
    
    x1a = seg_global->x1;
    y1a = seg_global->y1;
    x2a = seg_global->x2;
    y2a = seg_global->y2;                
    la = seg_global->length;
    cxa = seg_global->cx;
    cya = seg_global->cy;
    sxxa = seg_global->sxx;
    sxya = seg_global->sxy;
    syya = seg_global->syy;
    
    lb = sqrt((x1b-x2b)*(x1b-x2b)+(y1b-y2b)*(y1b-y2b));               
    cxb = (x1b+x2b)/2;
    cyb = (y1b+y2b)/2;
    sxxb = (x1b-x2b)*(x1b-x2b)/12;
    sxyb = (x1b-x2b)*(y1b-y2b)/12;
    syyb = (y1b-y2b)*(y1b-y2b)/12;

    cxr = (cxa*la + cxb*lb)/(la + lb);
    cyr = (cya*la + cyb*lb)/(la + lb);

    sxxr = ((cxa-cxr)*(cxa-cxr)*la + (cxb-cxr)*(cxb-cxr)*lb)/(la + lb);
    sxyr = ((cxa-cxr)*(cya-cyr)*la + (cxb-cxr)*(cyb-cyr)*lb)/(la + lb);
    syyr = ((cya-cyr)*(cya-cyr)*la + (cyb-cyr)*(cyb-cyr)*lb)/(la + lb);
    sxxr += (sxxa*la+sxxb*lb)/(la+lb); 
    sxyr += (sxya*la+sxyb*lb)/(la+lb); 
    syyr += (syya*la+syyb*lb)/(la+lb); 

    temp = sqrt(sxxr*sxxr+4*sxyr*sxyr-2*sxxr*syyr+syyr*syyr); 
    if(fabs(sxyr)>EPSILON) {                        
        angle = atan2(1.0,(sxxr-syyr+temp) / (2*sxyr));
    } else {
	if(sxxr>=syyr) {
	   angle = 0;
	} else {
	   angle = M_PI/2;
	}
	if(angle < 0) {
            angle += M_PI;        
	}       
    }                   
    cosangle = cos(angle);
    sinangle = sin(angle);

    /* Extremos (por proyecci�): */
    temp = (x1a-cxr)*cosangle + (y1a-cyr)*sinangle;
    xp[0] = temp*cosangle + cxr;
    yp[0] = temp*sinangle + cyr;

    temp = (x2a-cxr)*cosangle + (y2a-cyr)*sinangle;
    xp[1] = temp*cosangle + cxr;
    yp[1] = temp*sinangle + cyr; 
    
    temp = (x1b-cxr)*cosangle + (y1b-cyr)*sinangle;
    xp[2] = temp*cosangle + cxr;
    yp[2] = temp*sinangle + cyr;
    
    temp = (x2b-cxr)*cosangle + (y2b-cyr)*sinangle;
    xp[3] = temp*cosangle + cxr;
    yp[3] = temp*sinangle + cyr; 
    
    distmax = 0;
    imax = -1;
    jmax = -1;
    for(i=0;i<4;i++) {
        for(j=i+1;j<4;j++) {
            dist = sqrt((xp[i]-xp[j])*(xp[i]-xp[j])+(yp[i]-yp[j])*(yp[i]-yp[j]));
	    if(dist > distmax) {
	    	distmax = dist;
		imax = i;
		jmax = j;
	    }
	}    
    }
    
    if((imax==-1)||(jmax==-1)) {
        printf("PARADOJA en unir_a_segmento_global!!\n");
        exit(1);
    }

    if(mycvDistPointPoint(x1a,y1a,xp[imax],yp[imax]) >
       mycvDistPointPoint(x1a,y1a,xp[jmax],yp[jmax])) {
        inter = imax;
        imax = jmax;        
        jmax = inter;
    }
    
    seg_global->x1 = xp[imax];
    seg_global->y1 = yp[imax];
    seg_global->x2 = xp[jmax];
    seg_global->y2 = yp[jmax];
    seg_global->length = la + lb; 
    seg_global->cx = cxr;
    seg_global->cy = cyr;
    seg_global->sxx = sxxr;
    seg_global->sxy = sxyr;
    seg_global->syy = syyr;
}


/* Id�tica a la anterior, pero no recomputa la orientaci� cada vez, sino 
 * que siempre conserva la del primer segmento (que deber� ser el m� largo).
 */
void unir_a_segmento_global_exper(TSegmentoGlobal *seg_global,
                                  float x1b,float y1b,float x2b,float y2b)

{
    float cxr,cyr,la,lb,x1a,y1a,x2a,y2a;
    float sxxr,sxyr,syyr;
    float temp,angle,cosangle,sinangle,dist,distmax,xp[4],yp[4];
    int i,j,imax,jmax,inter;
    
    x1a = seg_global->x1;
    y1a = seg_global->y1;
    x2a = seg_global->x2;
    y2a = seg_global->y2;                
    la = seg_global->length;
    cxr = seg_global->cx;
    cyr = seg_global->cy;
    sxxr = seg_global->sxx;
    sxyr = seg_global->sxy;
    syyr = seg_global->syy;
    
    lb = sqrt((x1b-x2b)*(x1b-x2b)+(y1b-y2b)*(y1b-y2b));               

    temp = sqrt(sxxr*sxxr+4*sxyr*sxyr-2*sxxr*syyr+syyr*syyr); 
    if(fabs(sxyr)>EPSILON) {                        
        angle = atan2(1.0,(sxxr-syyr+temp) / (2*sxyr));
    } else {
	if(sxxr>=syyr) {
	   angle = 0;
	} else {
	   angle = M_PI/2;
	}
	if(angle < 0) {
            angle += M_PI;        
	}       
    }                   
    cosangle = cos(angle);
    sinangle = sin(angle);

    /* Extremos (por proyecci�): */
    temp = (x1a-cxr)*cosangle + (y1a-cyr)*sinangle;
    xp[0] = temp*cosangle + cxr;
    yp[0] = temp*sinangle + cyr;

    temp = (x2a-cxr)*cosangle + (y2a-cyr)*sinangle;
    xp[1] = temp*cosangle + cxr;
    yp[1] = temp*sinangle + cyr; 
    
    temp = (x1b-cxr)*cosangle + (y1b-cyr)*sinangle;
    xp[2] = temp*cosangle + cxr;
    yp[2] = temp*sinangle + cyr;
    
    temp = (x2b-cxr)*cosangle + (y2b-cyr)*sinangle;
    xp[3] = temp*cosangle + cxr;
    yp[3] = temp*sinangle + cyr; 
    
    distmax = 0;
    imax = -1;
    jmax = -1;
    for(i=0;i<4;i++) {
        for(j=i+1;j<4;j++) {
            dist = sqrt((xp[i]-xp[j])*(xp[i]-xp[j])+(yp[i]-yp[j])*(yp[i]-yp[j]));
	    if(dist > distmax) {
	    	distmax = dist;
		imax = i;
		jmax = j;
	    }
	}    
    }
    
    if((imax==-1)||(jmax==-1)) {
        printf("PARADOJA en unir_a_segmento_global!!\n");
        exit(1);
    }

    if(mycvDistPointPoint(x1a,y1a,xp[imax],yp[imax]) >
       mycvDistPointPoint(x1a,y1a,xp[jmax],yp[jmax])) {
        inter = imax;
        imax = jmax;        
        jmax = inter;
    }
    
    seg_global->x1 = xp[imax];
    seg_global->y1 = yp[imax];
    seg_global->x2 = xp[jmax];
    seg_global->y2 = yp[jmax];
    seg_global->length = la + lb;
}


int compara_length(const void *a, const void *b)
{
    int tam_a,tam_b;
    
    tam_a = (int)((TSegment*)(a))->length;
    tam_b = (int)((TSegment*)(b))->length;
    return (tam_b-tam_a);
}


void mycvPostProcessSegments(TSegment **segments,
                             int *numsegments,
                             float minlength,float maxlengthtodel,
                             unsigned char mincontrast)
{
    int k,l,num_segs_glob,*candidatos,numcandidatos,antiguonumsegments;
    int *captados,*presionados,repetir;
    TSegmentoGlobal *segs_glob;
    TSegment *segmentsnew;
    float x1s,y1s,x2s,y2s,x1g,y1g,x2g,y2g;    
    
    if(*numsegments==0) { /* No hay nada que hacer, volvemos */
        return;
    }
    
    segs_glob = (TSegmentoGlobal *)
                   malloc((*numsegments)*sizeof(TSegmentoGlobal));    
    captados =(int*)malloc((*numsegments)*sizeof(int));
    presionados =(int*)malloc((*numsegments)*sizeof(int));
    candidatos =(int*)malloc((*numsegments)*sizeof(int));
    
    for(k=0;k<*numsegments;k++) {
        if((abs((*segments)[k].gray_izq-(*segments)[k].gray_der) <
            mincontrast)&&((*segments)[k].length<maxlengthtodel)) {
           /* No da el contraste y no es mayor que maxlengthtodel => 
            * se elimina, marc�dolo como captado. */
            captados[k] = TRUE;
        } else {
            captados[k] = FALSE;
        }
        presionados[k] = FALSE;
    }

    num_segs_glob = 0;
    for(k=0;k<*numsegments;k++) {
        if(!captados[k]) {
            if(((*segments)[k].length >= minlength) || (presionados[k])){
                /* S�o los segmentos largos (o los presionados por estar 
                 * alineados con largos) generan hip�esis de segmento
                 * nuevo. Creamos un nuevo segmento global: */
    	    	captados[k] = TRUE;
    	        x1g = (*segments)[k].x1;
    	        y1g = (*segments)[k].y1;
    	        x2g = (*segments)[k].x2;
	            y2g = (*segments)[k].y2;

                segs_glob[num_segs_glob].x1 = x1g;
                segs_glob[num_segs_glob].y1 = y1g;
                segs_glob[num_segs_glob].x2 = x2g;
                segs_glob[num_segs_glob].y2 = y2g;

                segs_glob[num_segs_glob].length = (*segments)[k].length;

                segs_glob[num_segs_glob].gray_izq = (*segments)[k].gray_izq;
                segs_glob[num_segs_glob].gray_der = (*segments)[k].gray_der;
                segs_glob[num_segs_glob].r_izq = (*segments)[k].r_izq;
                segs_glob[num_segs_glob].r_der = (*segments)[k].r_der;
                segs_glob[num_segs_glob].g_izq = (*segments)[k].g_izq;
                segs_glob[num_segs_glob].g_der = (*segments)[k].g_der;
                segs_glob[num_segs_glob].b_izq = (*segments)[k].b_izq;
                segs_glob[num_segs_glob].b_der = (*segments)[k].b_der;

	            segs_glob[num_segs_glob].cx = (x1g + x2g)/2;    
	            segs_glob[num_segs_glob].cy = (y1g + y2g)/2;    
	            segs_glob[num_segs_glob].sxx = (x1g-x2g)*(x1g-x2g)/12;
	            segs_glob[num_segs_glob].sxy = (x1g-x2g)*(y1g-y2g)/12;
	            segs_glob[num_segs_glob].syy = (y1g-y2g)*(y1g-y2g)/12;

                /* Ahora buscamos candidatos a unir con este segmento. */
                numcandidatos = 0;
                for(l=k+1;l<*numsegments;l++) {
                    if(captados[l]) {
                        /* Los ya captados no entran a unirse. */
                        continue;
                    }
                    if(angulo_similar((*segments)[k].angle,(*segments)[l].angle,
		       THRES_ANGLE)) {
                        /* S�o hacemos cuentas si tienen �gulos similares: */
    		        x1s = (*segments)[l].x1;
    		        y1s = (*segments)[l].y1;
    		        x2s = (*segments)[l].x2;
    		        y2s = (*segments)[l].y2;
                        if((mycvDistPointLine(x1g,y1g,x2g,y2g,x1s,y1s,0)<THRES_DIST_LINE) &&  
                           (mycvDistPointLine(x1g,y1g,x2g,y2g,x2s,y2s,0)<THRES_DIST_LINE)) {
                            /* Est� alineados. Como poco, ya
                             * metemos presi� para iniciar una nueva 
                             * hip�esis. */
                            presionados[l] = TRUE;
                            /* Tambi� a�dimos a la lista actual de 
                             * candidatos a unir al segmento global
                             * (pero s�o se a�dir� si tambi� est�
                             * cerca los extremos. Esto se har�despu�,
                             * para todos los candidatos juntos. */
                            candidatos[numcandidatos] = l; 
                            numcandidatos++;                                  
                        }                        
                    }
                }
                
                /* Ahora iteramos sobre los candidatos, para ir uniendo al
                 * segmento global. */
                do {
                    repetir = FALSE;
                    for(l=0;l<numcandidatos;l++) {
    		        x1s = (*segments)[candidatos[l]].x1;
    		        y1s = (*segments)[candidatos[l]].y1;
    		        x2s = (*segments)[candidatos[l]].x2;
    		        y2s = (*segments)[candidatos[l]].y2;
                        if((mycvDistPointSegment(x1g,y1g,x2g,y2g,x1s,y1s)<THRES_DIST_SEG)||
                          (mycvDistPointSegment(x1g,y1g,x2g,y2g,x2s,y2s)<THRES_DIST_SEG)) {
                           /* Los extremos est� cerca. Hay que unir, y repetir
                            * despu� el proceso (por si alguno que no entr�                            * va a entrar ahora). */
                           unir_a_segmento_global(&(segs_glob[num_segs_glob]),
                                                        x1s,y1s,x2s,y2s);
                    	   x1g = segs_glob[num_segs_glob].x1;
                    	   y1g = segs_glob[num_segs_glob].y1;
                           x2g = segs_glob[num_segs_glob].x2;
                           y2g = segs_glob[num_segs_glob].y2;

                           captados[candidatos[l]] = TRUE;
                           repetir = TRUE;
                           /* Borramos el candidato ahora unido, y pasamos el
                            * ltimo a su lugar. */
                           candidatos[l] = candidatos[numcandidatos-1];
                           numcandidatos--;
                           break;  
                        }
                    }
                } while(repetir);
                /* Ya tenemos el nuevo segmento global. Incrementamos la
                 * cuenta de segmentos globales. */
                num_segs_glob++;
            }                        
        }
    }
    
    /* Finalmente, pasamos los segmentos globales y los no captados a un
     * nuevo array, le ajustamos la memoria din�ica, borramos el antiguo
     * array, y llamamos recursivamente al procedimiento hasta que
     * se vuelva estable: */
    segmentsnew = (TSegment*) 
              malloc((num_segs_glob+(*numsegments))*sizeof(TSegment)); 
    l = 0; 
    for(k=0;k<num_segs_glob;k++) {
        segmentsnew[l].x1 = segs_glob[k].x1;
        segmentsnew[l].y1 = segs_glob[k].y1;
        segmentsnew[l].x2 = segs_glob[k].x2;
        segmentsnew[l].y2 = segs_glob[k].y2;

        /*segmentsnew[l].numpoints = OJO; SIN HACER */

        /*segmentsnew[l].desv_perp = OJO; SIN HACER */

        segmentsnew[l].length = sqrt((segs_glob[k].x2-segs_glob[k].x1)*
                        	     (segs_glob[k].x2-segs_glob[k].x1) + 
                        	     (segs_glob[k].y2-segs_glob[k].y1)*
                        	     (segs_glob[k].y2-segs_glob[k].y1));

        segmentsnew[l].gray_izq = segs_glob[k].gray_izq;
        segmentsnew[l].gray_der = segs_glob[k].gray_der;
        segmentsnew[l].r_izq = segs_glob[k].r_izq;
        segmentsnew[l].r_der = segs_glob[k].r_der;
        segmentsnew[l].g_izq = segs_glob[k].g_izq;
        segmentsnew[l].g_der = segs_glob[k].g_der;
        segmentsnew[l].b_izq = segs_glob[k].b_izq;
        segmentsnew[l].b_der = segs_glob[k].b_der;


        segmentsnew[l].cx = (segmentsnew[k].x2+segmentsnew[k].x1)/2;
        segmentsnew[l].cy = (segmentsnew[k].y2+segmentsnew[k].y1)/2;

        segmentsnew[l].angle = atan2(segmentsnew[k].y2-segmentsnew[k].y1,
                                     segmentsnew[k].x2-segmentsnew[k].x1)+PI;
        l++;
    }

    for(k=0;k<*numsegments;k++) {
        if(!captados[k]) {
    	    memcpy(&(segmentsnew[l]),&((*segments)[k]),sizeof(TSegment));
	        l++;
        }
    }
    
    antiguonumsegments = *numsegments;    
    *numsegments = l;
    qsort(segmentsnew,*numsegments,sizeof(TSegment),compara_length);
    free(*segments);
    segmentsnew = (TSegment*) 
                  realloc(segmentsnew,(*numsegments)*sizeof(TSegment)); 
    *segments = segmentsnew;
   
    /* Liberamos espacio intermedio: */
    free(captados);
    free(presionados);
    free(segs_glob);
    free(candidatos);    
    
    /* Llamada recursiva: */
    if(antiguonumsegments!=*numsegments) {
        mycvPostProcessSegments(segments,numsegments,
                                        minlength,maxlengthtodel,mincontrast);
    }
    
    /* Finalmente, eliminaci� de los cortos: */
    l = 0;
    while((*segments)[l].length>=minlength) {
        l++;
    }
    *segments = (TSegment*) 
                    realloc(*segments,l*sizeof(TSegment)); 
    *numsegments = l;    
}

