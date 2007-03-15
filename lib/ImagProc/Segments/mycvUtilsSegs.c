/* Funciones de utilidad para segmentos: */
#include <stdlib.h>
#include <stdio.h>
#include <math.h>

#define TRUE  0
#define FALSE 1

#include "mycvUtils.h"
#include "mycvSegments.h"
#include "mycvUtilsSegs.h"

void print_segment(TSegment *seg_in)
{
    printf("((x1,y1) (x2,y2))=((%f,%f),(%f,%f))    ",
           seg_in->x1,seg_in->y1,seg_in->x2,seg_in->y2);
    printf("(cx,cy)=(%f,%f)    (angle)=%f    (length)=%f\n",
           seg_in->cx,seg_in->cy,seg_in->angle,seg_in->length);
}

/* Resetea un segmento: */
void reset_segment(TSegment *seg_out)
{
    seg_out->x1 = 0;
    seg_out->y1 = 0;
    seg_out->x2 = 0;
    seg_out->y2 = 0;        
    seg_out->cx = 0;        
    seg_out->cy = 0;        
    seg_out->angle = 0;        
    seg_out->length = 0;        
}


/* Recomputa parámetros de un segmento a partir de sus extremos: */
void recompute_seg_parms_from_extremes(TSegment *seg_out)
{    
    /* Resto de campos: */
    seg_out->cx = (seg_out->x1 + seg_out->x2)/2;
    seg_out->cy = (seg_out->y1 + seg_out->y2)/2;
    seg_out->angle = atan2(seg_out->y2-seg_out->y1,seg_out->x2-seg_out->x1) + M_PI;
    seg_out->length = sqrt((seg_out->y2-seg_out->y1)*(seg_out->y2-seg_out->y1) +
                           (seg_out->x2-seg_out->x1)*(seg_out->x2-seg_out->x1));
}

/* Recomputa parámetros de un segmento intercambiando los extremos: */
void inv_seg_extremes(TSegment *seg_in_out)
{    
    float temp;
    unsigned char tempchar;
    
    temp = seg_in_out->x1;
    seg_in_out->x1 = seg_in_out->x2;
    seg_in_out->x2 = temp;
    temp = seg_in_out->y1;
    seg_in_out->y1 = seg_in_out->y2;
    seg_in_out->y2 = temp;
    if(seg_in_out->angle>M_PI) {
        seg_in_out->gray_der -= M_PI;
    } else {
        seg_in_out->gray_der += M_PI;
    }
    tempchar = seg_in_out->gray_der;
    seg_in_out->gray_der = seg_in_out->gray_izq;
    seg_in_out->gray_izq = tempchar;
}

/* Calcula el punto de cruce de dos segmentos y devuelve la distancia mayor
 * o menor (dependiendo del flag de entrada "mayor") de dicho punto a uno 
 * de los extremos de los segmentos. */
float pto_cruce_segs(TSegment *seg1,TSegment *seg2,
                     int mayor,float *x,float *y)
{
    float d1,d2,mindist1,mindist2,xout,yout,zout;
    
    mycvGetCrossPoint(seg1->x1,seg1->y1,seg1->x2,seg1->y2,
                      seg2->x1,seg2->y1,seg2->x2,seg2->y2,    
                      &xout,&yout,&zout);
    if(fabs(zout)<EPSILON) {
        *x = 0;
        *y = 0;
        return FLOAT_INFINITY;
    } else {
        *x = xout/zout;
        *y = yout/zout;
        d1 = mycvDistPointPoint(seg1->x1,seg1->y1,*x,*y);
        d2 = mycvDistPointPoint(seg1->x2,seg1->y2,*x,*y);
        mindist1 = (d1<d2)?d1:d2;
        d1 = mycvDistPointPoint(seg2->x1,seg2->y1,*x,*y);
        d2 = mycvDistPointPoint(seg2->x2,seg2->y2,*x,*y);
        mindist2 = (d1<d2)?d1:d2;
        if(mayor) {
            return ((mindist1>mindist2)?mindist1:mindist2);
        } else {
            return ((mindist1<mindist2)?mindist1:mindist2);
        }
    }
}



/* Función que devuelve 0 si los extremos de dos segmentos no coinciden 
 * (hasta un umbral maxdist), o 1, 2, 3 o 4 si coinciden p1-p1, p1-p2,
 * p2-p1 o p2-p2: */
int coinciden_segs(TSegment *seg1,TSegment *seg2,
                   float maxdist)
{
    float d11,d12,d21,d22,min;

    d11 = mycvDistPointPoint(seg1->x1,seg1->y1,seg2->x1,seg2->y1);
    d12 = mycvDistPointPoint(seg1->x1,seg1->y1,seg2->x2,seg2->y2);
    d21 = mycvDistPointPoint(seg1->x2,seg1->y2,seg2->x1,seg2->y1);
    d22 = mycvDistPointPoint(seg1->x2,seg1->y2,seg2->x2,seg2->y2);

    min = d11;
    if(d12<min){
        min=d12;
    }
    if(d21<min){
        min=d21;
    }
    if(d22<min){
        min=d22;
    }
    if(min<=maxdist) {
        if(min==d11) {
            return 1;
        } else if(min==d12) {
            return 2;
        } else if(min==d21) {
            return 3;
        } else if(min==d22) {
            return 4;
        }
    }
    
    return 0;
}


/* Devuelve 1 o 0 según dos segmentos estén alineados o no hasta un umbral
 * de distancia perpendicular a la recta marcada por el más largo de maxdist:
 */
int alineados_segs(TSegment *seg1,TSegment *seg2,
                   float maxdist,float maxangle)
{
    TSegment *tempptr;
    float d1,d2;
    if(!angulo_similar(seg1->angle,seg2->angle,maxangle)) {
        return 0;
    }
    if(seg1->length < seg2->length) {
        tempptr = seg1;
        seg1 = seg2;
        seg2 = tempptr;
    }
    d1 = mycvDistPointLine(seg1->x1,seg1->y1,seg1->x2,seg1->y2,
                           seg2->x1,seg2->y1,0);
    d2 = mycvDistPointLine(seg1->x1,seg1->y1,seg1->x2,seg1->y2,
                           seg2->x2,seg2->y2,0);
    if((d1<maxdist)&&(d2<maxdist)) {
        return 1;
    } else {
        return 0;
    }
}


/* Devuelve 0 si un segmento no incide (uno de sus extremos toca el otro seg-
 * mento a cualquier altura) en el otro, y 1 o 2 si sí incide, a través de su
 * extremo 1 o 2:
 */
int incide_en_segs(TSegment *seg1,TSegment *seg2,float maxdist)
{
    float d1,d2;
    
    d1 = mycvDistPointSegment(seg2->x1,seg2->y1,seg2->x2,seg2->y2,
                              seg1->x1,seg1->y1);
    d2 = mycvDistPointSegment(seg2->x1,seg2->y1,seg2->x2,seg2->y2,
                              seg1->x2,seg1->y2);
    if(d1<maxdist) {
        return 1;
    } else if(d2<maxdist) {
        return 2;
    } else {
        return 0;
    }
}

/* Devuelve 1 si el segmento 1 está superpuesto sobre el 2, 0 en caso 
 * contrario. Hay un umbral para distancia de los extremos del segundo
 * segmento sobre una banda alrededor del primero, y otro umbral para 
 * el ángulo. */
int superpuesto_sobre_seg(TSegment *seg_1,TSegment *seg_2,
                           float maxdist,float maxangle)
{
    if((angulo_similar(seg_1->angle,seg_2->angle,maxangle)) &&
       (mycvDistPointSegment(seg_2->x1,seg_2->y1,seg_2->x2,seg_2->y2,
                             seg_1->x1,seg_1->y1) < maxdist) &&
       (mycvDistPointSegment(seg_2->x1,seg_2->y1,seg_2->x2,seg_2->y2,
                             seg_1->x2,seg_1->y2) < maxdist)) {
        return 1;
    } else {
        return 0;
    }
}

/* Igual al anterior, pero el umbral de distancia es a la recta (no al
 * segmento), y la condición es ahora también que los extremos correspondientes
 * de los segmentos no salgan del otro de un umbral extrem_thres: */
int superpuesto_sobre_seg_with_extreme_thres(TSegment *seg_1,
                                             TSegment *seg_2,
                                             float maxdist,float maxangle,
                                             float extrem_thres)
{
    float d1,d2;
    int invertido = FALSE,resp;
    
    if(!angulo_similar(seg_1->angle,seg_2->angle,maxangle)) {
        return 0;
    }
    d1 = mycvDistPointLine(seg_2->x1,seg_2->y1,seg_2->x2,seg_2->y2,
                           seg_1->x1,seg_1->y1,0);
    d2 = mycvDistPointLine(seg_2->x1,seg_2->y1,seg_2->x2,seg_2->y2,
                           seg_1->x2,seg_1->y2,0);
    if((d1>maxdist)||(d2>maxdist)) {
        return 0;
    } else {
        if(fabs(incr_angle(seg_1->angle,seg_2->angle,FALSE)) > M_PI/2) {
            invertido = TRUE;
            inv_seg_extremes(seg_2);
        }
        if( (mycvDistPointSegment(seg_2->x1,seg_2->y1,seg_2->x2,seg_2->y2,
                                  seg_1->x1,seg_1->y1) > extrem_thres)||
            (mycvDistPointSegment(seg_2->x1,seg_2->y1,seg_2->x2,seg_2->y2,
                                  seg_1->x2,seg_1->y2) > extrem_thres) ) {
            resp = FALSE;
        } else {
            resp = TRUE;
        }
    }

    if(invertido) { // Dejar como estaba
        inv_seg_extremes(seg_2);
    }
    
    return resp;
}


/* Devuelve el valor X en el que se corta un segmento con el infinito: */
float corte_infinito(TSegment *segmento,float yhoriz,int rows, int cols)
{    
    float xout,yout,zout;
    
    mycvGetCrossPoint(segmento->x1,segmento->y1,segmento->x2,segmento->y2,
         	     0,rows/2-yhoriz,
                     cols,rows/2-yhoriz,
                     &xout,&yout,&zout);
    if(fabs(zout)<EPSILON) {
        return FLOAT_INFINITY;
    }
    return (xout/zout);
}

/* Transforma un segmento por una homografía (haciendo el cambio de centro
 * de imagen adecuado). Devuelve false en caso de fallo: */
int transf_seg_homog(float homog[3][3],
        		          int img_to_wrld,	
                          TSegment *seg_in,
                          TSegment *seg_out,int rows,int cols)
{   
    float point1[3],point2[3];    

    if(img_to_wrld) {	
	point1[0] = seg_in->x1-cols/2;
	point1[1] = rows/2-seg_in->y1;
    } else {
	point1[0] = seg_in->x1;
	point1[1] = seg_in->y1;    
    }	
    point1[2] = 1.0;
    mycvCalcImgHomog(homog,point1,point2);
    if(fabs(point2[2])<EPSILON) {
        return FALSE;
    }
    if(img_to_wrld) {
	seg_out->x1 = point2[0]/point2[2];
	seg_out->y1 = point2[1]/point2[2];
    } else {
	seg_out->x1 = point2[0]/point2[2] + cols/2;
	seg_out->y1 = -point2[1]/point2[2] + rows/2;    
    }

    if(img_to_wrld) {	
	point1[0] = seg_in->x2-cols/2;
	point1[1] = rows/2-seg_in->y2;
    } else {
	point1[0] = seg_in->x2;
	point1[1] = seg_in->y2;    
    }	
    point1[2] = 1.0;
    mycvCalcImgHomog(homog,point1,point2);
    if(fabs(point2[2])<EPSILON) {
        return FALSE;
    }
    if(img_to_wrld) {
	seg_out->x2 = point2[0]/point2[2];
	seg_out->y2 = point2[1]/point2[2];
    } else {
	seg_out->x2 = point2[0]/point2[2] + cols/2;
	seg_out->y2 = -point2[1]/point2[2] + rows/2;    
    }

    /* Parámetros restantes: */	
    recompute_seg_parms_from_extremes(seg_out);
    seg_out->num_points = seg_in->num_points;
    seg_out->gray_izq = seg_in->gray_izq;
    seg_out->gray_der = seg_in->gray_der;      
    return TRUE;    
}


/* Busca una línea izquierda y otra derecha de pasillo (de rodapie o de suelo,
 * según se le pida), y devuelve los índices de los segmentos encontrados,
 * y la altura de horizonte resultante: */
int mycvGetYhorizFromSegs(TSegment *segments,int numsegments,
                               int rodapie,float min_length_line,
                               float *yhoriz,int *ind_l,int *ind_r)
{
    int found_left,found_right,condition;
    int k,ind_left,ind_right;
    float xout,yout,zout;
    unsigned char col_arr=0,col_aba=0;
    
    found_left = FALSE;    
    found_right = FALSE;
    ind_left = -1;
    ind_right = -1;
    k=0;
    while((!found_left)||(!found_right)) {
        if(k==numsegments) {
            break;
        }
        if(segments[k].length<min_length_line) {
            break;
        }
        if(segments[k].x1<segments[k].x2) {
            /* arriba es izquierda */
            col_arr = segments[k].gray_izq;
            col_aba = segments[k].gray_der;
        } else {
            /* arriba es derecha */
            col_arr = segments[k].gray_der;
            col_aba = segments[k].gray_izq;
        }
        condition = FALSE;
        if((rodapie&&(col_arr>col_aba))||((!rodapie)&&(col_arr<col_aba))) {
            condition = TRUE;
        }
        if((!found_left)&&(angulo_similar(segments[k].angle,11*M_PI/16,M_PI/8))&&
	       (condition)) {
    	    found_left = TRUE;
	    ind_left = k;
        } else if((!found_right)&&(condition)&&
                (angulo_similar(segments[k].angle,5*M_PI/16,M_PI/8))) {
    	    found_right = TRUE;	
	    ind_right = k;
	}
	k++;
    }
    
    if(found_right&&found_left) {
	mycvGetCrossPoint(segments[ind_left].x1,segments[ind_left].y1,
                	  segments[ind_left].x2,segments[ind_left].y2,
        			  segments[ind_right].x1,segments[ind_right].y1,
                	  segments[ind_right].x2,segments[ind_right].y2,
			  &xout,&yout,&zout);
	if(fabs(zout)<EPSILON) {
    	    printf("PARADOJA en mycvGetyhorizFromSegs!!\n");
	    exit(1);
	}
        if(segments[ind_left].cx<segments[ind_right].cx) {
	    *yhoriz = yout/zout;
            *ind_l = ind_left;
            *ind_r = ind_right;
        } else {
            printf("Izquierdo y derecho de pasillo encontrados, pero no ordenados!!\n");
            return FALSE;
        }        
        return TRUE;
    } else {
        return FALSE;
    }
          
}			  


/* Busca el corte de la línea de suelo más grande con el horizonte, y devuelve
 * dicho valor, junto con el índice de la línea buscada. */
int mycvGetXhorizFromSegs(TSegment *segments,int numsegments,
                               int rodapie,float min_length_line,
                               float yhoriz,float *xhoriz,int *ind,
                               int rows,int cols)
{
    int found_line,condition;
    int k,ind_line;
    unsigned char col_arr=0,col_aba=0;
    
    ind_line = -1;
    found_line = FALSE;    
    k=0;
    while(!found_line) {
        if(k==numsegments) {
	        break;
    	}
    	if(segments[k].length<min_length_line) {
	        break;
    	}
        if(segments[k].x1<segments[k].x2) {
            /* arriba es izquierda */
            col_arr = segments[k].gray_izq;
            col_aba = segments[k].gray_der;
        } else {
            /* arriba es derecha */
            col_arr = segments[k].gray_der;
            col_aba = segments[k].gray_izq;
        }	
        condition = FALSE;
        if((rodapie&&(col_arr>col_aba))||((!rodapie)&&(col_arr<col_aba))) {
            condition = TRUE;
        }
        if((!found_line)&&(condition)&&
	    ( (angulo_similar(segments[k].angle,5*M_PI/16,M_PI/8)) ||
	      (angulo_similar(segments[k].angle,11*M_PI/16,M_PI/8)) ) ) {
    	    found_line = TRUE;	
            ind_line = k;
    	}
	    k++;
    }
    
    if(found_line) {
        *xhoriz = corte_infinito(&segments[ind_line],yhoriz,rows,cols);
        return TRUE;
    } else {
        return FALSE;
    }
}

/* OJO: Busca un punto de puerta en uno de los lados del pasillo. */
int mycvDoorPointFromSegs(TSegment *segments,int numsegments,
                               int rodapie,int left_side,
                               float *x,float *y)
{
    int found_side_line,condition;
    int k,ind_side_line,ind_vert_line;
    float xout,yout,zout,maxy;
    unsigned char col_arr=0,col_aba=0,col_izq=0,col_der=0;
    
    ind_side_line = -1;
    found_side_line = FALSE;    
    k = 0;
    while(!found_side_line) {
        if(k==numsegments) {
    	    break;
	    }
    	/* OJO if(segments[k].length<MIN_LENGTH_CORR_LINE) {
	        break;
    	} */
        if(segments[k].x1<segments[k].x2) {
            /* arriba es izquierda */
            col_arr = segments[k].gray_izq;
            col_aba = segments[k].gray_der;
        } else {
            /* arriba es derecha */
            col_arr = segments[k].gray_der;
            col_aba = segments[k].gray_izq;
        }	
        condition = FALSE;
        if((rodapie&&(col_arr>col_aba))||((!rodapie)&&(col_arr<col_aba))) {
            condition = TRUE;
        }        
        if((!found_side_line)&&(condition)&&
    	   ( ((left_side)&&(angulo_similar(segments[k].angle,3*M_PI/4,M_PI/8)))||
	         ((!left_side)&&
	        (angulo_similar(segments[k].angle,M_PI/4,M_PI/8))) ) ) {
    	    found_side_line = TRUE;	
    	    ind_side_line = k;
	    }
	    k++;
    }

    if(!found_side_line) {
    	return FALSE;
    }

    k = 0;
    maxy = 0;
    ind_vert_line = -1;
    //OJO while(segments[k].length>=MIN_LENGTH_VERT_LINE) {
    while(segments[k].length>=40) {    
        if(segments[k].y1<segments[k].y2) {
            /* derecha es izquierda */
            col_der = segments[k].gray_izq;
            col_izq = segments[k].gray_der;
        } else {
            /* izquierda es izquierda */
            col_der = segments[k].gray_der;
            col_izq = segments[k].gray_izq;
        }	
	/* Buscamos la vertical: */
        if((angulo_similar(segments[k].angle,M_PI/2,M_PI/8))&&
	   ( ((left_side)&&(col_izq>col_der)) ||
	     ((!left_side)&&(col_der>col_izq)) ) ) {
    	    //printf("---->%d\n",k);
	        mycvGetCrossPoint(
	            segments[ind_side_line].x1,segments[ind_side_line].y1,
                    segments[ind_side_line].x2,segments[ind_side_line].y2,
	            segments[k].x1,segments[k].y1,
                    segments[k].x2,segments[k].y2,
                    &xout,&yout,&zout);
	        if(yout/zout > maxy) { 
    	    	    maxy = yout/zout;
	                ind_vert_line = k;
	        }	
    	}
	    k++;
    }

    mycvGetCrossPoint(
	    segments[ind_side_line].x1,segments[ind_side_line].y1,
            segments[ind_side_line].x2,segments[ind_side_line].y2,
	    segments[ind_vert_line].x1,segments[ind_vert_line].y1,
            segments[ind_vert_line].x2,segments[ind_vert_line].y2,
            &xout,&yout,&zout);
    if(ind_vert_line!=-1) {
	    *x = xout/zout;
	    *y = yout/zout;
	    return TRUE;
    } else {
    	return FALSE;
    }
}

/* Recorta un segmento a los bordes de la imagen (algoritmo Cohen-Sutherland). */
int clip_segment_to_image(TSegment *seg2D,int rows,int cols)
{
    unsigned char code1=0,code2=0;
    float xout,yout,zout;
    
    if(seg2D->y1<0) {
        code1 |= 0x08;
    }
    if(seg2D->y1>rows) {
        code1 |= 0x04;
    }
    if(seg2D->x1>cols) {
        code1 |= 0x02;
    }
    if(seg2D->x1<0) {
        code1 |= 0x01;
    }
    if(seg2D->y2<0) {
        code2 |= 0x08;
    }
    if(seg2D->y2>rows) {
        code2 |= 0x04;
    }
    if(seg2D->x2>cols) {
        code2 |= 0x02;
    }
    if(seg2D->x2<0) {
        code2 |= 0x01;
    }

    if((code1&code2)!=0) {
        reset_segment(seg2D);
        return FALSE;
    } else if((code1|code2)==0) {
        recompute_seg_parms_from_extremes(seg2D);
        return TRUE;
    } else {
        if(code1) {
            if(code1>=8) {
                mycvGetCrossPoint(seg2D->x1,seg2D->y1,seg2D->x2,seg2D->y2,
                                  0,0,cols,0,&xout,&yout,&zout);
            } else if(code1>=4){
                mycvGetCrossPoint(seg2D->x1,seg2D->y1,seg2D->x2,seg2D->y2,
                                  0,rows,cols,rows,&xout,&yout,&zout);
            } else if(code1>=2) {
                mycvGetCrossPoint(seg2D->x1,seg2D->y1,seg2D->x2,seg2D->y2,
                                  cols,0,cols,rows,&xout,&yout,&zout);
            } else if(code1>=1) {
                mycvGetCrossPoint(seg2D->x1,seg2D->y1,seg2D->x2,seg2D->y2,
                                  0,0,0,rows,&xout,&yout,&zout);        
            } 
            if(fabs(zout)<EPSILON) {
                reset_segment(seg2D);
                return FALSE;
            } else {
                seg2D->x1 = xout/zout;
                seg2D->y1 = yout/zout;
            }    
        }
        
        if(code2) {
            if(code2>=8) {
                mycvGetCrossPoint(seg2D->x1,seg2D->y1,seg2D->x2,seg2D->y2,
                                  0,0,cols,0,&xout,&yout,&zout);
            } else if(code2>=4){
                mycvGetCrossPoint(seg2D->x1,seg2D->y1,seg2D->x2,seg2D->y2,
                                  0,rows,cols,rows,&xout,&yout,&zout);
            } else if(code2>=2) {
                mycvGetCrossPoint(seg2D->x1,seg2D->y1,seg2D->x2,seg2D->y2,
                                  cols,0,cols,rows,&xout,&yout,&zout);
            } else if(code2>=1) {
                mycvGetCrossPoint(seg2D->x1,seg2D->y1,seg2D->x2,seg2D->y2,
                                  0,0,0,rows,&xout,&yout,&zout);        
            } 
            if(fabs(zout)<EPSILON) {
                reset_segment(seg2D);
                return FALSE;
            } else {
                seg2D->x2 = xout/zout;
                seg2D->y2 = yout/zout;
            }    
        }
        return clip_segment_to_image(seg2D,rows,cols);
    }   
} 

int proyectar_segmento(TSeg3DEtiq *seg3D,float mat_proy[3][4],
                            TSegment *seg2D,int rows,int cols)
{
    int i,j,k;
    float pin[2][4],pout[2][3],x[2],y[2],alfa;
    int finito[2];

    pin[0][0] = seg3D->x1;            
    pin[0][1] = seg3D->y1;
    pin[0][2] = seg3D->z1;
    pin[0][3] = 1.0;
    pin[1][0] = seg3D->x2;
    pin[1][1] = seg3D->y2;
    pin[1][2] = seg3D->z2;
    pin[1][3] = 1.0;
    
    for(k=0;k<2;k++) {        
        for(i=0;i<3;i++) {
            pout[k][i] = 0.0;
            for(j=0;j<4;j++) {
                pout[k][i] += mat_proy[i][j]*pin[k][j];
            }
        }
    }

    for(k=0;k<2;k++) {       
        finito[k] = FALSE; 
        if(fabs(pout[k][2])>EPSILON) {
            x[k] = pout[k][0]/pout[k][2];    
            y[k] = pout[k][1]/pout[k][2];
            finito[k] = TRUE;
        }
    }
    
    if((!finito[0])&&(!finito[1])) {
        //printf("\n\n¡1!\n\n");
        return FALSE;
    } else if(!finito[0]) {
        alfa = atan2(pout[0][1],pout[0][2]);
        x[0] = x[1] + 1000.0*cos(alfa);
        y[0] = y[1] + 1000.0*sin(alfa);
    } else if(!finito[1]) {
        alfa = atan2(pout[1][1],pout[1][2]);
        x[1] = x[0] + 1000.0*cos(alfa);
        y[1] = y[0] + 1000.0*sin(alfa);
    }
    
    if((pout[0][2]<0.0)&&(pout[1][2]<0.0)) {
        //printf("\n\n¡2!\n\n");
        return FALSE;        
    } else if(pout[0][2]<0.0) {
        x[0] = 2*x[1]-x[0];
        y[0] = 2*y[1]-y[0];
    } else if(pout[1][2]<0.0) {
        x[1] = 2*x[0]-x[1];
        y[1] = 2*y[0]-y[1];    
    }
    
    seg2D->x1 = x[0]+cols/2;
    seg2D->y1 = rows/2-y[0];
    seg2D->x2 = x[1]+cols/2;
    seg2D->y2 = rows/2-y[1];            

    //printf("\n\n(%f %f) (%f %f)\n\n",x[0],y[0],x[1],y[1]);
    return clip_segment_to_image(seg2D,rows,cols);     
}

void corrige_segmento_3D_con_odom(TSeg3DEtiq *seg_etiq,float incodomx,
                                  float incodomy,float incodomtheta)
{
    float nx,ny,nxr,nyr;

    /* Corregimos sólo las X e Y del segmento (la Z no cambia): */
    nx = seg_etiq->x1-incodomx;
    ny = seg_etiq->y1-incodomy;
    nxr = cos(incodomtheta)*nx+sin(incodomtheta)*ny;
    nyr = -sin(incodomtheta)*nx+cos(incodomtheta)*ny;
    seg_etiq->x1 = nxr;
    seg_etiq->y1 = nyr;

    nx = seg_etiq->x2-incodomx;
    ny = seg_etiq->y2-incodomy;
    nxr = cos(incodomtheta)*nx+sin(incodomtheta)*ny;
    nyr = -sin(incodomtheta)*nx+cos(incodomtheta)*ny;
    seg_etiq->x2 = nxr;
    seg_etiq->y2 = nyr;
}    

/* Devuelve (0,1,2 o 3) si el color de (nigún lado,el lado izq.,el lado der.
 * o los dos lados) está en el intervalo (thresmin,thresmax). */
int lado_intervalo_color(TSegment *seg,
                         unsigned char thresmin,unsigned char thresmax)
{
    if((seg->gray_izq>=thresmin)&&(seg->gray_izq<=thresmax) &&
       (seg->gray_der>=thresmin)&&(seg->gray_der<=thresmax)) {
        return 3;
    } else if((seg->gray_izq>=thresmin)&&(seg->gray_izq<=thresmax)) {
        return 1;
    } else if((seg->gray_der>=thresmin)&&(seg->gray_der<=thresmax)) {
        return 2;
    } else {
        return 0;
    }
}


float mycvDistSegmentSegment(TSegment *seg1,TSegment *seg2)
{
    float d[4],minim;
    int i;
    
    d[0] = mycvDistPointSegment(seg1->x1,seg1->y1,seg1->x2,seg1->y2,
                                seg2->x1,seg2->y1);
    d[1] = mycvDistPointSegment(seg1->x1,seg1->y1,seg1->x2,seg1->y2,
                                seg2->x2,seg2->y2);
    d[2] = mycvDistPointSegment(seg2->x1,seg2->y1,seg2->x2,seg2->y2,
                                seg1->x1,seg1->y1);
    d[3] = mycvDistPointSegment(seg2->x1,seg2->y1,seg2->x2,seg2->y2,
                                seg1->x2,seg1->y2);
    minim = d[0];
    for(i=1;i<4;i++) {
        if(d[i]<minim) {
            minim = d[i];
        }
    }
    return minim;  
}

int se_cortan_segs(TSegment *seg1,TSegment *seg2,float thres)
{
    float x,y;
    
    pto_cruce_segs(seg1,seg2,TRUE,&x,&y);
    if((mycvDistPointSegment(seg1->x1,seg1->y1,seg1->x2,seg1->y2,x,y)<thres)&&
       (mycvDistPointSegment(seg2->x1,seg2->y1,seg2->x2,seg2->y2,x,y)<thres)){
        return TRUE;
    } else {
        return FALSE;
    }
}
