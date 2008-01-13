#ifndef _MYCV_UTILSSEGS_H
#define _MYCV_UTILSSEGS_H

#ifdef __cplusplus
extern "C" {
#endif

/* Funciones de utilidad para segmentos (prototipos): */
#include <math.h>

typedef enum {
    SUELO,
    RODAPIE,
    COM_PUERTA,
    FIN_PUERTA,
    JUNTA_PLANOS
} TEtiq;

typedef struct {
    float x1,y1,z1,x2,y2,z2; /* Coordenadas 3D del segmento. */
    TEtiq etiq; /* Etiqueta con el tipo de segmento. */
    //int corroboracion;
} TSeg3DEtiq;

/* Imprime un segmento: */
void print_segment(TSegment *seg_in);
/* Resetea un segmento: */
void reset_segment(TSegment *seg_out);
/* Recomputa parámetros de un segmento a partir de sus extremos: */
void recompute_seg_parms_from_extremes(TSegment *seg_out);
/* Recomputa parámetros de un segmento intercambiando los extremos: */
void inv_seg_extremes(TSegment *seg_in_out);
/* Función que devuelve 0 si los extremos de dos segmentos no coinciden 
 * (hasta un umbral maxdist), o 1, 2, 3 o 4 si coinciden p1-p1, p1-p2,
 * p2-p1 o p2-p2: */
/* Calcula el punto de cruce de dos segmentos y devuelve la distancia mayor
 * o menor (dependiendo del flag de entrada "mayor") de dicho punto a uno 
 * de los extremos de los segmentos. */
float pto_cruce_segs(TSegment *seg1,TSegment *seg2,
                     int mayor,float *x,float *y); 
int coinciden_segs(TSegment *seg1,TSegment *seg2,
                   float maxdist);
/* Devuelve 1 o 0 según dos segmentos estén alineados o no hasta un umbral
 * de distancia perpendicular a la recta marcada por el más largo de maxdist:
 */
int alineados_segs(TSegment *seg1,TSegment *seg2,float maxdist,
                   float maxangle);
/* Devuelve 0 si un segmento no incide (uno de sus extremos toca el otro seg-
 * mento a cualquier altura) en el otro, y 1 o 2 si sí incide, a través de su
 * extremo 1 o 2:
 */
int incide_en_segs(TSegment *seg1,TSegment *seg2,float maxdist);
/* Devuelve 1 si el segmento 1 está superpuesto sobre el 2, 0 en caso 
 * contrario. Hay un umbral para distancia de los extremos del segundo
 * segmento sobre una banda alrededor del primero, y otro umbral para 
 * el ángulo. */
int superpuesto_sobre_seg(TSegment *seg_1,TSegment *seg_2,
                           float maxdist,float maxangle);
/* Igual al anterior, pero el umbral de distancia es a la recta (no al
 * segmento), y la condición es ahora también que los extremos correspondientes
 * de los segmentos no salgan del otro de un umbral extrem_thres: */
int superpuesto_sobre_seg_with_extreme_thres(TSegment *seg_1,
                                             TSegment *seg_2,
                                             float maxdist,float maxangle,
                                             float extrem_thres);
/* Devuelve el valor X en el que se corta un segmento con el infinito: */
float corte_infinito(TSegment *segmento,float yhoriz,int rows,int cols);
/* Transforma un segmento por una homografía (haciendo el cambio de centro
 * de imagen adecuado). Devuelve false en caso de fallo: */
int transf_seg_homog(float homog[3][3],
        		          int img_to_wrld,	
                          TSegment *seg_in,
                          TSegment *seg_out,int rows,int cols);
/* Busca una línea izquierda y otra derecha de pasillo (de rodapie o de suelo,
 * según se le pida), y devuelve los índices de los segmentos encontrados,
 * y la altura de horizonte resultante: */
int mycvGetYhorizFromSegs(TSegment *segments,int numsegments,
                               int rodapie,float min_length_line,
                               float *yhoriz,int *ind_l,int *ind_r);
/* Busca el corte de la línea de suelo más grande con el horizonte, y devuelve
 * dicho valor, junto con el índice de la línea buscada. */
int mycvGetXhorizFromSegs(TSegment *segments,int numsegments,
                               int rodapie,float min_length_line,
                               float yhoriz,float *xhoriz,int *ind,
                               int rows,int cols);
/* OJO: Busca un punto de puerta en uno de los lados del pasillo. */
int mycvDoorPointFromSegs(TSegment *segments,int numsegments,
                               int rodapie,int left_side,
                               float *x,float *y);
/* Recorta un segmento a los bordes de la imagen. */
int clip_segment_to_image(TSegment *seg2D,int rows,int cols);
/* Proyecta un segmento 3D a uno 2D: */
int proyectar_segmento(TSeg3DEtiq *seg3D,float mat_proy[3][4],
                            TSegment *seg2D,int rows,int cols);
/* Recoloca un segmento 3D a partir de un movimiento medido con la odometría: */
void corrige_segmento_3D_con_odom(TSeg3DEtiq *seg_etiq,float incodomx,
                                  float incodomy,float incodomtheta);
/* Devuelve (0,1,2 o 3) si el color de (nigún lado,el lado izq.,el lado der.
 * o los dos lados) está en el intervalo (thresmin,thresmax): */
int lado_intervalo_color(TSegment *seg,
                         unsigned char thresmin,unsigned char thresmax);
/* Distancia entre dos segmentos: */                         
float mycvDistSegmentSegment(TSegment *seg1,TSegment *seg2);
/* Dice si se cortan o no dos segmentos: */
int se_cortan_segs(TSegment *seg1,TSegment *seg2,float thres);
#ifdef __cplusplus
}
#endif

#endif /* _MYCV_UTILSSEGS_H */
