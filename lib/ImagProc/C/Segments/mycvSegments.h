#ifndef _MYCV_SEGMENTS_H
#define _MYCV_SEGMENTS_H

#ifdef __cplusplus
extern "C" {
#endif

/* IPP headers inclusion (valid for static & dynamic linking, depending on the
 * defined compilation flags: */
#ifdef MYCPU_T7
  #include <ipp_t7.h>
#endif
#ifdef MYCPU_W7
  #include <ipp_w7.h>
#endif
#ifdef MYCPU_A6
  #include <ipp_a6.h>
#endif
#ifdef MYCPU_PX
  #include <ipp_px.h>
#endif
#include <ipp.h>

/* Tipo de datos segmento, con todos los campos calculados por las funciones 
 * del m�dulo. Todos deber�an ser autoexplicativos. Los valores medianos 
 * corresponden al valor gris � vector RGB que caracteriza mejor a la imagen
 * fuente a cada lado del segmento. */
typedef struct {
    float x1,y1,x2,y2; /* Extremos del segmento. */
    unsigned char gray_izq,gray_der; /* Valores de gris medianos. */
    unsigned char r_izq,r_der,g_izq,g_der,b_izq,b_der; /* RGB medianos. */
    int num_points; /* N�mero de puntos pasa-alta capturados. */
    float cx,cy,angle,length; /* Centro, �ngulo y longitud del segmento.*/
    float desv_perp; /* Desviaci�n t�pica en direcci�n perpendicular. */
} TSegment;

/* Funci�n que calcula segmentos con precisi�n subpixel, a partir de una 
 * imagen de entrada de grises (8u_C1). Hay dos versiones, una con par�metros
 * por defecto, y otra cambiando el usuario estos par�metros: */
/*void mycvSegments_8u_C1(Ipp8u *imageIn,IplImage *imageIn,IplImage *imageInGray,
                        TSegment **segments,int *num_segments); */
void mycvSegmentsWithParms_8u_C1_C3(Ipp8u *imgIn,int stepIn, 
                                    int x1,int y1,int roiSize_w,int roiSize_h,
                                    TSegment **segments,int *num_segments,
                                    int user_radius,float user_width_edgel,
                                    int med_siz,
                                    unsigned char thres_high,
                                    unsigned char thres_low,
                                    int one_channel);

/* Funci�n que, de modo eficiente, trata de unir segmentos alineados, agru-
 * pando en torno a los mas largos. Tambi�n elimina aquellos muy cortos o que
 * no tienen un contraste m�nimo. */
void mycvPostProcessSegments(TSegment **segments,
                             int *numsegments,
                             float minlength,float maxlengthtodel,
                             unsigned char mincontrast);
#ifdef __cplusplus
}
#endif

#endif /* _MYCV_SEGMENTS_H */
