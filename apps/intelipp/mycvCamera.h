/* Copyright Pedro E. López de Teruel <pedroe@ditec.um.es>
   License: GPL
*/

#ifndef _MYCV_CAMERA_H
#define _MYCV_CAMERA_H

#ifdef __cplusplus
extern "C" {
#endif

/* IPP headers inclusion (valid for static & dynamic linking, depending on the
 * defined compilation flags: */

#include <ipp.h>

/* Tipos de c�ara: */
#define IIDC_CAM_LIVE  0
#define DV_CAM_LIVE    1
#define DV_CAM_FILE    2
#define RGB_CAM_FILE   3
#define BW_CAM_FILE    4

/* Modos de c�ara (RGB/BW): */
#define RGB_MODE     0
#define BW_MODE      1

/* Tama� por defecto para im�enes de c�aras IIDC (1394 no comprimidas): */
#define IIDC_COLS 640
#define IIDC_ROWS 480 

/* Tama� por defecto para im�enes DV: */
#define DV_COLS 720
#define DV_ROWS 576 /* OJO, 540 es el que corresponder� a prop. 1.333). */

/* Tama� m�imo de imagen permitido en la captura: */
#define MAX_COLS (DV_COLS*5)
#define MAX_ROWS (DV_ROWS*5)

/* Estructura con informaci� de una c�ara abierta: */
typedef struct t_info_cam {
    /* Tipo de c�ara: */
    int type;
    
    /* Nmero de filas y columnas de la fuente (fichero o dispositivo) y del
     * destino (tama� solicitado por el usuario): */
    int src_rows,src_cols,dst_rows,dst_cols;

    /* Direcci� de recorrido actual del fichero de imagen (s�o para c�aras
     * no live). dir=0 => hacia adelante, dir=1 => hacia atr� (cuando se
     * alcanza el final del fichero, se vuelve sobre los pasos anteriores): */
    int dir;
    
    /* Nmero de frame actual del fichero (0..maxframes-1), num�o de frames 
     * le�os (coincide con cur_frame para lectura continua de c�aras en vivo
     * ("/dev/dv1394" (DV), o "/dev/video1394" (IIDC))pero no necesariamente
     * en el caso de un fichero, si se empieza a leer hacia atr� al llegar
     * al final), y m�imo nmero de frames almacenados en el fichero
     * (arbitrariamente 0 para c�eras "live"): */
    int cur_frame,num_frames,max_frames;

    /* Modo actual (BW_MODE / RGB_MODE): */
    int mode;
} TInfoCam;

/* Funci� para abrir una c�ara, que recibe un nombre de fichero (que puede ser
 * ".dv", con formato 144000 bytes/frame crudo, ".rgb" o ".bw", con dos enteros 
 * de 32 bits en la cabecera que indican el nmero de filas y columnas, y frames
 * de (3 �1)*rows*cols bytes, sin padding, en orden RGB o), o bien un
 * dispositivo de c�ara ("/dev/dv1394" o "/dev/video1394"). Devuelve un
 * entero nico, que hace las veces de descriptor de la c�ara abierta (y que
 * coincide con el descriptor del fichero/dispositivo asociado). */
int mycvOpenCamera(char *filename);

/* Funci� para poner la captura de una c�ara en modo BW/RGB y con un tama�
 * en filas y columnas determinado: */
void mycvSetModeCamera(int camera,int mode,int rows,int cols);

/* Funci� que devuelve un puntero a una zona de memoria con la imagen 
 * solicitada, del tama� solicitado. Tambi� actualiza el par�etro step_bytes
 * con el paso correspondiente entre l�eas de la imagen, en bytes (podr� no
 * coincidir con el nmero de columnas multiplicado por el nmero de canales
 * (3 en RGB o 1 en gray), debido al padding que realiza la librer� IPP para
 * aumentar la eficiencia de las funciones asociadas a im�enes): */
Ipp8u *mycvGetFrameCamera(int camera,int *step_bytes);

/* Funci� que cierra una c�ara abierta con anterioridad: */
void mycvCloseCamera(int camera);

/* Funci� que obtiene informaci� de una c�ara abierta dada: */
void mycvGetInfoCamera(int camera,TInfoCam *info_cam);

/* Funci� que coloca una c�ara en un frame determinado (s�o si no es una 
 * c�ara "live", y si el frame pedido est�entre 0 y max_frames-1, si no avisa
 * del error y aborta) , y con una direcci� dada (0->adelante,1->atr�): */
void mycvGoToFrameCamera(int camera,int frame,int dir);

#ifdef __cplusplus
}
#endif

#endif /* _MYCV_CAMERA_H */
