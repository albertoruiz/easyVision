/* Copyright Pedro E. López de Teruel <pedroe@ditec.um.es>
   License: GPL
*/

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

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

#include <libraw1394/raw1394.h>
#include <libdc1394/dc1394_control.h>

#include "mycvCamera.h"

/* Tama� de frame DV en bytes: */
#define SIZE_FRAME_DV 144000

/* Constantes para el proceso de decodificaci�: */
#define LPITCHX (720*2)
#define SIZE_TABLE_A_DEQUANTIZE	64*14*2
#define NBYTESPERPIXEL 2

/* Estructura interna de los descriptores de c�ara: */
typedef struct t_cam_desc {
    /* Tipo de c�ara (DV_CAM_FILE, DV_CAM_LIVE, RGB_CAM_FILE): */
    int type;
    
    /* Descriptor del fichero a leer (un ".dv" o el "/dev/dv1394"): */
    int fd;

    /* Nmero de filas y columnas de la fuente (fichero o dispositivo) y del
     * destino (tama� solicitado por el usuario): */
    int src_rows,src_cols,dst_rows,dst_cols;

    /* Direcci� de recorrido actual del fichero de imagen (s�o para c�aras
     * no live). dir=0 => hacia adelante, dir=1 => hacia atr� (cuando se
     * alcanza el final del fichero, se vuelve sobre los pasos anteriores): */
    int dir;
    
    /* Nmero de frame actual del fichero (0..maxframes-1), frame actual
     * (coincide con cur_frame para lectura continua de "/dev/dv1394",
     * pero no necesariamente en el caso de un fichero, si se empieza a leer
     * hacia atr� al llegar al final), y m�imo nmero de frames
     * almacenados en el fichero (0 para c�eras live): */
    int cur_frame,num_frames,max_frames;

    /* Modo RGB o gray: */
    int mode;

    /* Buffers para im�enes, cuyo espacio deber�reservarse en funci� del
     * tipo de imagen:
     *  - buf_in_DV: para cada frame .dv de entrada (de tama� SIZE_FRAME_DV =
     *                144000 bytes).
     *  - src_img_YUV: para el YUV resultado de la decodificaci�/lectura
     *                IIDC, de tama� DV_ROWS*DV_COLS*2 / IIDC_ROWS*IIDC_COLS*2.
     *  - src_img_BW: para la imagen de grises fuente, de tama�
                     src_rows*src_cols.     
     *  - src_img_RGB: para el RGB fuente, de tama� src_rows*src_cols*3.
     *  - dst_img_RGB: para el RGB pedido por el usuario, de tama�
     *                dst_rows*dst_cols*3.
     *  - dst_img_BW: para la imagen de grises pedida por el usuario, de tama�
     *                dst_rows*dst_cols. */
    unsigned char *buf_in_DV;
    Ipp8u *src_img_YUV,*src_img_BW,*src_img_RGB,*dst_img_RGB,*dst_img_BW;

    /* Paso entre l�eas, en bytes, para cada una de las im�enes: */
    int step_src_YUV,step_src_BW,step_src_RGB,step_dst_RGB,step_dst_BW;
    
    /* Variables de ROI (regi� de inter�) para los procedimientos de imagen
     * (a tama� fuente original y a tama� destino solicitado por usuario),
     * y rect�gulo fuente (para la operaci� resize): */
    IppiSize src_roi_size,dst_roi_size;    
    IppiRect src_rect;

    /* Espacio para 5 macrobloques + alineamiento (s�o c�aras DV): */
    Ipp16s *shortBlocks; 
    Ipp16s shortBlocksNA[5*6*64+16]; 

    /* S�o para c�aras IIDC: */
    raw1394handle_t handle;
    dc1394_cameracapture camera;

    /* Puntero al siguiente descriptor: */
    struct t_cam_desc *sig;
} TCamDesc;

TCamDesc *list_cams=NULL;

/* Tablas de Huffman, de decuantizaci� y auxiliares (una vez inicializadas,
 * valen para todas las c�aras): */
Ipp32s *pHuffT1;
Ipp16s *lpADequantizeTable;
Ipp32s *lpADequantizeLineTable;
Ipp16s *Buffer2;

Ipp32s dvTable1[] =
{
    9, /* Longitud del c�igo */
    1,
    9,
    0, /* 1-bit codes */
    0, /* 2-bit codes */
    2, /* 3-bit codes */
    0x00000000, 0, 1, 0x00000001, 0, -1,
    3, /* 4-bit codes */
    0x00000004, 0, 2, 0x00000005, 0, -2, 0x00000006, 100, 100,
    6, /* 5-bit codes */
    0x0000000e, 1, 1, 0x0000000f, 1, -1, 0x00000010, 0, 3, 0x00000011, 0, -3,
    0x00000012, 0, 4, 0x00000013, 0, -4,
    8, /* 6-bit codes */
    0x00000028, 2, 1, 0x00000029, 2, -1, 0x0000002a, 1, 2, 0x0000002b, 1, -2,
    0x0000002c, 0, 5, 0x0000002d, 0, -5, 0x0000002e, 0, 6, 0x0000002f, 0, -6,
    8, /* 7-bit codes */
    0x00000060, 3, 1, 0x00000061, 3, -1, 0x00000062, 4, 1, 0x00000063, 4, -1,
    0x00000064, 0, 7, 0x00000065, 0, -7, 0x00000066, 0, 8, 0x00000067, 0, -8,

    16, /* 8-bit codes */
    0x000000d0, 5, 1, 0x000000d1, 5, -1, 0x000000d2, 6, 1, 0x000000d3, 6, -1,
    0x000000d4, 2, 2, 0x000000d5, 2, -2, 0x000000d6, 1, 3, 0x000000d7, 1, -3,
    0x000000d8, 1, 4, 0x000000d9, 1, -4, 0x000000da, 0, 9, 0x000000db, 0, -9,
    0x000000dc, 0, 10,0x000000dd, 0, -10,0x000000de, 0, 11,0x000000df, 0, -11,
    32, /* 9-bit codes */
    0x000001c0, 7, 1, 0x000001c1, 7, -1, 0x000001c2, 8, 1, 0x000001c3, 8, -1,
    0x000001c4, 9, 1, 0x000001c5, 9, -1, 0x000001c6, 10, 1,0x000001c7, 10, -1,
    0x000001c8, 3, 2, 0x000001c9, 3, -2, 0x000001ca, 4, 2, 0x000001cb, 4, -2,
    0x000001cc, 2, 3, 0x000001cd, 2, -3, 0x000001ce, 1, 5, 0x000001cf, 1, -5,
    0x000001d0, 1, 6, 0x000001d1, 1, -6, 0x000001d2, 1, 7, 0x000001d3, 1, -7,
    0x000001d4, 0, 12,0x000001d5, 0, -12,0x000001d6, 0, 13,0x000001d7, 0, -13,
    0x000001d8, 0, 14,0x000001d9, 0, -14,0x000001da, 0, 15,0x000001db, 0, -15,
    0x000001dc, 0, 16,0x000001dd, 0, -16,0x000001de, 0, 17,0x000001df, 0, -17,
    -1 /* end of table */
};

Ipp32s dvTable2[] =
{
    9, /* Longitud m�ima del c�igo. */
    2,
    9,
    0, /* 1-bit codes */
    0, /* 2-bit codes */
    0, /* 3-bit codes */
    0, /* 4-bit codes */
    0, /* 5-bit codes */
    32, /* 6-bit codes */
    0x00000000, 11, 1, 0x00000001, 11, -1, 0x00000002, 12, 1, 0x00000003, 12, -1,
    0x00000004, 13, 1, 0x00000005, 13, -1, 0x00000006, 14, 1, 0x00000007, 14, -1,
    0x00000008, 5,  2, 0x00000009, 5 , -2, 0x0000000a, 6 , 2, 0x0000000b, 6 , -2,
    0x0000000c, 3,  3, 0x0000000d, 3 , -3, 0x0000000e, 4 , 3, 0x0000000f, 4 , -3,
    0x00000010, 2, 4 , 0x00000011, 2, -4 , 0x00000012, 2, 5 , 0x00000013, 2, -5,
    0x00000014, 1, 8 , 0x00000015, 1, -8 , 0x00000016, 0, 18, 0x00000017, 0, -18,
    0x00000018, 0, 19, 0x00000019, 0, -19, 0x0000001a, 0, 20, 0x0000001b, 0, -20,
    0x0000001c, 0, 21, 0x0000001d, 0, -21, 0x0000001e, 0, 22, 0x0000001f, 0, -22,
    16, /* 7-bit codes */
    0x00000040, 5, 3, 0x00000041, 5, -3, 0x00000042, 3, 4, 0x00000043, 3, -4,
    0x00000044, 3, 5, 0x00000045, 3, -5, 0x00000046, 2, 6, 0x00000047, 2, -6,
    0x00000048, 1, 9, 0x00000049, 1, -9, 0x0000004a, 1,10, 0x0000004b, 1, -10,
    0x0000004c, 1, 11,0x0000004d, 1, -11,0x0000004e, 0, 0, 0x0000004f, 1, 0,
    16, /* 8-bit codes */
    0x000000a0, 6, 3, 0x000000a1, 6, -3, 0x000000a2, 4, 4, 0x000000a3, 4, -4,
    0x000000a4, 3, 6, 0x000000a5, 3, -6, 0x000000a6, 1,12, 0x000000a7, 1,-12,
    0x000000a8, 1,13, 0x000000a9, 1,-13, 0x000000aa, 1,14, 0x000000ab, 1,-14,
    0x000000ac, 2, 0, 0x000000ad, 3, 0 , 0x000000ae, 4, 0, 0x000000af, 5, 0,
    32+56+6+2, /* 9-bit codes */
    0x00000160, 7, 2, 0x00000161, 7, -2, 0x00000162, 8, 2,  0x00000163, 8, -2,
    0x00000164, 9, 2, 0x00000165, 9, -2, 0x00000166, 10, 2, 0x00000167, 10, -2,
    0x00000168, 7, 3, 0x00000169, 7, -3, 0x0000016a, 8, 3,  0x0000016b, 8, -3,
    0x0000016c, 4, 5, 0x0000016d, 4, -5, 0x0000016e, 3, 7,  0x0000016f, 3, -7,
    0x00000170, 2, 7, 0x00000171, 2, -7, 0x00000172, 2, 8,  0x00000173, 2, -8,
    0x00000174, 2, 9, 0x00000175, 2, -9, 0x00000176, 2, 10, 0x00000177, 2, -10,
    0x00000178, 2, 11,0x00000179, 2, -11,0x0000017a, 1, 15, 0x0000017b, 1, -15,
    0x0000017c, 1, 16,0x0000017d, 1, -16,0x0000017e, 1, 17, 0x0000017f, 1, -17,

    0x00000180+0, 0, 0, 0x00000180+1, 0, 0, /*1*/
    0x00000180+2, 0, 0, 0x00000180+3, 0, 0, 0x00000180+4 , 0 , 0, 0x00000180+5 , 0 , 0, /*1*/
    
    0x00000180+6 , 6 , 0, 0x00000180+7 , 7 , 0, 0x00000180+8 , 8 , 0, 0x00000180+9 , 9 , 0, /*1*/
    0x00000180+10, 10, 0, 0x00000180+11, 11, 0, 0x00000180+12, 12, 0, 0x00000180+13, 13, 0, /*2*/
    0x00000180+14, 14, 0, 0x00000180+15, 15, 0, 0x00000180+16, 16, 0, 0x00000180+17, 17, 0, /*3*/
    0x00000180+18, 18, 0, 0x00000180+19, 19, 0, 0x00000180+20, 20, 0, 0x00000180+21, 21, 0, /*4*/
    0x00000180+22, 22, 0, 0x00000180+23, 23, 0, 0x00000180+24, 24, 0, 0x00000180+25, 25, 0, /*5*/
    0x00000180+26, 26, 0, 0x00000180+27, 27, 0, 0x00000180+28, 28, 0, 0x00000180+29, 29, 0, /*6*/
    0x00000180+30, 30, 0, 0x00000180+31, 31, 0, 0x00000180+32, 32, 0, 0x00000180+33, 33, 0, /*7*/
    0x00000180+34, 34, 0, 0x00000180+35, 35, 0, 0x00000180+36, 36, 0, 0x00000180+37, 37, 0, /*8*/
    0x00000180+38, 38, 0, 0x00000180+39, 39, 0, 0x00000180+40, 40, 0, 0x00000180+41, 41, 0, /*9*/
    0x00000180+42, 42, 0, 0x00000180+43, 43, 0, 0x00000180+44, 44, 0, 0x00000180+45, 45, 0, /*10*/
    0x00000180+46, 46, 0, 0x00000180+47, 47, 0, 0x00000180+48, 48, 0, 0x00000180+49, 49, 0, /*11*/
    0x00000180+50, 50, 0, 0x00000180+51, 51, 0, 0x00000180+52, 52, 0, 0x00000180+53, 53, 0, /*12*/
    0x00000180+54, 54, 0, 0x00000180+55, 55, 0, 0x00000180+56, 56, 0, 0x00000180+57, 57, 0, /*13*/
    0x00000180+58, 58, 0, 0x00000180+59, 59, 0, 0x00000180+60, 60, 0, 0x00000180+61, 61, 0, /*14*/

    0x00000180+62, 0, 0, 0x00000180+63, 0, 0,
    -1 /* end of table */
};

Ipp32u INTERNAL_DEZIGZAG_TABLE[] =
{
     0,  1,  8, 16,  9,  2,  3, 10,
    17, 24, 32, 25, 18, 11,  4,  5,
    12, 19, 26, 33, 40, 48, 41, 34,
    27, 20, 13,  6,  7, 14, 21, 28,
    35, 42, 49, 56, 57, 50, 43, 36,
    29, 22, 15, 23, 30, 37, 44, 51,
    58, 59, 52, 45, 38, 31, 39, 46,
    53, 60, 61, 54, 47, 55, 62, 63,
     0, 32,  1, 33,  8, 40,  2, 34,
     9, 41, 16, 48, 24, 56, 17, 49,
    10, 42,  3, 35,  4, 36, 11, 43,
    18, 50, 25, 57, 26, 58, 19, 51,
    12, 44,  5, 37,  6, 38, 13, 45,
    20, 52, 27, 59, 28, 60, 21, 53,
    14, 46,  7, 39, 15, 47, 22, 54,
    29, 61, 30, 62, 23, 55, 31, 63
};

#define INIT_A_D_E(num,zone0,zone1,zone2,zone3)   \
    lpBuffer[(num)*64 + 0] = 1;                   \
    lpBuffer[(num)*64 + 1] = zone0;               \
    lpBuffer[(num)*64 + 2] = zone0;               \
    lpBuffer[(num)*64 + 3] = zone0;               \
    lpBuffer[(num)*64 + 4] = zone0;               \
    lpBuffer[(num)*64 + 5] = zone0;               \
    lpBuffer[(num)*64 + 6] = zone1;               \
    lpBuffer[(num)*64 + 7] = zone1;               \
    lpBuffer[(num)*64 + 8] = zone1;               \
    lpBuffer[(num)*64 + 9] = zone1;               \
    lpBuffer[(num)*64 + 10] = zone1;              \
    lpBuffer[(num)*64 + 11] = zone1;              \
    lpBuffer[(num)*64 + 12] = zone1;              \
    lpBuffer[(num)*64 + 13] = zone1;              \
    lpBuffer[(num)*64 + 14] = zone1;              \
    lpBuffer[(num)*64 + 15] = zone1;              \
    lpBuffer[(num)*64 + 16] = zone1;              \
    lpBuffer[(num)*64 + 17] = zone1;              \
    lpBuffer[(num)*64 + 18] = zone1;              \
    lpBuffer[(num)*64 + 19] = zone1;              \
    lpBuffer[(num)*64 + 20] = zone1;              \
    lpBuffer[(num)*64 + 21] = zone2;              \
    lpBuffer[(num)*64 + 22] = zone2;              \
    lpBuffer[(num)*64 + 23] = zone2;              \
    lpBuffer[(num)*64 + 24] = zone2;              \
    lpBuffer[(num)*64 + 25] = zone2;              \
    lpBuffer[(num)*64 + 26] = zone2;              \
    lpBuffer[(num)*64 + 27] = zone2;              \
    lpBuffer[(num)*64 + 28] = zone2;              \
    lpBuffer[(num)*64 + 29] = zone2;              \
    lpBuffer[(num)*64 + 30] = zone2;              \
    lpBuffer[(num)*64 + 31] = zone2;              \
    lpBuffer[(num)*64 + 32] = zone2;              \
    lpBuffer[(num)*64 + 33] = zone2;              \
    lpBuffer[(num)*64 + 34] = zone2;              \
    lpBuffer[(num)*64 + 35] = zone2;              \
    lpBuffer[(num)*64 + 36] = zone2;              \
    lpBuffer[(num)*64 + 37] = zone2;              \
    lpBuffer[(num)*64 + 38] = zone2;              \
    lpBuffer[(num)*64 + 39] = zone2;              \
    lpBuffer[(num)*64 + 40] = zone2;              \
    lpBuffer[(num)*64 + 41] = zone2;              \
    lpBuffer[(num)*64 + 42] = zone2;              \
    lpBuffer[(num)*64 + 43] = zone3;              \
    lpBuffer[(num)*64 + 44] = zone3;              \
    lpBuffer[(num)*64 + 45] = zone3;              \
    lpBuffer[(num)*64 + 46] = zone3;              \
    lpBuffer[(num)*64 + 47] = zone3;              \
    lpBuffer[(num)*64 + 48] = zone3;              \
    lpBuffer[(num)*64 + 49] = zone3;              \
    lpBuffer[(num)*64 + 50] = zone3;              \
    lpBuffer[(num)*64 + 51] = zone3;              \
    lpBuffer[(num)*64 + 52] = zone3;              \
    lpBuffer[(num)*64 + 53] = zone3;              \
    lpBuffer[(num)*64 + 54] = zone3;              \
    lpBuffer[(num)*64 + 55] = zone3;              \
    lpBuffer[(num)*64 + 56] = zone3;              \
    lpBuffer[(num)*64 + 57] = zone3;              \
    lpBuffer[(num)*64 + 58] = zone3;              \
    lpBuffer[(num)*64 + 59] = zone3;              \
    lpBuffer[(num)*64 + 60] = zone3;              \
    lpBuffer[(num)*64 + 61] = zone3;              \
    lpBuffer[(num)*64 + 62] = zone3;              \
    lpBuffer[(num)*64 + 63] = zone3;

#define INTERNAL_CS1 0.980785	/* cos(1*PI/16) */
#define INTERNAL_CS2 0.923880	/* cos(2*PI/16) */
#define INTERNAL_CS3 0.831470	/* cos(3*PI/16) */
#define INTERNAL_CS4 0.707107	/* cos(4*PI/16) */
#define INTERNAL_CS5 0.555570	/* cos(5*PI/16) */
#define INTERNAL_CS6 0.382683	/* cos(6*PI/16) */
#define INTERNAL_CS7 0.195090	/* cos(7*PI/16) */

#define W0 (1.0000001)
#define W1 (INTERNAL_CS4 / (4.0 * INTERNAL_CS7 * INTERNAL_CS2))
#define W2 (INTERNAL_CS4 / (2.0 * INTERNAL_CS6))
#define W3 (1.0 / (2.0 * INTERNAL_CS5))
#define W4 (7.0 / 8.0)
#define W5 (INTERNAL_CS4 / INTERNAL_CS3)
#define W6 (INTERNAL_CS4 / INTERNAL_CS2)
#define W7 (INTERNAL_CS4 / INTERNAL_CS1)

int DVCreateADequantizeTable()
{
    int i,j;
	short int lpBuffer[14*64],*lpBuffer1;
	unsigned int tabl1[] = {
      0,  1,  8,  16,  9,  2,  3, 10,
      17, 24, 32, 25, 18, 11,  4,  5,
      12, 19, 26, 33, 40, 48, 41, 34,
      27, 20, 13,  6,  7, 14, 21, 28,
      35, 42, 49, 56, 57, 50, 43, 36,
      29, 22, 15, 23, 30, 37, 44, 51,
      58, 59, 52, 45, 38, 31, 39, 46,
      53, 60, 61, 54, 47, 55, 62, 63 };
	unsigned int tabl2[] = {
       0, 32,  1, 33,  8, 40,  2, 34,
       9, 41, 16, 48, 24, 56, 17, 49,
      10, 42,  3, 35,  4, 36, 11, 43,
      18, 50, 25, 57, 26, 58, 19, 51,
      12, 44,  5, 37,  6, 38, 13, 45,
      20, 52, 27, 59, 28, 60, 21, 53,
      14, 46,  7, 39, 15, 47, 22, 54,
      29, 61, 30, 62, 23, 55, 31, 63 };
	int *lineTable,lineTable1[] = {
        5,  5,  4, 4, 3,  3,  2, 2, 1, 0, 0, 0, 0,  0, 0, 0,
        7,  6,  6, 5, 5,  4,  4, 3, 3, 2, 2, 1, 0,  0, 0, 0,
        8,  8,  7, 7, 6,  6,  5, 5, 4, 4, 3, 3, 2,  2, 1, 0,
       13, 12, 12, 8, 8, 11, 11, 6, 6, 5, 5, 4, 4, 10, 9, 9 };
	static double TablW0[] = {
       (0.5),(W0*W1/2),(W0*W2/2),(W0*W3/2),(W0*W4/2),(W0*W5/2),(W0*W6/2),(W0*W7/2),
       (W1*W0/2),(W1*W1/2),(W1*W2/2),(W1*W3/2),(W1*W4/2),(W1*W5/2),(W1*W6/2),(W1*W7/2),
       (W2*W0/2),(W2*W1/2),(W2*W2/2),(W2*W3/2),(W2*W4/2),(W2*W5/2),(W2*W6/2),(W2*W7/2),
       (W3*W0/2),(W3*W1/2),(W3*W2/2),(W3*W3/2),(W3*W4/2),(W3*W5/2),(W3*W6/2),(W3*W7/2),
       (W4*W0/2),(W4*W1/2),(W4*W2/2),(W4*W3/2),(W4*W4/2),(W4*W5/2),(W4*W6/2),(W4*W7/2),
       (W5*W0/2),(W5*W1/2),(W5*W2/2),(W5*W3/2),(W5*W4/2),(W5*W5/2),(W5*W6/2),(W5*W7/2),
       (W6*W0/2),(W6*W1/2),(W6*W2/2),(W6*W3/2),(W6*W4/2),(W6*W5/2),(W6*W6/2),(W6*W7/2),
       (W7*W0/2),(W7*W1/2),(W7*W2/2),(W7*W3/2),(W7*W4/2),(W7*W5/2),(W7*W6/2),(W7*W7/2) };
	static double TablW1[] = {
       (0.5),(W0*W1/2),(W0*W2/2),(W0*W3/2),(W0*W4/2),(W0*W5/2),(W0*W6/2),(W0*W7/2),
       (W2*W0/2),(W2*W1/2),(W2*W2/2),(W2*W3/2),(W2*W4/2),(W2*W5/2),(W2*W6/2),(W2*W7/2),
       (W4*W0/2),(W4*W1/2),(W4*W2/2),(W4*W3/2),(W4*W4/2),(W4*W5/2),(W4*W6/2),(W4*W7/2),
       (W6*W0/2),(W6*W1/2),(W6*W2/2),(W6*W3/2),(W6*W4/2),(W6*W5/2),(W6*W6/2),(W6*W7/2),
       (W0*W0/2),(W0*W1/2),(W0*W2/2),(W0*W3/2),(W0*W4/2),(W0*W5/2),(W0*W6/2),(W0*W7/2),
       (W2*W0/2),(W2*W1/2),(W2*W2/2),(W2*W3/2),(W2*W4/2),(W2*W5/2),(W2*W6/2),(W2*W7/2),
       (W4*W0/2),(W4*W1/2),(W4*W2/2),(W4*W3/2),(W4*W4/2),(W4*W5/2),(W4*W6/2),(W4*W7/2),
       (W6*W0/2),(W6*W1/2),(W6*W2/2),(W6*W3/2),(W6*W4/2),(W6*W5/2),(W6*W6/2),(W6*W7/2)
	};

	lpADequantizeLineTable = 0;
	lpADequantizeTable = 0;

	/* Pedir memoria alineada: */
	Buffer2 = (short int *) malloc(SIZE_TABLE_A_DEQUANTIZE*2 + 0x10);
	lpBuffer1 = (short*)(((int)(Buffer2) + 0xf) & 0xfffffff0);
	lineTable = (int *) malloc(1024);
	for(i=0;i<64;i+=2) {
		lineTable[i] = lineTable1[i];
    }
	for(i=1;i<64;i+=2) {
		lineTable[i] = lineTable1[i];
    }

	if(0 == lpBuffer1) {
		return -1;
    }

    INIT_A_D_E(0,1,1,1,1)
    INIT_A_D_E(1,1,1,1,2)
    INIT_A_D_E(2,1,1,2,2)
    INIT_A_D_E(3,1,2,2,4)
    INIT_A_D_E(4,2,2,4,4)
    INIT_A_D_E(5,2,4,4,8)
    INIT_A_D_E(6,4,4,8,8)
    INIT_A_D_E(7,4,8,8,16)
    INIT_A_D_E(8,8,8,16,16)
    INIT_A_D_E(9,2,2,2,2)  /*0*/
    INIT_A_D_E(10,2,2,2,4) /*1*/
    /*2-4*/
    /*3-5*/
    /*4-6*/
    INIT_A_D_E(11,4,8,8,16)/*5*/
    /*6-8*/
    INIT_A_D_E(12,8,16,16,32)/*7*/
    INIT_A_D_E(13,16,16,32,32)/*8*/

    for(j=0;j<14;j++) {
        for(i=0;i<64;i++) {
        lpBuffer1[j*64 + tabl1[i]] = 
            lpBuffer[j*64 + i] * (int)(((double) (1 << 8 /*13*/))  / 
              TablW0[tabl1[i]]);
        }
    }

	for(j=0;j<14;j++) {
		for(i=0;i<64;i++) {
			lpBuffer1[64 * 14 + j * 64 + tabl2[i]] =
              lpBuffer[j* 64 + i]  *(int)(((double) (1 << 8 /*13*/))  / 
                TablW1[tabl2[i]]);
        }
    }

	lpADequantizeLineTable = lineTable;
	lpADequantizeTable = lpBuffer1;
	return 0;
} 

Ipp8s sBYTE_OF(Ipp16s sData)
{
    if (255 < sData)
        return 255;
    if (0 > sData)
        return sData;
    return (Ipp8s) sData;

}

/* Esta funci�, a trav� de WRITE_QUAD_2, "simula" una c�ara no entrelazada, repitiendo
 * el campo (field) 1 en lugar de colocar el campo (field) 2. */
void Fill625_UYVY_P1(Ipp32u *lpnArray)
{
    Ipp16s *lpsSrc, *lpsSrcV;
    Ipp8u *lpcDst;
    int i, j, k;

#define WRITE_QUAD(num, y_blk, y_num, u_num, v_num)                        \
    lpcDst[(num) * 4 + 0] = sBYTE_OF(lpsSrc[64 * y_blk + y_num] + 128);    \
    lpcDst[(num) * 4 + 1] = sBYTE_OF(lpsSrcV[64 * 1 + u_num] + 128);       \
    lpcDst[(num) * 4 + 2] = sBYTE_OF(lpsSrc[64 * y_blk + y_num + 1] + 128);\
    lpcDst[(num) * 4 + 3] = sBYTE_OF(lpsSrcV[64 * 0 + v_num] + 128);

#define WRITE_QUAD_2(num, y_blk, y_num, u_num, v_num)                        \
    lpcDst[(num) * 4 + 0] = lpcDst[(num) * 4 + 0 - LPITCHX];    \
    lpcDst[(num) * 4 + 1] = lpcDst[(num) * 4 + 1 - LPITCHX];    \
    lpcDst[(num) * 4 + 2] = lpcDst[(num) * 4 + 2 - LPITCHX];    \
    lpcDst[(num) * 4 + 3] = lpcDst[(num) * 4 + 3 - LPITCHX]; 

    lpsSrc = (Ipp16s *) lpnArray[0];
    lpsSrcV = lpsSrc + 64 * 4;
    lpcDst = (Ipp8u *) lpnArray[1];
    lpnArray += 2;

    while((lpsSrc) && (lpcDst)) {
        for(i=0;i<2;i++) {
            for(k=0;k<2;k++) {
                for(j=0;j<2;j++) {
                    WRITE_QUAD(0,0,0,0,0)
                    WRITE_QUAD(1,0,2,1,1)
                    WRITE_QUAD(2,0,4,2,2)
                    WRITE_QUAD(3,0,6,3,3) 

                    WRITE_QUAD(4,1,0,4,4)
                    WRITE_QUAD(5,1,2,5,5)
                    WRITE_QUAD(6,1,4,6,6)
                    WRITE_QUAD(7,1,6,7,7)

                    lpcDst += LPITCHX;
                    lpsSrc += 8;
                    lpsSrcV += 8;

                    WRITE_QUAD_2(0,0,0,0,0)
                    WRITE_QUAD_2(1,0,2,1,1)
                    WRITE_QUAD_2(2,0,4,2,2)
                    WRITE_QUAD_2(3,0,6,3,3)

                    WRITE_QUAD_2(4,1,0,4,4)
                    WRITE_QUAD_2(5,1,2,5,5)
                    WRITE_QUAD_2(6,1,4,6,6)
                    WRITE_QUAD_2(7,1,6,7,7)

                    lpcDst += LPITCHX;
                    lpsSrc += 8;
                    lpsSrcV -= 8;
                }
                lpsSrcV += 16;
            }
            lpsSrc += 64;
        }

        lpsSrc = (Ipp16s *) lpnArray[0];
        lpsSrcV = lpsSrc + 64 * 4;
        lpcDst = (Ipp8u *) lpnArray[1];
        lpnArray += 2;
    }
}

/* OJO: Esta era la funci� original: el problema es que en la imagen se aprecia el
 * entrelazado, as�que usamos la anterior, en realidad. */
void Fill625_UYVY_P1_normal(Ipp32u *lpnArray)
{
    Ipp16s *lpsSrc, *lpsSrcV;
    Ipp8u *lpcDst;
    int i, j, k;

#define WRITE_QUAD(num, y_blk, y_num, u_num, v_num)                        \
    lpcDst[(num) * 4 + 0] = sBYTE_OF(lpsSrc[64 * y_blk + y_num] + 128);    \
    lpcDst[(num) * 4 + 1] = sBYTE_OF(lpsSrcV[64 * 1 + u_num] + 128);       \
    lpcDst[(num) * 4 + 2] = sBYTE_OF(lpsSrc[64 * y_blk + y_num + 1] + 128);\
    lpcDst[(num) * 4 + 3] = sBYTE_OF(lpsSrcV[64 * 0 + v_num] + 128);

    lpsSrc = (Ipp16s *) lpnArray[0];
    lpsSrcV = lpsSrc + 64 * 4;
    lpcDst = (Ipp8u *) lpnArray[1];
    lpnArray += 2;

    while((lpsSrc) && (lpcDst)) {
        for(i=0;i<2;i++) {
            for(k=0;k<2;k++) {
                for(j=0;j<2;j++) {
                    /* Bloque y0 */
                    WRITE_QUAD(0,0,0,0,0)
                    WRITE_QUAD(1,0,2,1,1)
                    WRITE_QUAD(2,0,4,2,2)
                    WRITE_QUAD(3,0,6,3,3)

                    /* Bloque y1 */
                    WRITE_QUAD(4,1,0,4,4)
                    WRITE_QUAD(5,1,2,5,5)
                    WRITE_QUAD(6,1,4,6,6)
                    WRITE_QUAD(7,1,6,7,7)

                    lpcDst += LPITCHX;
                    lpsSrc += 8;
                    lpsSrcV += 8;

                    /* Bloque y0 */
                    WRITE_QUAD(0,0,0,0,0)
                    WRITE_QUAD(1,0,2,1,1)
                    WRITE_QUAD(2,0,4,2,2)
                    WRITE_QUAD(3,0,6,3,3)

                    /* Bloque y1 */
                    WRITE_QUAD(4,1,0,4,4)
                    WRITE_QUAD(5,1,2,5,5)
                    WRITE_QUAD(6,1,4,6,6)
                    WRITE_QUAD(7,1,6,7,7)

                    lpcDst += LPITCHX;
                    lpsSrc += 8;
                    lpsSrcV -= 8;
                }
                lpsSrcV += 16;
            }
            lpsSrc += 64;
        }

        lpsSrc = (Ipp16s *) lpnArray[0];
        lpsSrcV = lpsSrc + 64 * 4;
        lpcDst = (Ipp8u *) lpnArray[1];
        lpnArray += 2;
    }
}


int GetOffsetMB_PAL(int a)
{
    int col,row;

    col = a/3;
    if (col & 1) {
        row = 2 - (a - col*3);
    } else {
        row = a - col*3;
    }
    return LPITCHX * 16 * row + col * 16 * 2;
}

void Init625_UYVY(TCamDesc* cam_desc,Ipp32u *nArray,
                  Ipp8u *lpcDestination, int i, int k)
{
    int nOffsetMBInSB;

    nOffsetMBInSB = GetOffsetMB_PAL(k);

    nArray[0] = (Ipp32u)(cam_desc->shortBlocks + 64 * 6 * 0);
    nArray[1] = (Ipp32u)(lpcDestination +               /* Destination + */
                        ((i + 2) % 12) * LPITCHX * 48 + /* offset of needed SB line + */
                        (2) * 16 * NBYTESPERPIXEL * 9 + /* offset of needed SB + */
                         nOffsetMBInSB);                /* position MB in SB */

    nArray[2] = (Ipp32u)(cam_desc->shortBlocks + 64 * 6 * 1);
    nArray[3] = (Ipp32u)(lpcDestination +               /* Destination + */
                        ((i + 6) % 12) * LPITCHX * 48 + /* offset of needed SB line + */
                        (1) * 16 * NBYTESPERPIXEL * 9 + /* offset of needed SB + */
                         nOffsetMBInSB);                /* position MB in SB */

    nArray[4] = (Ipp32u)(cam_desc->shortBlocks + 64 * 6 * 2);
    nArray[5] = (Ipp32u)(lpcDestination +               /* Destination + */
                        ((i + 8) % 12) * LPITCHX * 48 + /* offset of needed SB line + */
                        (3) * 16 * NBYTESPERPIXEL * 9 + /* offset of needed SB + */
                         nOffsetMBInSB);                /* position MB in SB */

    nArray[6] = (Ipp32u)(cam_desc->shortBlocks + 64 * 6 * 3);
    nArray[7] = (Ipp32u)(lpcDestination +               /* Destination + */
                        ((i + 0) % 12) * LPITCHX * 48 + /* offset of needed SB line + */
                        (0) * 16 * NBYTESPERPIXEL * 9 + /* offset of needed SB + */
                         nOffsetMBInSB);                /* position MB in SB */

    nArray[8] = (Ipp32u)(cam_desc->shortBlocks + 64 * 6 * 4);
    nArray[9] = (Ipp32u)(lpcDestination +               /* Destination + */
                        ((i + 4) % 12) * LPITCHX * 48 + /* offset of needed SB line + */
                        (4) * 16 * NBYTESPERPIXEL * 9 + /* offset of needed SB + */
                         nOffsetMBInSB);                /* position MB in SB */
} 

void PutV625_UYVY_PX(TCamDesc *cam_desc,Ipp8u *surfe, int i, int k)
{
    unsigned int nArray[12];

    Init625_UYVY(cam_desc,nArray,surfe,i,k);
    nArray[0x0a] = nArray[0x0b] = (unsigned int) 0;
    Fill625_UYVY_P1(nArray);
}

void inline DecompressSegment(TCamDesc* cam_desc,Ipp8u *lpSrc)
{
    Ipp32u cBuffer[30];
    Ipp32u i,j,uu2,qno;
    Ipp16s *lpsTableKvadr;

    memset(cam_desc->shortBlocks,0,sizeof(Ipp16s)*64*5*6);
    if(ippiHuffmanDecodeSegment_DV_8u16s((Ipp8u *) lpSrc, 
                                      (Ipp32u *) INTERNAL_DEZIGZAG_TABLE,
                                      (Ipp32u *) pHuffT1, 
                                      (Ipp16s *) cam_desc->shortBlocks, 
                                      (Ipp32u *) cBuffer) !=
                                      ippStsNoErr) {
        printf("Error en ippiHuffmanDecodeSegment_DV_8u16s\n");
    	exit(1);
    } 
    
    for(j=0;j<5;j++) {
        qno = (Ipp32s)(*(((Ipp16u *)cBuffer) + (j+j+j)*4 + 1) & 0x0f);
        for(i=0;i<6;i++) {
            uu2 = (cBuffer[j*6+i]) & 0x00030;
            lpsTableKvadr = lpADequantizeTable + 
	                    64*lpADequantizeLineTable[qno+uu2] + 
                            (cBuffer[i+6*j] & 0x040)*14;
            ippiQuantInv_DV_16s_C1I(cam_desc->shortBlocks +
                                    64*(i+6*j),lpsTableKvadr);
            if ((0x40 & (cBuffer[i+6*j])) == 0) {
                ippiDCT8x8Inv_16s_C1I(cam_desc->shortBlocks + 64*(i+6*j));
            } else {
                ippiDCT2x4x8Inv_16s_C1I(cam_desc->shortBlocks + 64*(i+6*j));
            }
        }
    }
}

void DecodeDVFrame(TCamDesc *cam_desc,Ipp8u *lpCfr, Ipp8u *surfe) 
{
    Ipp32u indexSrc = 150;
    int i,k;

    for(i=0;i<12;i++) { /* 12 veces, una por pista (PAL) */
        for(k=0;k<27;k++) { /* 27 macrobloques en un superbloque. */
            /* Check for video block (see blue book) */
            if (6==(indexSrc & 0x0f)) {
                if(150 != indexSrc) { /* Saltar el audio: */
                    indexSrc += 1;
                } else { /* Nueva secuencia: */
                    indexSrc = 7;
                }
            }

            DecompressSegment(cam_desc,lpCfr + indexSrc*80 + 150*80*i);
            PutV625_UYVY_PX(cam_desc,(Ipp8u *)surfe, i, k);

            indexSrc += 5;
        }
    }
}

void InitDVDecoder(void)
{

Ipp32s **pHuffT1bis = &pHuffT1;

    if(ippiInitAllocHuffmanTable_DV_32u((Ipp32s *)dvTable1,
                                       (Ipp32s *)dvTable2,
                                       (Ipp32u **)pHuffT1bis) != ippStsNoErr) {
        printf("Error en ippiInitAllocHuffmanTable_DV_32u(...)\n");
        exit(1);
    }
    if (DVCreateADequantizeTable()) {
        printf("Error en DVCreateADequantizeTable(...)\n");
        exit(1);
    }
}

void CloseDVDecoder(void) {
    ippiFreeHuffmanTable_DV_32u((Ipp32u *)pHuffT1);
    free(Buffer2);
    free(lpADequantizeLineTable);
}

/***************** FUNCIONES DE INTERFAZ CON EL USUARIO: *********************/

int mycvOpenCamera(char *filename)
{
    struct stat buf;
    TCamDesc *cam_desc;
    int len;
    
    /* Inicializaci� del decodificador, s�o para la primera c�ara.
     * NOTA: se inicializa aunque no sea una c�ara DV, pero no supone
     * carga computacional importante, y simplifica mucho la programaci�: */
    if(list_cams==NULL) {
        InitDVDecoder();
    }

    /* Creamos un nuevo descriptor, y lo a�dimos al principio de la lista: */
    cam_desc = (TCamDesc*) malloc(sizeof(TCamDesc));
    cam_desc->sig = list_cams;
    list_cams = cam_desc;

    /* Vemos tipo de entrada, segn nombre de fichero: */
    len = strlen(filename);
    if(!strcmp(filename,"/dev/video1394")) {
        cam_desc->type = IIDC_CAM_LIVE;
    } else if(!strcmp(filename,"/dev/dv1394")) {
        cam_desc->type = DV_CAM_LIVE;
    } else if((len>=4) && (filename[len-3]=='.') &&
              ((filename[len-2]|0x20)=='d') &&
              ((filename[len-1]|0x20)=='v')) {
        cam_desc->type = DV_CAM_FILE;                
    } else if((len>=5) && (filename[len-4]=='.') &&
              ((filename[len-3]|0x20)=='r') &&
              ((filename[len-2]|0x20)=='g') &&
              ((filename[len-1]|0x20)=='b')) {
        cam_desc->type = RGB_CAM_FILE;                
    } else if((len>=4) && (filename[len-3]=='.') &&
              ((filename[len-2]|0x20)=='b') &&
              ((filename[len-1]|0x20)=='w')) {
        cam_desc->type = BW_CAM_FILE;                
    } else {
        printf("Nombre de fichero no válido (%s)\n",filename);
        exit(1);
    }
    
    /* Abre fichero de entrada (podr� ser el dispositivo /dev/dv1394): */
    if((cam_desc->fd = open(filename,O_RDONLY)) == -1) {
        printf("Error al abrir fichero (%s)\n",filename);
        exit(1);
    }

    if(cam_desc->type == IIDC_CAM_LIVE) {
        /* Hay que utilizar la librer� dc1394: */
        if((cam_desc->handle = dc1394_create_handle(0))==NULL){
           printf("Error al adquirir manejador raw1394 (puerto 0) en IIDC\n");
           exit(1);
        }           
        cam_desc->camera.node = 0;
        /* Reducimos el nmero de frames del anillo de captura DMA a 2, para
         * evitar retardos excesivos: */
        if(dc1394_dma_setup_capture(cam_desc->handle,cam_desc->camera.node,0, 
  			                        FORMAT_VGA_NONCOMPRESSED,
                                    MODE_640x480_YUV422,
                                    SPEED_400,
                                    FRAMERATE_30, 2, 1, /* eliminado en FC3: 1, */
                                    "/dev/video1394",
                                    &(cam_desc->camera))!= DC1394_SUCCESS){
     	    printf("Error al establecer captura DMA en IIDC\n");
	        exit(1);
        }

        if(dc1394_start_iso_transmission(cam_desc->handle,
                                   cam_desc->camera.node) != DC1394_SUCCESS) {
            printf("No se puede empezar la transmisión ISO en IIDC\n");
            //dc1394_release_camera(cam_desc->handle,&(cam_desc->camera));
            dc1394_destroy_handle(cam_desc->handle);
            exit(1);
        }
        
        cam_desc->src_rows = (cam_desc->dst_rows = IIDC_ROWS);
        cam_desc->src_cols = (cam_desc->dst_cols = IIDC_COLS);           
        cam_desc->max_frames = 0; /* Lo fijamos a 0 en IIDC_CAM_LIVE. */
        
    } else { 
        /* Calcula tama�, para calcular el nmero de frames y ver si el tama�
         * es coherente con el tipo de fichero. Al tiempo, inicializamos los
         * tama�s de filas y columnas fuente y destino (�te ltimo por defecto
         * es igual al fuente al principio): */
        fstat(cam_desc->fd,&buf);
        if((cam_desc->type == DV_CAM_LIVE) || (cam_desc->type == DV_CAM_FILE)) {
            if((buf.st_size % SIZE_FRAME_DV) != 0) {
                printf("Tamaño de fichero .dv erróneo (%s)\n",filename);
                exit(1);
            } else {
                cam_desc->src_rows = (cam_desc->dst_rows = DV_ROWS);
                cam_desc->src_cols = (cam_desc->dst_cols = DV_COLS);           
                cam_desc->max_frames = buf.st_size/SIZE_FRAME_DV; 
                /* Es 0 en DV_CAM_LIVE, justo lo que queremos. */
            }
        } else if(cam_desc->type == RGB_CAM_FILE) {
            /* Filas y columnas se leen de la cabecera del fichero: */
            read(cam_desc->fd,&(cam_desc->src_rows),sizeof(int));
            read(cam_desc->fd,&(cam_desc->src_cols),sizeof(int));
            cam_desc->dst_rows = cam_desc->src_rows;
            cam_desc->dst_cols = cam_desc->src_cols;
            if(((buf.st_size-2*sizeof(int)) %
               (3*(cam_desc->src_rows)*(cam_desc->src_cols))) != 0) {
                printf("Tamaño de fichero .rgb erróneo (%s,%d x %d)\n",
                       filename,cam_desc->src_rows,cam_desc->src_cols);         
                exit(1);       
            } else {
                cam_desc->max_frames = (buf.st_size-2*sizeof(int)) /
                    (3*(cam_desc->src_rows)*(cam_desc->src_cols));
            }        
        } else if(cam_desc->type == BW_CAM_FILE) {
            /* Filas y columnas se leen de la cabecera del fichero: */
            read(cam_desc->fd,&(cam_desc->src_rows),sizeof(int));
            read(cam_desc->fd,&(cam_desc->src_cols),sizeof(int));
            cam_desc->dst_rows = cam_desc->src_rows;
            cam_desc->dst_cols = cam_desc->src_cols;
            if(((buf.st_size-2*sizeof(int)) %
               ((cam_desc->src_rows)*(cam_desc->src_cols))) != 0) {
                printf("Tamaño de fichero .bw erróneo (%s,%d x %d)\n",
                       filename,cam_desc->src_rows,cam_desc->src_cols);           
                exit(1);       
            } else {
                cam_desc->max_frames = (buf.st_size-2*sizeof(int)) /
                    ((cam_desc->src_rows)*(cam_desc->src_cols));
            }        
        }
    }
    
    /* Comenzamos hacia adelante: */
    cam_desc->dir = 0;

    /* Inicializaci� de contadores de frames: */
    cam_desc->cur_frame = 0;
    cam_desc->num_frames = 0;

    /* Modo imagen (Gray/RGB) por defecto (BW s�o para ficheros BW_CAM_FILE;
     * para el resto de casos, RGB): */
    if(cam_desc->type == BW_CAM_FILE) {
        cam_desc->mode = BW_MODE;
    } else {
        cam_desc->mode = RGB_MODE;
    }

    /* Inicializaci� de ROIs fuente y destino, y rect�gulo fuente (para
     * la operaci� resize): */
    cam_desc->src_roi_size.width = cam_desc->src_cols;
    cam_desc->src_roi_size.height = cam_desc->src_rows;
    cam_desc->dst_roi_size.width = cam_desc->dst_cols;
    cam_desc->dst_roi_size.height = cam_desc->dst_rows;
    cam_desc->src_rect.x = 0;
    cam_desc->src_rect.y = 0;
    cam_desc->src_rect.width = cam_desc->src_cols;
    cam_desc->src_rect.height = cam_desc->src_rows;

    /* Reserva de espacio para los buffers de imagen (con la actualizaci� de 
     * los correspondientes pasos entre l�eas): */
    if((cam_desc->type == DV_CAM_LIVE)||(cam_desc->type == DV_CAM_FILE)) {
        cam_desc->buf_in_DV = 
          (unsigned char*) malloc(SIZE_FRAME_DV*sizeof(unsigned char));
        cam_desc->src_img_YUV = (Ipp8u *)
            ippiMalloc_8u_C2(cam_desc->src_cols,cam_desc->src_rows,
                             &(cam_desc->step_src_YUV));    
    } else if (cam_desc->type == IIDC_CAM_LIVE) {
        cam_desc->src_img_YUV = (Ipp8u *)
            ippiMalloc_8u_C2(cam_desc->src_cols,cam_desc->src_rows,
                             &(cam_desc->step_src_YUV));        
    } else {
        cam_desc->buf_in_DV = NULL;
        cam_desc->src_img_YUV = NULL;    
    }

    if(cam_desc->type == BW_CAM_FILE) {
        /* Reservamos sitio para el BW fuente: */
        cam_desc->src_img_BW = (Ipp8u *)
            ippiMalloc_8u_C1(cam_desc->src_cols,cam_desc->src_rows,
                             &(cam_desc->step_src_BW));    
        /* Por ahora, dst_img_BW coincidir�con src_img_BW, ya que coinciden
         * las filas y las columnas: */
        cam_desc->dst_img_BW = cam_desc->src_img_BW;
        cam_desc->step_dst_BW = cam_desc->step_src_BW;
        /* Ponemos src_img_RGB y dst_img_RGB tambi� a NULL: */
        cam_desc->src_img_RGB = NULL;
        cam_desc->dst_img_RGB = NULL;
    } else {
        /* Reservamos sitio para el RGB fuente: */
        cam_desc->src_img_RGB = (Ipp8u *)
            ippiMalloc_8u_C3(cam_desc->src_cols,cam_desc->src_rows,
                             &(cam_desc->step_src_RGB));    
        /* Por ahora, dst_img_RGB coincidir�con src_img_RGB, ya que coinciden
         * las filas y las columnas: */
        cam_desc->dst_img_RGB = cam_desc->src_img_RGB;
        cam_desc->step_dst_RGB = cam_desc->step_src_RGB;
        /* Ponemos src_img_BW y dst_img_BW tambi� a NULL: */
        cam_desc->src_img_BW = NULL;
        cam_desc->dst_img_BW = NULL;
    }

    /* Alineamiento de buffer auxiliar (s�o para decodificaci� DV): */
    if((cam_desc->type == DV_CAM_LIVE)||(cam_desc->type == DV_CAM_FILE)) {
        cam_desc->shortBlocks = 
            (short *)(((unsigned int)cam_desc->shortBlocksNA + 15) & (~15));
    }            

    /* Devolvemos el propio descriptor de fichero como identificador de 
     * la c�ara abierta: */
    return cam_desc->fd;
}

void mycvSetModeCamera(int camera,int mode,int rows,int cols)
{
    TCamDesc *cam_desc;
    int old_mode,old_rows,old_cols;
    
    /* Buscamos el descriptor en la lista de c�aras: */
    cam_desc = list_cams;
    while(cam_desc != NULL) {
        if(cam_desc->fd == camera) {
            break;
        } else {
            cam_desc = cam_desc->sig;
        }
    }    
    if(cam_desc == NULL) {
        printf("Descriptor de cámara inválido (%d) en mycvSetModeCamera\n",
               camera);
        exit(1);
    }

    /* Nuevo modo de c�ara: */
    if((mode != RGB_MODE) && (mode != BW_MODE)) {
        printf("Modo de cámara inválido (%d) en mycvSetModeCamera\n",mode);
        exit(1);    
    } else if((mode==RGB_MODE) && (cam_desc->type==BW_CAM_FILE)) {
        printf("RGB_MODE no permitido para BW_CAM_FILE en mycvSetModeCamera\n");
        exit(1);        
    } else {
        old_mode = cam_desc->mode;
        cam_desc->mode = mode;
    }
    
    /* Nuevo tama� destino: */
    if((rows<=0)||(rows>MAX_ROWS)||(cols<=0)||(cols>MAX_COLS)) {
       printf("Tamaño inválido (%d x %d) en mycvSetModeCamera\n",rows,cols);
        exit(1);        
    } else {
        old_rows = cam_desc->dst_rows;
        old_cols = cam_desc->dst_cols;
        cam_desc->dst_rows = rows;
        cam_desc->dst_cols = cols;
    }

    /* Reinicializaci� de ROI destino: */
    cam_desc->dst_roi_size.width = cols;
    cam_desc->dst_roi_size.height = rows;

    /* Reasignaci� de espacio para los buffers de imagen fuente y destino 
     * (con la actualizaci� de los correspondientes pasos entre l�eas): */
    if((old_mode==RGB_MODE)&&((old_rows!=cam_desc->src_rows)||
                              (old_cols!=cam_desc->src_cols))) {
        ippiFree(cam_desc->src_img_RGB);
        ippiFree(cam_desc->dst_img_RGB);
    } else if((old_mode==BW_MODE)&&((old_rows!=cam_desc->src_rows)||
                                    (old_cols!=cam_desc->src_cols))) {
        ippiFree(cam_desc->src_img_BW);
        ippiFree(cam_desc->dst_img_BW);
    }
    if(mode==RGB_MODE) {
        /* Primero hace falta espacio para la fuente en RGB: */
        cam_desc->src_img_RGB = (Ipp8u *)
            ippiMalloc_8u_C3(cam_desc->src_cols,cam_desc->src_rows,
                             &(cam_desc->step_src_RGB));    

        /* Luego, quiz� tambi� para el destino en RGB: */
        if((rows!=cam_desc->src_rows)||(cols!=cam_desc->src_cols)) {
            cam_desc->dst_img_RGB = (Ipp8u *)
                ippiMalloc_8u_C3(cam_desc->dst_cols,cam_desc->dst_rows,
                               &(cam_desc->step_dst_RGB));            
        } else {
            cam_desc->dst_img_RGB = cam_desc->src_img_RGB;
            cam_desc->step_dst_RGB = cam_desc->step_src_RGB;
        }
        cam_desc->dst_img_BW = NULL;        
    } else { /* BW_MODE */
        /* Primero hace falta espacio para la fuente en BW: */
        cam_desc->src_img_BW = (Ipp8u *)
            ippiMalloc_8u_C1(cam_desc->src_cols,cam_desc->src_rows,
                             &(cam_desc->step_src_BW));    

        /* Luego, quiz� tambi� para el destino en BW: */
        if((rows!=cam_desc->src_rows)||(cols!=cam_desc->src_cols)) {
            cam_desc->dst_img_BW = (Ipp8u *)
                ippiMalloc_8u_C1(cam_desc->dst_cols,cam_desc->dst_rows,
                                 &(cam_desc->step_dst_BW));
        } else {
            cam_desc->dst_img_BW = cam_desc->src_img_BW;
            cam_desc->step_dst_BW = cam_desc->step_src_BW;
        }
        cam_desc->dst_img_RGB = NULL;
    }
}

Ipp8u *mycvGetFrameCamera(int camera,int *step_bytes)
{
    TCamDesc *cam_desc;
    int i,j,l,m;
    
    /* Buscamos el descriptor en la lista de c�aras: */
    cam_desc = list_cams;
    while(cam_desc != NULL) {
        if(cam_desc->fd==camera) {
            break;
        } else {
            cam_desc = cam_desc->sig;
        }
    }    
    if(cam_desc == NULL) {
        printf("Descriptor de cámara inválido (%d) en mycvGetFrameCamera\n",
               camera);
        exit(1);
    }
            
    if(cam_desc->type == DV_CAM_FILE) {
        /* Buscamos frame actual en la posici� correspondiente del 
         * fichero .dv: */
        lseek(cam_desc->fd,cam_desc->cur_frame*SIZE_FRAME_DV,SEEK_SET);
    } else if(cam_desc->type == RGB_CAM_FILE) { 
        /* Buscamos frame actual en la posici� correspondiente del 
         * fichero .rgb: */
        lseek(cam_desc->fd,2*sizeof(int) + cam_desc->cur_frame*3*
                           (cam_desc->src_rows*cam_desc->src_cols),SEEK_SET);
    } else if(cam_desc->type == BW_CAM_FILE) { 
        /* Buscamos frame actual en la posici� correspondiente del 
         * fichero .bw: */
        lseek(cam_desc->fd,2*sizeof(int) + cam_desc->cur_frame*
                           (cam_desc->src_rows*cam_desc->src_cols),SEEK_SET);
    }

    if(cam_desc->type == IIDC_CAM_LIVE) {
        /* Leemos frame YUV: */
        if(dc1394_dma_single_capture(&(cam_desc->camera))!=DC1394_SUCCESS){
            printf("Error al capturar frame DMA en IIDC\n");
            //dc1394_release_camera(cam_desc->handle,&(cam_desc->camera));
            dc1394_destroy_handle(cam_desc->handle);
            exit(1);
        }
        /* Del YUV obtenemos el RGB o el GRAY: */
        if(cam_desc->mode == RGB_MODE) {
            /* Intercambia canales YUV: */
            m = 0;
            for(i=0;i<cam_desc->src_rows;i++) {
                for(j=0;j<cam_desc->src_cols;j++) {
                    cam_desc->src_img_YUV[m] = 
                      ((Ipp8u*)cam_desc->camera.capture_buffer)[m+1];
                    cam_desc->src_img_YUV[m+1] = 
                    ((Ipp8u*)cam_desc->camera.capture_buffer)[m];
                    m += 2;    
                }
            }
            /* Pasa la imagen YUV a RGB: */
            if(ippiYUV422ToRGB_8u_C2C3R(cam_desc->src_img_YUV,
                                        2*cam_desc->src_cols /*srcStep*/,
                                        cam_desc->src_img_RGB,
                                        cam_desc->step_src_RGB /*dstStep*/, 
                                        cam_desc->src_roi_size)!=ippStsNoErr){
                printf("Error en ippiYUV422ToRGB_8u_C2C3R\n");
                exit(1);
            }             
        } else { /* BW_MODE */
            /* Como no hay funci� en las IPP que separe 1 de 2 canales, hacemos
             * esto, que es tambi� bastante r�ido (no hace falta tener en
             * cuenta el paso en bytes, porque src_cols=640, que es mltiplo
             * exacto de 32): */
                  
            for(i=0;i<cam_desc->src_rows;i++) {
                l=i*(cam_desc->step_src_BW);
                m=i*(2*cam_desc->src_cols)+1;
                for(j=0;j<cam_desc->src_cols;j++) {
                  cam_desc->src_img_BW[l] =
                    ((Ipp8u*)cam_desc->camera.capture_buffer)[m];
                  l++;
                  m+=2;    
                }
            }              
        }        
        /* Soltamos el buffer del anillo DMA: */
        dc1394_dma_done_with_buffer(&(cam_desc->camera));	
    } else if((cam_desc->type == DV_CAM_LIVE)||(cam_desc->type == DV_CAM_FILE)){
        /* Lee frame DV y lo decodifica para obtener una imagen YUV: */
        read(cam_desc->fd,cam_desc->buf_in_DV,SIZE_FRAME_DV);
        DecodeDVFrame(cam_desc,cam_desc->buf_in_DV,cam_desc->src_img_YUV);

        /* Del YUV obtenemos el RGB o el GRAY: */
        if(cam_desc->mode == RGB_MODE) {
            /* Pasa la imagen YUV a RGB: */
            if(ippiYUV422ToRGB_8u_C2C3R(cam_desc->src_img_YUV,
                                        cam_desc->step_src_YUV /*srcStep*/,
                                        cam_desc->src_img_RGB,
                                        cam_desc->step_src_RGB /*dstStep*/, 
                                        cam_desc->src_roi_size)!=ippStsNoErr){
                printf("Error en ippiYUV422ToRGB_8u_C2C3R\n");
                exit(1);
            }             
        } else { /* BW_MODE */
            /* Como no hay funci� en las IPP que separe 1 de 2 canales, hacemos
             * esto, que es tambi� bastante r�ido (no hace falta tener en
             * cuenta el paso en bytes, porque src_cols=720, que es mltiplo
             * exacto de 8): */
            for(i=0;i<cam_desc->src_rows;i++) {
                l=i*(cam_desc->step_src_BW);
                m=i*(cam_desc->step_src_YUV);
                for(j=0;j<cam_desc->src_cols;j++) {
                  cam_desc->src_img_BW[l] =
                    cam_desc->src_img_YUV[m];
                l++;
                m+=2;    
                }
            }
        }        
    } else if(cam_desc->type == RGB_CAM_FILE) { 
        /* Del fichero RGB obtenemos el RGB : */
        if(cam_desc->step_src_RGB == cam_desc->src_cols*3) {
            /* Si se puede leer de golpe, lo hacemos: */
            read(cam_desc->fd,cam_desc->src_img_RGB,
                 3*(cam_desc->src_rows*cam_desc->src_cols));
        } else {
            /* Si no, leemos fila a fila: */
            for(i=0;i<cam_desc->src_rows;i++) {
                read(cam_desc->fd,
                     cam_desc->src_img_RGB+i*cam_desc->step_src_RGB,
                     3*(cam_desc->src_cols));            
            }
        }    

        /* Si se pide el gris, hacemos la conversi�: */
        if(cam_desc->mode == BW_MODE) {
            ippiRGBToGray_8u_C3C1R(cam_desc->src_img_RGB,
                                   cam_desc->step_src_RGB,
                                   cam_desc->src_img_BW,
                                   cam_desc->step_src_BW,
                                   cam_desc->src_roi_size);
        }
    } else if(cam_desc->type == BW_CAM_FILE) { 
        if(cam_desc->mode == RGB_MODE) {
            printf("Cómo es que el modo es RGB_MODE para BW_CAM_FILE?\n");
            exit(1);
        }     

        /* Del fichero BW obtenemos la imagen BW: */
        if(cam_desc->step_src_BW == cam_desc->src_cols) {
            /* Si se puede leer de golpe, lo hacemos: */
            read(cam_desc->fd,cam_desc->src_img_BW,
                 (cam_desc->src_rows*cam_desc->src_cols));
        } else {
            /* Si no, leemos fila a fila: */
            for(i=0;i<cam_desc->src_rows;i++) {
                read(cam_desc->fd,
                     cam_desc->src_img_BW+i*cam_desc->step_src_BW,
                     (cam_desc->src_cols));            
            }
        }    
    }

    
    /* Ahora s�o falta remuestrear la imagen al tama� pedido, ya sea
     * RGB o gris, pero s�o si los tama�s fuente y destino no coinciden: */
    if((cam_desc->mode == RGB_MODE) && 
       ((cam_desc->src_rows!=cam_desc->dst_rows)||
        (cam_desc->src_cols!=cam_desc->dst_cols))){
        ippiResize_8u_C3R(cam_desc->src_img_RGB,
                          cam_desc->src_roi_size,
                          cam_desc->step_src_RGB,
                          cam_desc->src_rect, 
                          cam_desc->dst_img_RGB,
                          cam_desc->step_dst_RGB,
                          cam_desc->dst_roi_size,
                          (double)cam_desc->dst_roi_size.width/
                          (double)cam_desc->src_roi_size.width, 
                          (double)cam_desc->dst_roi_size.height/
                          (double)cam_desc->src_roi_size.height,
                          IPPI_INTER_LINEAR); 
    } else if((cam_desc->mode == BW_MODE) && 
              ((cam_desc->src_rows!=cam_desc->dst_rows)||
               (cam_desc->src_cols!=cam_desc->dst_cols))){
        ippiResize_8u_C1R(cam_desc->src_img_BW,
                          cam_desc->src_roi_size,
                          cam_desc->step_src_BW,
                          cam_desc->src_rect, 
                          cam_desc->dst_img_BW,
                          cam_desc->step_dst_BW,
                          cam_desc->dst_roi_size,
                          (double)cam_desc->dst_roi_size.width/
                          (double)cam_desc->src_roi_size.width, 
                          (double)cam_desc->dst_roi_size.height/
                          (double)cam_desc->src_roi_size.height,
                          IPPI_INTER_LINEAR); 
    }

    /* Actualizaci� del nmero total de frames y el �dice de frame actual
     * (recordar que al llegar al final de fichero se vuelve sobre los frames
     * anteriores en direcci� contraria): */
    cam_desc->num_frames++;
    if(cam_desc->max_frames > 1) {    
        if(cam_desc->cur_frame == cam_desc->max_frames-1) {
            cam_desc->cur_frame--;
            cam_desc->dir = 1;
        } else if(cam_desc->cur_frame == 0) {
            cam_desc->cur_frame++;
            cam_desc->dir = 0;
        } else if(cam_desc->dir) {
            cam_desc->cur_frame--;
        } else {
            cam_desc->cur_frame++;
        }
    }

    /* Devolvemos puntero a la imagen destino (que coincidir�con el fuente 
     * si no hizo falta reescalar por coincidir en tama� con el destino),
     * y tambi� el valor correspondiente de step_bytes: */
    if(cam_desc->mode == RGB_MODE) {
        *step_bytes = cam_desc->step_dst_RGB;
        return cam_desc->dst_img_RGB; 
    } else { /* BW_MODE: */
        *step_bytes = cam_desc->step_dst_BW;
        return cam_desc->dst_img_BW; 
    }
}

void mycvCloseCamera(int camera)
{
    TCamDesc *cam_desc,*cam_desc_ant;

    /* Buscamos el descriptor en la lista de c�aras: */
    cam_desc_ant = NULL;
    cam_desc = list_cams;
    while(cam_desc != NULL) {
        if(cam_desc->fd==camera) {
            break;
        } else {
            cam_desc_ant = cam_desc;
            cam_desc = cam_desc->sig;
        }
    }    
    if(cam_desc == NULL) {
        printf("Descriptor de cámara inválido (%d) en mycvGetCloseCamera\n",
               camera);
        exit(1);
    }
    
    /* Para c�aras IIDC liberamos recursos correspondientes: */
    if(cam_desc->type == IIDC_CAM_LIVE) {
        //dc1394_release_camera(cam_desc->handle,&(cam_desc->camera));
        dc1394_destroy_handle(cam_desc->handle);    
    }

    /* Cerramos el fichero: */ 
    close(cam_desc->fd);

    /* Liberamos los buffers que hubi�emos reservado: */
    if(cam_desc->buf_in_DV != NULL) {
        free(cam_desc->buf_in_DV);
    }
    if(cam_desc->src_img_YUV != NULL) {
        ippiFree(cam_desc->src_img_YUV);
    }
    if(cam_desc->src_img_BW != NULL) {
        ippiFree(cam_desc->src_img_BW);
    }
    if(cam_desc->src_img_RGB != NULL) {
        ippiFree(cam_desc->src_img_RGB);
    }
    if((cam_desc->dst_img_RGB != NULL) && 
       ((cam_desc->src_rows != cam_desc->dst_rows) ||
        (cam_desc->src_cols != cam_desc->dst_cols)) ) {
        ippiFree(cam_desc->dst_img_RGB);
    }
    if((cam_desc->dst_img_BW != NULL) &&
       ((cam_desc->src_rows != cam_desc->dst_rows) ||
        (cam_desc->src_cols != cam_desc->dst_cols)) ) {    
        ippiFree(cam_desc->dst_img_BW);
    }

    /* Quitamos el nodo de la lista de descriptores de c�aras: */
    if(cam_desc_ant!=NULL) { /* No era el primer nodo: */
        cam_desc_ant->sig = cam_desc->sig;
    } else { /* Era el primero: */
        list_cams = cam_desc->sig;    
    }
    
    /* Liberamos memoria del nodo: */
    free(cam_desc);
    
    /* Cerramos el decodificador, s�o si era la ltima c�ara de la lista: */
    if(list_cams==NULL) {
        CloseDVDecoder();
    }
}

void mycvGetInfoCamera(int camera,TInfoCam *info_cam)
{
    TCamDesc *cam_desc;
    
    /* Buscamos el descriptor en la lista de c�aras: */
    cam_desc = list_cams;
    while(cam_desc != NULL) {
        if(cam_desc->fd==camera) {
            break;
        } else {
            cam_desc = cam_desc->sig;
        }
    }    
    if(cam_desc == NULL) {
        printf("Descriptor de cámara inválido (%d) en mycvGetInfoCamera\n",
               camera);
        exit(1);
    }

    info_cam->type = cam_desc->type;
    info_cam->src_rows = cam_desc->src_rows;
    info_cam->src_cols = cam_desc->src_cols;
    info_cam->dst_rows = cam_desc->dst_rows;
    info_cam->dst_cols = cam_desc->dst_cols;
    info_cam->dir = cam_desc->dir;
    info_cam->cur_frame = cam_desc->cur_frame;
    info_cam->num_frames = cam_desc->num_frames;
    info_cam->max_frames = cam_desc->max_frames;
    info_cam->mode = cam_desc->mode;
}

void mycvGoToFrameCamera(int camera,int frame,int dir)
{
    TCamDesc *cam_desc;
    
    /* Buscamos el descriptor en la lista de c�aras: */
    cam_desc = list_cams;
    while(cam_desc != NULL) {
        if(cam_desc->fd==camera) {
            break;
        } else {
            cam_desc = cam_desc->sig;
        }
    }    
    if(cam_desc == NULL) {
        printf("Descriptor de cámara inválido (%d) en mycvGoToFrameCamera\n",
               camera);
        exit(1);
    }

    /* Comprobamos la correcci� de la petici�: */
    if((cam_desc->type == DV_CAM_LIVE) || (cam_desc->type == IIDC_CAM_LIVE)){
        printf("\aWARNING: mycvGoToFrameCamera no tiene efecto en una cámara \"live\"\n");
        return;
    } else if((frame<0) || (frame>=cam_desc->max_frames)) {
        printf("Frame no válido (%d) en mycvGoToFrameCamera\n",frame);
        exit(1);
    } else if ((dir!=0) && (dir!=1)) {
        printf("Dirección no válida (%d) en mycvGoToFrameCamera\n",dir);
        exit(1);    
    } 
    
    /* Corregimos el nmero de frame y el campo dir: */
    cam_desc->cur_frame = frame;
    cam_desc->dir = dir;    
}
