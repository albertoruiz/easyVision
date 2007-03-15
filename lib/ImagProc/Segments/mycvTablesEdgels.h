#ifndef _MYCV_TABLES_EDGELS_H
#define _MYCV_TABLES_EDGELS_H

#ifdef __cplusplus
extern "C" {
#endif

#include "mycvUtils.h"

/* Para el módulo TablesEdgels.c, se declaran las variables como tal. Para el
 * resto de módulos (que usan éste), se declaran como externas:
 */
/*#ifdef _MYCV_TABLE_EDGELS_C
#define EXTERN
#else
#define EXTERN extern
#endif */

/* Maximum radius of local environment for edgel processing: */
#define MAXRADIUS  10 

/* Radio y anchura por defecto del edgel: */
#define DEFAULT_RADIUS 5
#define DEFAULT_WIDTH_EDGEL 1.2

/* ------------------------------------------------------------------------ */
/* Tablas del módulo. El usuario las utiliza tras inicializarlas con las    */
/* funciones del módulo, y sólo deben precalcularse si cambia algún         */
/* parámetro (por ejemplo, el radio de la máscara, o el ancho del edgel,    */
/* etc.):                                                                   */
/* ------------------------------------------------------------------------ */
extern float tableAngles[MAXRADIUS*4];
/* Table for theta values corresponding to indexes in the local response test.
 */
 
extern int tableEdgelProc[4*MAXRADIUS][2*MAXRADIUS+1][2*MAXRADIUS+1];
/* Tabla de entornos locales etiquetados con -INT_INFINITY (zona izquierda 
 * del edgel), INT_INFINITY (zona derecha del edgel), y -2,-1,1,2 ... para 
 * los pixels por los que pasa cerca el edgel (a menos de una distancia 
 * igual a thresh_edg_proc), con un número (en valor absoluto) creciente 
 * para la distancia al centro del edgel, y con signo negativo para direcciones
 * hacia arriba y/o izquierda (es decir, que han debido ser procesadas ya si
 * al pixel se ha llegado recorriendo la imagen por filas), y positivo en 
 * caso contrario. Así, a la misma vez se caracteriza un lado y a
 * otro de la línea (para extraer información de colores a ambos lados 
 * del edgel, por ejemplo), y se introduce un orden de procesamiento 
 * para ir haciendo el clustering de segmentos. Por último, el valor de
 * la entrada de la tabla será cero para aquellos puntos a distancia mayor
 * que el radio+0.5 del centro (para hacer las máscaras redondas, en lugar
 * de cuadradas). Hay 4*MAXRADIUS cuadrículas (una por cada orientación), 
 * cada una de tamaño [2*MAXRADIUS+1][2*MAXRADIUS+1] (el entorno local del 
 * pixel, cuadrado).
 */

typedef struct {
    int index;
    float value;
} TElementEdgelLineMaskXY;

extern TElementEdgelLineMaskXY tableEdgelLineMaskXY[2*MAXRADIUS+1][2*MAXRADIUS+1]
                      [(2*MAXRADIUS+1)*(2*MAXRADIUS+1)];
/* Tabla para computar rápido la saliencia local en cada dirección. Está 
 * indexada por:
 * - Primer y segundo índices: posiciones Y y X en la máscara cuadrada.
 * - Tercer índice: Un valor por cada contribución del pixel a una máscara
 *   en una determinada dirección.
 * El contenido de cada celda indica:
 *  index: Dirección en la que se contribuye. -1 para indicar final.
 *  value: Valor de la contribución (entre 0, para valores a distancia
 *         thresh_edg_proc de la línea ideal del edgel, y 1, para valores
 *         a distancia cero (en la línea ideal)).
 */


extern float tableEdgelLineMaskProb[4*MAXRADIUS][2*MAXRADIUS+1][2*MAXRADIUS+1];
/* Tabla directa de la anterior. Contiene el valor de la contribución, para
 * cada dirección, de cada pixel. Indexada por:
 * - Primer índice: Dirección del edgel.
 * - Segundo y tercer índices: posiciones Y y X en la máscara cuadrada.
 */

typedef struct {
    int i,j;
    int marcafin;
} TElementEdgelLineIndex;

extern TElementEdgelLineIndex tableEdgelLineIndex[4*MAXRADIUS][2][(2*MAXRADIUS+1)*(2*MAXRADIUS+1)/2];
/* Tabla inversa de la anterior, para recorrer con rapidez el entorno de un
 * pixel usando información de orientación local, y en orden de menor a mayor
 * distancia del centro (utilizada, por ejemplo, para hacer el cluster de 
 * líneas). Está indexada por:
 * - Primer índice:  Indice de la dirección local (0 indica todo derecha, 
 *                   4*MAXRADIUS-1 indica todo izquierda, un pixel abajo).
 * - Segundo índice: Indica una u otra dirección a lo largo del edgel. 
 * - Tercer índice: Recorre posiciones del array.
 * En cada celda, se encuentra la posición (i y j) siguiente a procesar en
 * la dirección del edgel (en offset desde el centro). El campo marca fin
 * contiene el valor 0 en todas las posiciones, y 1 en la posición siguiente
 * a la última (y que ya no contiene información útil).
 */

/* ------------------------------------------------------------------------ */
/* ------------ Prototipos de las funciones del módulo: --------------------*/
/* ------------------------------------------------------------------------ */
void mycvInitTablesEdgels(int user_radius,float user_width_edgel);
int mycvGetRadiusEdgels();
float mycvGetWidthEdgels();

#ifdef __cplusplus
}
#endif

#endif /* _MYCV_TABLES_EDGELS_H */
