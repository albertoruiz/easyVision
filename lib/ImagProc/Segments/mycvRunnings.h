#ifndef _MYCV_RUNNINGS_H
#define _MYCV_RUNNINGS_H

#ifdef __cplusplus
extern "C" {
#endif

/* Tipo de celda elemental de la matriz que devuelve la función createRunLine:*/
typedef struct {
    int y,x;
} TRunPos;

/* --------------- Prototipos de las funciones del módulo: ------------------ */
/* Función que crea y devuelve una matriz rectangular para acceder, de modo 
 * ordenado, a los pixels que rodean a una línea. En width pasamos la anchura
 * (en dirección perpendicular, pero en unidades de pixel, que no tiene porqué
 * coincidir con la euclídea a la línea que deseamos. La función devuelve
 * una matriz de dimensiones runWidth x runLength, donde runWidth será siempre
 * 2*width+1 (la línea estará en la fila width, los pixels por encima de ella
 * en las filas 0..width-1, y los pixels por debajo en width+1..2*width). La
 * variable runLength será el maximo(abs(x1-x2)+1,abs(y1-y2)+1), puesto que
 * la línea se recorre con el algoritmo de Bresenham, que genera pixels 
 * 8-conectados. 
 * La función asegura que se devuelve una matriz rectangular de posiciones
 * que recorre de modo denso el rectángulo que rodea (simétricamente) a la
 * línea de entrada. Pero, ojo, porque el parámetro de entrada width no indica
 * distancia euclídea (en perpendicular) a la recta, sino simplemente orden
 * de distancia a esta. La distancia coincidirá con la euclídea sólo para
 * rectas horizontales y verticales.
 */
TRunPos **createRunLine(int x1,int y1,int x2,int y2,int width,
                        int *runWidth,int *runLength);
                            
/* Esta función libera una matriz de accesos devuelta por la función 
 * createRunLine anterior: 
 */                            
void freeRunLine(TRunPos **runLine,int width);

/* Esta función crea un array unidimensional de recorrido alrededor de un
 * pixel, hasta un radio máximo pedido. Los desplazamientos x e y son
 * relativos a un centro (0,0).
 */
TRunPos *createRunAround(float radius,int *runLength);

/* Esta función libera un vector de accesos devuelto por la función 
 * createRunAround anterior: 
 */                            
void freeRunAround(TRunPos *runAround);

#ifdef __cplusplus
}
#endif

#endif /* _MYCV_RUNNINGS_H */
