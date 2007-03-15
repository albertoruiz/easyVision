#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "mycvUtils.h"
#include "mycvRunnings.h"

TRunPos **createRunLine(int x1,int y1,int x2,int y2,int width,
                            int* runWidth,int *runLength)
{
    int i,paso,sx,sy,dx,dy,e,origx1,origy1;
    int index_width,incx,incy,octante;
    TRunPos *origLine,**runLineMatrix;
    int c1x,c1y,c2x,c2y,p1,p2;

    if((x1==x2)&&(y1==y2)) {
        /* No tiene dimensión. Devolvemos matriz vacía. */
        *runLength = 0;
        *runWidth = 0;
        return NULL;       
    }

    /* Guardamos el origen del segmento, porque vamos a tocar x1 e y1: */
    origx1 = x1;
    origy1 = y1;
    
    /* Primero, en el array origLine colocamos el recorrido de Bressenham
     * de los dos puntos originales. A la vez, vemos en qué octante 
     * estamos: */
    paso = 0;
    dx = abs(x2-x1);
    sx = ((x2-x1)>=0)?1:-1;
    dy = abs(y2-y1);
    sy = ((y2-y1)>=0)?1:-1;
    if(dy>dx) {
        if(sx==1) {
            if(sy==1) {
                octante = 1;
            } else {
                octante = 6;
            }
        } else {
            if(sy==1) {
                octante = 2;
            } else {
                octante = 5;
            }
        }
    	paso = 1; /* Domina la Y, en lugar de la X: */
        x1 ^= y1; y1 ^= x1; x1 ^= y1; /* Intercambia x e y.   */ 
        dx ^= dy; dy ^= dx; dx ^= dy; /* Intercambia dx y dy. */
        sx ^= sy; sy ^= sx; sx ^= sy; /* Intercambia sx y sy. */        
    } else {
        if(sx==1) {
            if(sy==1) {
                octante = 0;
            } else {
                octante = 7;
            }
        } else {
            if(sy==1) {
                octante = 3;
            } else {
                octante = 4;
            }
        }
    }
    e = 2*dy-dx;      
    origLine = (TRunPos*) malloc((dx+1)*sizeof(TRunPos));      
    for(i=0;i<dx;i++) {
        if(paso) {
            origLine[i].y = x1;
            origLine[i].x = y1;
        } else {
            origLine[i].y = y1;
            origLine[i].x = x1;
        }
	if(e >= 0) {
            y1 += sy;
            e -= 2*dx;
        }
        x1 += sx;
        e += 2*dy;
    }
    origLine[dx].x = x2;
    origLine[dx].y = y2;    
    
    /* Creamos un array bidimensional con anchura=2*width1 y longitud=dx+1,
     * para colocar las sucesivas réplicas de la línea inicial, que cubren 
     * de modo denso el espacio de pixels, en un rectángulo centrado simétri-
     * camente a lo largo de la línea.*/
    runLineMatrix = (TRunPos**) malloc((2*width+1)*sizeof(TRunPos*));
    for(i=0;i<2*width+1;i++) {
        runLineMatrix[i] = (TRunPos*) malloc((dx+1)*sizeof(TRunPos));
    }
    
    /* La fila central es la línea original: */
    for(i=0;i<=dx;i++) {
        runLineMatrix[width][i].x = origLine[i].x;
        runLineMatrix[width][i].y = origLine[i].y;
    }
    
    /* Calculamos los incrementos por defecto en X y en Y (para el grueso
     * de la línea, excepto quizá la primera posición), en cada paso
     * (podrán valer (1,0) (-1,0) (0,1) ó (0,-1)): */
    if(paso) { /* Domina Y: */
        incx = y2>origy1?-1:1;
        incy = 0;
    } else { /* Domina X: */
        incx = 0;
        incy = x2>origx1?1:-1;
    }
    /*printf("(incx=%d,incy=%d,octante=%d)\n",incx,incy,octante);*/

    /* Para llenar el resto de líneas (hacia la derecha), calculamos dos
     * puntos candidatos iniciales, c1x y c2x. El primero es el resultado
     * de simplemente copiar la línea anterior, con el incremento en la
     * dirección no dominante. El segundo, el punto 2 (por la izq.) o 
     * el penúltimo menos el desplazamiento total en x y en y(por la der.), 
     * según octante en el que estemos.
     * De los dos puntos candidatos, nos quedamos con aquel que minimiza
     * el producto escalar con el vector de dirección de la recta (será
     * el que esté más cerca de la perpendicular a la recta).
     * Para llenar hacia el otro lado (la izquierda) se hace de modo
     * análogo. */      
    for(index_width=width+1;index_width<2*width+1;index_width++) {
        c1x = runLineMatrix[index_width-1][0].x + incx;
        c1y = runLineMatrix[index_width-1][0].y + incy;
        if(!(octante%2)) { /* octante par */
            c2x = runLineMatrix[index_width-1][dx-1].x + incx - (x2-origx1);
            c2y = runLineMatrix[index_width-1][dx-1].y + incy - (y2-origy1);
        } else { /* octante impar */
            c2x = runLineMatrix[index_width-1][1].x;
            c2y = runLineMatrix[index_width-1][1].y;
        }
        p1 = (x2-origx1)*(c1x-origx1) + (y2-origy1)*(c1y-origy1);
        p2 = (x2-origx1)*(c2x-origx1) + (y2-origy1)*(c2y-origy1);
        /*printf("c1=(%d,%d) c2=(%d,%d) p1=%d p2=%d\n",c1x,c1y,c2x,c2y,p1,p2);*/
        if(abs(p1)<abs(p2)) { /* Gana c1; replicamos línea a ambos lados: */
            for(i=0;i<=dx;i++) {
                /* Lado derecho: */
                runLineMatrix[index_width][i].x = 
                    runLineMatrix[index_width-1][i].x + incx;
                runLineMatrix[index_width][i].y = 
                    runLineMatrix[index_width-1][i].y + incy;
                    
                /* Lado izquierdo: */    
                runLineMatrix[2*width-index_width][i].x = 
                    runLineMatrix[2*width-index_width+1][i].x - incx;
                runLineMatrix[2*width-index_width][i].y = 
                    runLineMatrix[2*width-index_width+1][i].y - incy;                    
            }
        } else { /* Gana c2; replicamos, pero con un desplazamiento de 1 pix.: */
            if(!(octante%2)) {
                /* Lado derecho: */
                for(i=1;i<=dx;i++) {
                    runLineMatrix[index_width][i].x = 
                        runLineMatrix[index_width-1][i-1].x + incx;
                    runLineMatrix[index_width][i].y = 
                        runLineMatrix[index_width-1][i-1].y + incy;
                }
                runLineMatrix[index_width][0].x = c2x;
                runLineMatrix[index_width][0].y = c2y;

                /* Lado izquierdo: */    
                for(i=0;i<dx;i++) {
                    runLineMatrix[2*width-index_width][i].x = 
                        runLineMatrix[2*width-index_width+1][i+1].x - incx;
                    runLineMatrix[2*width-index_width][i].y = 
                        runLineMatrix[2*width-index_width+1][i+1].y - incy;
                }
                runLineMatrix[2*width-index_width][dx].x = 
                    runLineMatrix[2*width-index_width][0].x + (x2-origx1);
                runLineMatrix[2*width-index_width][dx].y = 
                    runLineMatrix[2*width-index_width][0].y + (y2-origy1);
            } else {
                /* Lado derecho: */
                for(i=0;i<dx;i++) {
                    runLineMatrix[index_width][i].x = 
                        runLineMatrix[index_width-1][i+1].x + incx;
                    runLineMatrix[index_width][i].y = 
                        runLineMatrix[index_width-1][i+1].y + incy;
                }
                runLineMatrix[index_width][dx].y = 
                    runLineMatrix[index_width][0].y + (y2-origy1);
                runLineMatrix[index_width][dx].x = 
                    runLineMatrix[index_width][0].x + (x2-origx1);            

                /* Lado izquierdo: */                        
                for(i=1;i<=dx;i++) {
                    runLineMatrix[2*width-index_width][i].x = 
                        runLineMatrix[2*width-index_width+1][i-1].x - incx;
                    runLineMatrix[2*width-index_width][i].y = 
                        runLineMatrix[2*width-index_width+1][i-1].y - incy;
                }
                runLineMatrix[2*width-index_width][0].x = 
                    runLineMatrix[2*width-index_width][dx].x - (x2-origx1);
                runLineMatrix[2*width-index_width][0].y = 
                    runLineMatrix[2*width-index_width][dx].y - (y2-origy1);
            }
        }
    }    
	
    /* Liberamos el array intermedio: */
    free(origLine);

    /* Las dimensiones del array devuelto son (2*width+1)x(dx+1): */
    *runWidth = 2*width+1;
    *runLength = (dx+1);    

    return runLineMatrix;
}


void freeRunLine(TRunPos **runLine,int width)
{
    int i;

    if(runLine!=NULL) {    
        for(i=0;i<2*width+1;i++) {
            free(runLine[i]);
        }
        free(runLine);
    }
}

/* Tipo y función auxiliares para *createRunAround: */
typedef struct {
    int y,x;
    float dist;
} TAux;
int compardist2(const void *p1,const void *p2)
{
    return (((TAux *)p1)->dist) > (((TAux *)p2)->dist);
}

/* Crea un recorrido ordenado alrededor del punto 0: */
TRunPos *createRunAround(float radius,int *runLength)
{
    int i,j,k,intradius;
    TRunPos *runAroundVector;
    TAux *auxVec;
    
    /* Creamos espacio para array devuelto (después ajustaremos al tamaño 
     * exacto): */
    intradius = (int) ceil(radius+1);    
    if(intradius<=1) {
        *runLength=0;
        return NULL;
    }    
    runAroundVector = 
        (TRunPos*) malloc(((2*intradius+1)*(2*intradius+1))*sizeof(TRunPos));
    
    /* Creamos vector auxiliar */
    auxVec = (TAux*) malloc(((2*intradius+1)*(2*intradius+1))*sizeof(TAux));    

    /* Llenamos el array con las distancias al centro: */
    k = 0;
    for(i=0;i<2*intradius+1;i++) {
        for(j=0;j<2*intradius+1;j++) {
            auxVec[k].y = i-intradius;
            auxVec[k].x = j-intradius;
            auxVec[k].dist = 
              mycvDistPointPoint((float)j,(float)i,
                                 (float)intradius,(float)intradius);
            k++;  
        }    
    }    

    /* Ordenamos por distancia: */
    qsort(auxVec,(2*intradius+1)*(2*intradius+1),sizeof(TAux),compardist2);

    k = 0;
    do {
        runAroundVector[k].y = auxVec[k].y;
        runAroundVector[k].x = auxVec[k].x;
        k++;
    } while(auxVec[k].dist<=radius);
    
    /* Ajustamos el tamaño del vector devuelto: */
    runAroundVector = 
        (TRunPos*) realloc(runAroundVector,k*sizeof(TRunPos));    
    *runLength = k;
    
    /* Liberamos vector auxiliar: */
    free(auxVec);

    /* Y devolvemos array solución (con el espacio reservado; el usuario
     * es responsable de liberarlo con la función freeRunAround):
     */
    return runAroundVector;
}


void freeRunAround(TRunPos *runAround)
{
    free(runAround);
}
