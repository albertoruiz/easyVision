#include <stdlib.h>
#include <stdio.h>
#include <math.h>

#include "mycvTablesEdgels.h"

static int radius=DEFAULT_RADIUS;
/* This value determines the size of the local environment which the algo-
 * rithm will take into account when locally finding lines. Every point
 * whose Manhattan distance to the current point is not greater than this
 * value will be tested.
 * The greater this value is, the most accurate will be the process of
 * edgels, but it will also be computationally more expensive.
 * Default value is 5 pixels (good compromise).
 */

static float width_edgel=DEFAULT_WIDTH_EDGEL;
/* Este valor determina la máxima distancia (euclídea) a la que se encuentran
 * los pixels que se consideran parte del edgel, una vez que se tiene fija la
 * orientación del mismo (2*width_edgel es el ancho del edgel en la 
 * dirección perpendicular al mismo. El valor por defecto es 1.2, que da un
 * ancho en pixels de entre 2 y 3 pixels.
 */

static int tables_initialized = 0;
/* Flag que indica si las tablas han sido inicializadas. */

float tableAngles[MAXRADIUS*4];
int tableEdgelProc[4*MAXRADIUS][2*MAXRADIUS+1][2*MAXRADIUS+1];
TElementEdgelLineMaskXY tableEdgelLineMaskXY[2*MAXRADIUS+1][2*MAXRADIUS+1]
                                       [(2*MAXRADIUS+1)*(2*MAXRADIUS+1)];
float tableEdgelLineMaskProb[4*MAXRADIUS][2*MAXRADIUS+1][2*MAXRADIUS+1];
TElementEdgelLineIndex tableEdgelLineIndex[4*MAXRADIUS][2][(2*MAXRADIUS+1)*(2*MAXRADIUS+1)/2];

/* ------------------------------------------------------------------------ */
/* ------------- Funciones para inicialización de las tablas: ------------- */
/* ------------------------------------------------------------------------ */
/* This procedure inits a table with the corresponding theta values for each
 * of the maximum response indexes in a local line detector.
 */
void init_table_angles()
{
    int k;

    for(k=0;k<radius*4;k++) {
        tableAngles[k] = k*PI/(radius*4);
    }
}


typedef struct {
        int i,j;
        float dist;
} compar_dist_struct;
    
int compardist(const void *p1,const void *p2)
{
    return (((compar_dist_struct *)p1)->dist) > (((compar_dist_struct *)p2)->dist);
}

void init_table_edgel_proc()
{
    int i,j,k,contpos,contneg;    
    float nx1,ny1,nx2,ny2,nx1perp,ny1perp,nx2perp,ny2perp,dist1,dist2,dist3;
    compar_dist_struct arraydistcenterpos[(2*MAXRADIUS+1)*(2*MAXRADIUS+1)],
                       arraydistcenterneg[(2*MAXRADIUS+1)*(2*MAXRADIUS+1)];
        
    /* Primero las tablas directas... */
    for(k=0;k<radius*4;k++) {
        nx1 = radius*cos(tableAngles[k]);
        ny1 = radius*sin(tableAngles[k]);
        nx2 = -nx1;
        ny2 = -ny1;
        nx1perp = ny1;
        ny1perp = -nx1;
        nx2perp = -nx1perp;
        ny2perp = -ny1perp;
        
        contpos = 0;
        contneg = 0;
        for(i=0;i<2*radius+1;i++) {
            for(j=0;j<2*radius+1;j++) {
                dist1 = mycvDistPointPoint(radius,radius,i,j);
                if((dist1>radius+0.5)) {
                        tableEdgelProc[k][i][j] = 0;                                                         
                } else {
                    dist2 = mycvDistPointLine(nx1,ny1,nx2,ny2,j-radius,i-radius,1);
                    if(fabs(dist2) <= width_edgel) {
                        dist3 = mycvDistPointLine(nx1perp,ny1perp,nx2perp,ny2perp,
                                                j-radius,i-radius,1);
                        if(dist3>=0) {
                            arraydistcenterpos[contpos].dist = dist3;
                            arraydistcenterpos[contpos].i = i;                        
                            arraydistcenterpos[contpos].j = j;                        
                            contpos++;
                        } else {
                            arraydistcenterneg[contneg].dist = -dist3;
                            arraydistcenterneg[contneg].i = i;                        
                            arraydistcenterneg[contneg].j = j;                        
                            contneg++;
                        }
                    } else if(dist2>0) {
                        tableEdgelProc[k][i][j] = -INT_INFINITY;                                
                    } else {
                        tableEdgelProc[k][i][j] = INT_INFINITY;                                
                    }
                }   
            }
        }
        qsort(arraydistcenterpos,contpos,sizeof(compar_dist_struct),compardist);
        qsort(arraydistcenterneg,contneg,sizeof(compar_dist_struct),compardist);
        for(i=0;i<contpos;i++) {
            tableEdgelProc[k][arraydistcenterpos[i].i][arraydistcenterpos[i].j] =
              i+1;                                            
            tableEdgelLineIndex[k][0][i].i = arraydistcenterpos[i].i - radius;
            tableEdgelLineIndex[k][0][i].j = arraydistcenterpos[i].j - radius;              
            tableEdgelLineIndex[k][0][i].marcafin = 0;
        }
        tableEdgelLineIndex[k][0][contpos].marcafin = 1;        
        for(i=0;i<contneg;i++) {
            tableEdgelProc[k][arraydistcenterneg[i].i][arraydistcenterneg[i].j] =
              -i-1;                                            
            tableEdgelLineIndex[k][1][i].i = arraydistcenterneg[i].i - radius;
            tableEdgelLineIndex[k][1][i].j = arraydistcenterneg[i].j - radius;
            tableEdgelLineIndex[k][1][i].marcafin = 0;
        }
        tableEdgelLineIndex[k][1][contneg].marcafin = 1;
    }
}


void init_table_edgel_line_mask()
{
    int i,j,k,count;
    float nx1,ny1,nx2,ny2,dist,prob_tot;
    
    for(k=0;k<radius*4;k++) {
        nx1 = radius*cos(tableAngles[k]);
        ny1 = radius*sin(tableAngles[k]);
        nx2 = -nx1;
        ny2 = -ny1;
        prob_tot = 0;
        for(i=0;i<2*radius+1;i++) {
            for(j=0;j<2*radius+1;j++) {
                tableEdgelLineMaskProb[k][i][j] = 0;
                if((tableEdgelProc[k][i][j]!=INT_INFINITY) &&
                   (tableEdgelProc[k][i][j]!=-INT_INFINITY) &&
                   (tableEdgelProc[k][i][j]!=0) ) {
                    dist = mycvDistPointLine(nx1,ny1,nx2,ny2,j-radius,i-radius,0);
                    if(dist <= width_edgel) {
                        tableEdgelLineMaskProb[k][i][j] = width_edgel - dist;
                        prob_tot += tableEdgelLineMaskProb[k][i][j];
                    }    
                }
            }
        }
        for(i=0;i<2*radius+1;i++) {
            for(j=0;j<2*radius+1;j++) {
                if(tableEdgelLineMaskProb[k][i][j] != 0) {
                    tableEdgelLineMaskProb[k][i][j] /= prob_tot;
                }    
            }
        }        
    }    
            
    /* Tabla inversa (indexada por posición en la máscara): */
    for(i=0;i<2*radius+1;i++) {
        for(j=0;j<2*radius+1;j++) {
            count = 0;
            for(k=0;k<radius*4;k++) {
                if(tableEdgelLineMaskProb[k][i][j]!=0) {
                    tableEdgelLineMaskXY[i][j][count].index = k;
                    tableEdgelLineMaskXY[i][j][count].value = 
                        tableEdgelLineMaskProb[k][i][j];
                    count++;
                }
            }
            tableEdgelLineMaskXY[i][j][count].index = -1;             
        }
    }     
}


void mycvInitTablesEdgels(int user_radius,float user_width_edgel)
{
    if(user_radius==0) {
        user_radius = DEFAULT_RADIUS;
    }
    
    if(user_width_edgel==0.0) {
        user_width_edgel = DEFAULT_WIDTH_EDGEL;
    }
    
    if( (user_radius==radius)&&(user_width_edgel==width_edgel)&&
        (tables_initialized) ) {
//        printf("Ya inicializadas...no trabajo.\n");
        return;
    } 

    if((user_radius>MAXRADIUS)||(user_radius<0)) {
        printf("mycvTablesEdgels: Valor de radio inválido\n");
        exit(1);
    } else {
        radius = user_radius;
    }

    if((user_width_edgel>radius)||(user_width_edgel<0.5)) {
        printf("mycvTablesEdgels: Valor de anchura de edgel inválido\n");
        exit(1);
    } else {
        width_edgel = user_width_edgel;
    }

/*    printf("Inicializando tablas con radius=%d y width_edgel=%2.2f\n",
           radius,width_edgel);    */
    init_table_angles();
    init_table_edgel_proc();
    init_table_edgel_line_mask();

    tables_initialized = 1;
}

int mycvGetRadiusEdgels() 
{
    return radius;
}

float mycvGetWidthEdgels()
{
    return width_edgel;
}

#undef _TABLE_EDGELS_C
