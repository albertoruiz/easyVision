#include <qvimageprocessing/mycvRunnings.h>
#include <qvimageprocessing/mycvSegments.h>
#include <qvimageprocessing/mycvTablesEdgels.h>
#include <qvimageprocessing/mycvUtils.h>
#include <qvimageprocessing/mycvUtilsSegs.h>

/*! \mainpage

mycvLibrary es una librer� con distintas funciones de captura y procesamiento de imagen. A destacar la capacidad de leer distintos tipos de dispositivos IEEE-1394 y formatos de ficheros .rgb, .bw y .dv entre las primeras, y el extractor de segmentos con informaci� de color entre las segundas. 

INTRODUCCI�

Este directorio contiene el c�igo fuente de la librer� libmycv. Tras la compilaci� del mismo, se genera un fichero libmycv.a, contra el que pueden enlazar nuestras aplicaciones. 

COMPILACI�

El proceso de compilaci� de la librer� es an�ogo al seguido para la primera aplicaci� sencilla de ejemplo de uso de las IPP (ver directorio Intel-SW/simple-example-IPP): simplemente hay que exportar la variable de entorno COMPVIS=icc, si se quiere usar el compilador de intel en lugar del gcc que se usa por defecto, y/o editar el fichero Makefile y definir el valor deseado de las variables $(LINKING) y $(CPUOPTION), si se quieren cambiar las opciones de compilaci�.

NOTA IMPORTANTE: El fichero libmycv.a debe generarse con las mismas opciones de compilaci� (es decir, variables COMPILER=gcc|icc, LINKING=SSP|DMP y CPUOPTION=-DMYCPU_T7|-DMYCPU_W7|-DMYCPU_A6|-DMYCPU_PX en el Makefile) con el que se vayan a compilar las aplicaciones que use la librer�. En caso contrario, al intentar enlazar con la misma se producir�n errores de referencias no resueltas.

USO


El fichero de cabecera de la librer� (a incluir por las aplicaciones que la usen) es <mycv.h> Dicho fichero simplemente incluye a su vez las cabeceras de los distintos m�ulos incluidos en la librer�. Si el programador s�o va a utilizar alguno de �tos m�ulos por separado, y si lo desea, puede incluir s�o el fichero de cabecera correspondiente (<mycvCalibOdom.h>, <mycvCamera.h>, <mycvRunnings.h>, <mycvSegments.h>, <mycvTablesEdgels.h>, <mycvUtils.h> �<mycvUtilsSegs.h>). En principio, la librer� puede ser utilizada sin problema tanto desde programas en C (como las aplicaciones GTK del directorio /examples) como desde programas en C++ (por ejemplo, para desarrollar aplicaciones con QT).

A continuaci� sigue una breve descripci� (m� detallada en aquellos m�ulos que considero m� importantes) de las funciones proporcionadas los distintos m�ulos (en general, los respectivos ficheros de cabecera est� convenientemente comentados para facilitar la utilizaci� de las distintas funciones ofrecidas):

- M�ulo mycvCalibOdom.h: (POR HACER)

- M�ulo mycvCamera.h: (POR HACER)

- M�ulo mycvRunnings.h: (POR HACER)

- M�ulo mycvSegments.h: (POR HACER)

- M�ulo mycvTablesEdgels.h: (POR HACER)

- M�ulo mycvUtils.h: (POR HACER)

- M�ulo mycvUtilsSegs.h: (POR HACER)


*/
