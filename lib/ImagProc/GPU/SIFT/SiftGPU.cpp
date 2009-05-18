#include "SiftGPU.h"

#include <locale.h>
#include <vector>
#include <cstdio>
#include <cstdlib>
#include <GL/gl.h>


extern "C" {

int c_siftGPU(int argc, char ** argv,
              unsigned char * pSrc, int sstep, int sr1, int sr2, int sc1, int sc2,
              int* tot, int nres, double* res) {

//     for(int k =0; k<argc; k++) {
//         printf("%s\n",argv[k]);
//     }

    static SiftGPU sift;
    static bool firstTime = true;

    // Needed for correct interpretation of dot (.) in numeric strings: (!)
    setlocale(LC_NUMERIC,"C");

    sift.ParseParam(argc, argv);

    // OpenGL context creation:
    if(firstTime) {
        //sift.CreateContextGL();

        if( sift.VerifyContextGL() != SiftGPU::SIFTGPU_FULL_SUPPORTED) {
            printf("ContextGL initialization failure\n");
            exit(0);
        }
        firstTime = false;
    }

    // Run of GPU SIFT method:
    int width = sc2-sc1+1, height = sr2-sr1+1;
    const unsigned char *data = pSrc;
    sift.RunSIFT(width,height,data,GL_LUMINANCE,GL_UNSIGNED_BYTE);

    // Extract output:
    int num = sift.GetFeatureNum();

    *tot = num;
    int max = nres/(128+4);
    if (max < *tot) *tot = max;

    std::vector<float> descriptors(128*num);
    std::vector<SiftGPU::SiftKeypoint> keys(num);
    sift.GetFeatureVector(&keys[0],&descriptors[0]);
    int k = 0;

    double sc = (double)width/2;
    double rm = (double)height/2;
    double cm = (double)width/2;
    for(int i=0;i<*tot;i++) {
        res[k++] = (cm-keys[i].x) /sc;
        res[k++] = (rm-keys[i].y) /sc;
        res[k++] = keys[i].s /sc;
        res[k++] = keys[i].o;
        for(int j=0;j<128;j++) {
            res[k++] = descriptors[128*i+j];
        }
    }

    return 0;
}

}
