#include <GL/gl.h>

#include "SiftGPU.h"

#include <locale.h>
#include <vector>
#include <cstdio>
#include <cstdlib>

void* __dso_handle;


extern "C" {

int c_siftGPU(int argc, char ** argv,
              unsigned char * pSrc, int sstep, int sr1, int sr2, int sc1, int sc2,
              int* tot, int nres, double* res) {

//     for(int k =0; k<argc; k++) {
//         printf("%s\n",argv[k]);
//     }

    static SiftGPU sift;

    // Needed for correct interpretation of dot (.) in numeric strings: (!)
    setlocale(LC_NUMERIC,"C");

    sift.ParseParam(argc, argv);


    if( sift.VerifyContextGL() != SiftGPU::SIFTGPU_FULL_SUPPORTED) {
        printf("ContextGL initialization failure\n");
        exit(0);
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

//////////////////////////////////////////////////////////

int c_matchGPU(int* tot, double err, double ratio,
               int n1, double* v1, int n2, double* v2,
               int nres, double*res) {

    static SiftMatchGPU matcher(nres);
    if(not matcher.VerifyContextGL()) {
        printf("ContextGL initialization failure\n");
        exit(0);
    }

    // printf("%d %d\n",n1/128,n2/128);

    float * des1 = (float*)malloc(n1*sizeof(float));
    float * des2 = (float*)malloc(n2*sizeof(float));
    int (*match_buf)[2] = new int[nres/2][2];

    for (int k=0; k<n1; k++) des1[k] = v1[k];
    for (int k=0; k<n2; k++) des2[k] = v2[k];

    matcher.SetDescriptors(0, n1/128, des1);
    matcher.SetDescriptors(1, n2/128, des2);

    *tot = matcher.GetSiftMatch(nres/2, match_buf, err, ratio);

    //printf("matches = %d\n",*tot);

    int j = 0;
    for (int k=0; k<*tot; k++) {
        res[j++] = match_buf[k][0];
        res[j++] = match_buf[k][1];
        //printf("%d %d\n",match_buf[k][0],match_buf[k][1]);
    }

    delete [] match_buf;
    free(des1);
    free(des2);

    return 0;
}

}
