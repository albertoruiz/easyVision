void mycvSegmentsWithParms_8u_C1_C3(void *imgIn,int stepIn, 
                                    int x1,int y1, double roiSize,
                                    void*segments,int *num_segments,
                                    int user_radius,float user_width_edgel,
                                    int med_siz,
                                    unsigned char thres_high,
                                    unsigned char thres_low,
                                    int one_channel);

void mycvPostProcessSegments(void*segments,
                             int *numsegments,
                             float minlength,float maxlengthtodel,
                             unsigned char mincontrast);
