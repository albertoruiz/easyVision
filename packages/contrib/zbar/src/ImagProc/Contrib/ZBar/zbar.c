#include "wrappers.h"
#include <stdio.h>
#include <math.h>

#include <zbar.h>

int c_zbar(IMG(x), int*m, TBarcode* result) {
    
    //printf("hello\n");
    /* create a reader */
    zbar_image_scanner_t * scanner = zbar_image_scanner_create();

    /* configure the reader */
    zbar_image_scanner_set_config(scanner, 0, ZBAR_CFG_ENABLE, 1);

    /* obtain image data */
    int width = xsc2+1, height = xsr2+1;
    if (xsc1!=0 || xsr1!=0 || xsstep != width) {
        printf("c1=%d c2=%d r1=%d r2=%d step=%d\n",xsc1,xsc2,xsr1,xsr2,xsstep);
        return 1;
    }

    void *raw = xpSrc;
    //get_data(argv[1], &width, &height, &raw);

    /* wrap image data */
    zbar_image_t *image = zbar_image_create();
    zbar_image_set_format(image, *(int*)"Y800");
    zbar_image_set_size(image, width, height);
    zbar_image_set_data(image, raw, width * height, NULL);
    
    //printf("after set\n");
    /* scan the image for barcodes */
    int n = zbar_scan_image(scanner, image);
    //printf("scan_image code: %d\n",n);
    /* extract results */
    int k = 0;
    const zbar_symbol_t *symbol = zbar_image_first_symbol(image);
    for(; symbol; symbol = zbar_symbol_next(symbol)) {
        /* do something useful with results */
        zbar_symbol_type_t typ = zbar_symbol_get_type(symbol);
        result[k].symbol_type = zbar_get_symbol_name(typ);
        const char *data = zbar_symbol_get_data(symbol);
        result[k].symbol_value = data;
        // printf("decoded %s symbol \"%s\"\n",zbar_get_symbol_name(typ), data);

        int np = zbar_symbol_get_loc_size(symbol);
        //printf("np=%d\n",np);
        int j,x,y,r1,c1,r2,c2;
        c1 = c2 = zbar_symbol_get_loc_x(symbol,0);
        r1 = r2 = zbar_symbol_get_loc_y(symbol,0);
        for (j=1; j<np; j++) {
            x = zbar_symbol_get_loc_x(symbol,j);
            y = zbar_symbol_get_loc_y(symbol,j);
            if (y<r1) r1 = y;
            if (y>r2) r2 = y;
            if (x<c1) c1 = x;
            if (x>c2) c2 = x;
            //printf("j=%d\n",j);
        }
        result[k].bbr1 = r1;
        result[k].bbr2 = r2;
        result[k].bbc1 = c1;
        result[k].bbc2 = c2;
        k++;
        if(k>=50) break;
    }

    /* clean up */
    zbar_image_destroy(image);

    *m = k;
    
    return n;
}


