/*******************************************************************************
#	 	luvcview: Sdl video Usb Video Class grabber          .         #
#This package work with the Logitech UVC based webcams with the mjpeg feature. #
#All the decoding is in user space with the embedded jpeg decoder              #
#.                                                                             #
# 		Copyright (C) 2005 2006 Laurent Pinchart &&  Michel Xhaard     #
#                                                                              #
# This program is free software; you can redistribute it and/or modify         #
# it under the terms of the GNU General Public License as published by         #
# the Free Software Foundation; either version 2 of the License, or            #
# (at your option) any later version.                                          #
#                                                                              #
# This program is distributed in the hope that it will be useful,              #
# but WITHOUT ANY WARRANTY; without even the implied warranty of               #
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                #
# GNU General Public License for more details.                                 #
#                                                                              #
# You should have received a copy of the GNU General Public License            #
# along with this program; if not, write to the Free Software                  #
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA    #
#                                                                              #
*******************************************************************************/

#include "utils.h"
#include "color.h"
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <linux/types.h>
#include <string.h>
#include <fcntl.h>
#include <wait.h>
#include <time.h>
#include <limits.h>
#include "huffman.h"

#define ISHIFT 11

#define IFIX(a) ((int)((a) * (1 << ISHIFT) + .5))

#ifndef __P
# define __P(x) x
#endif

/* special markers */
#define M_BADHUFF	-1
#define M_EOF		0x80

struct jpeg_decdata {
    int dcts[6 * 64 + 16];
    int out[64 * 6];
    int dquant[3][64];
};

struct in {
    unsigned char *p;
    unsigned int bits;
    int left;
    int marker;
    int (*func) __P((void *));
    void *data;
};

/*********************************/
struct dec_hufftbl;
struct enc_hufftbl;

union hufftblp {
    struct dec_hufftbl *dhuff;
    struct enc_hufftbl *ehuff;
};

struct scan {
    int dc;			/* old dc value */

    union hufftblp hudc;
    union hufftblp huac;
    int next;			/* when to switch to next scan */

    int cid;			/* component id */
    int hv;			/* horiz/vert, copied from comp */
    int tq;			/* quant tbl, copied from comp */
};

/*********************************/

#define DECBITS 10		/* seems to be the optimum */

struct dec_hufftbl {
    int maxcode[17];
    int valptr[16];
    unsigned char vals[256];
    unsigned int llvals[1 << DECBITS];
};
static int huffman_init(void);
static void decode_mcus
__P((struct in *, int *, int, struct scan *, int *));
static int dec_readmarker __P((struct in *));
static void dec_makehuff
__P((struct dec_hufftbl *, int *, unsigned char *));

static void setinput __P((struct in *, unsigned char *));
/*********************************/

#undef PREC
#define PREC int

static void idctqtab __P((unsigned char *, PREC *));

inline static void idct(int *in, int *out, int *quant, long off, int max);

int is_huffman(unsigned char *buf);

/*********************************/

static void yuv420pto422(int * out,unsigned char *pic,int width);
static void yuv422pto422(int * out,unsigned char *pic,int width);
static void yuv444pto422(int * out,unsigned char *pic,int width);
static void yuv400pto422(int * out,unsigned char *pic,int width);
typedef void (*ftopict) ( int *out, unsigned char *pic, int width) ;
/*********************************/

#define M_SOI	0xd8
#define M_APP0	0xe0
#define M_DQT	0xdb
#define M_SOF0	0xc0
#define M_DHT   0xc4
#define M_DRI	0xdd
#define M_SOS	0xda
#define M_RST0	0xd0
#define M_EOI	0xd9
#define M_COM	0xfe

static unsigned char *datap;

static int getbyte(void)
{
    return *datap++;
}

static int getword(void)
{
    int c1, c2;
    c1 = *datap++;
    c2 = *datap++;
    return c1 << 8 | c2;
}

struct comp {
    int cid;
    int hv;
    int tq;
};

#define MAXCOMP 4
struct jpginfo {
    int nc;			/* number of components */
    int ns;			/* number of scans */
    int dri;			/* restart interval */
    int nm;			/* mcus til next marker */
    int rm;			/* next restart marker */
};

static struct jpginfo info;
static struct comp comps[MAXCOMP];

static struct scan dscans[MAXCOMP];

static unsigned char quant[4][64];

static struct dec_hufftbl dhuff[4];

#define dec_huffdc (dhuff + 0)
#define dec_huffac (dhuff + 2)

static struct in in;

static int readtables(int till, int *isDHT)
{
    int m, l, i, j, lq, pq, tq;
    int tc, th, tt;

    for (;;) {
	if (getbyte() != 0xff)
	    return -1;
nextbyte:
	if ((m = getbyte()) == till)
	    break;

	switch (m) {
	case 0xc2:
	    return 0;

	case M_DQT:
	//printf("find DQT \n");
	    lq = getword();
	    while (lq > 2) {
		pq = getbyte();
		tq = pq & 15;
		if (tq > 3)
		    return -1;
		pq >>= 4;
		if (pq != 0)
		    return -1;
		for (i = 0; i < 64; i++)
		    quant[tq][i] = getbyte();
		lq -= 64 + 1;
	    }
	    break;

	case M_DHT:
	//printf("find DHT \n");
	    l = getword();
	    while (l > 2) {
		int hufflen[16], k;
		unsigned char huffvals[256];

		tc = getbyte();
		th = tc & 15;
		tc >>= 4;
		tt = tc * 2 + th;
		if (tc > 1 || th > 1)
		    return -1;
		for (i = 0; i < 16; i++)
		    hufflen[i] = getbyte();
		l -= 1 + 16;
		k = 0;
		for (i = 0; i < 16; i++) {
		    for (j = 0; j < hufflen[i]; j++)
			huffvals[k++] = getbyte();
		    l -= hufflen[i];
		}
		dec_makehuff(dhuff + tt, hufflen, huffvals);
	    }
	    *isDHT= 1;
	    break;

	case M_DRI:
	printf("find DRI \n");
	    l = getword();
	    info.dri = getword();
	    break;
	case 0xff:
	    goto nextbyte;
	    break;

	default:
	    l = getword();
	    while (l-- > 2)
		getbyte();
	    break;
	}
    }

    return 0;
}

static void dec_initscans(void)
{
    int i;

    info.nm = info.dri + 1;
    info.rm = M_RST0;
    for (i = 0; i < info.ns; i++)
	dscans[i].dc = 0;
}

static int dec_checkmarker(void)
{
    int i;

    if (dec_readmarker(&in) != info.rm)
	return -1;
    info.nm = info.dri;
    info.rm = (info.rm + 1) & ~0x08;
    for (i = 0; i < info.ns; i++)
	dscans[i].dc = 0;
    return 0;
}


int jpeg_decode(unsigned char **pic, unsigned char *buf, int *width,
		int *height)
{
    struct jpeg_decdata *decdata;
    int i, j, m, tac, tdc;
    int intwidth, intheight;
    int mcusx, mcusy, mx, my;
    int ypitch ,xpitch,bpp,pitch,x,y;
    int mb;
    int max[6];
    ftopict convert;
    int err = 0;
    int isInitHuffman = 0;
    decdata = (struct jpeg_decdata *) malloc(sizeof(struct jpeg_decdata));
    
    if (!decdata) {
	err = -1;
	goto error;
    }
    if (buf == NULL) {
	err = -1;
	goto error;
    }
    datap = buf;
    if (getbyte() != 0xff) {
	err = ERR_NO_SOI;
	goto error;
    }
    if (getbyte() != M_SOI) {
	err = ERR_NO_SOI;
	goto error;
    }
    if (readtables(M_SOF0, &isInitHuffman)) {
	err = ERR_BAD_TABLES;
	goto error;
    }
    getword();
    i = getbyte();
    if (i != 8) {
	err = ERR_NOT_8BIT;
	goto error;
    }
    intheight = getword();
    intwidth = getword();
    
    if ((intheight & 7) || (intwidth & 7)) {
	err = ERR_BAD_WIDTH_OR_HEIGHT;
	goto error;
    }
    info.nc = getbyte();
    if (info.nc > MAXCOMP) {
	err = ERR_TOO_MANY_COMPPS;
	goto error;
    }
    for (i = 0; i < info.nc; i++) {
	int h, v;
	comps[i].cid = getbyte();
	comps[i].hv = getbyte();
	v = comps[i].hv & 15;
	h = comps[i].hv >> 4;
	comps[i].tq = getbyte();
	if (h > 3 || v > 3) {
	    err = ERR_ILLEGAL_HV;
	    goto error;
	}
	if (comps[i].tq > 3) {
	    err = ERR_QUANT_TABLE_SELECTOR;
	    goto error;
	}
    }
    if (readtables(M_SOS,&isInitHuffman)) {
	err = ERR_BAD_TABLES;
	goto error;
    }
    getword();
    info.ns = getbyte();
    if (!info.ns){
    printf("info ns %d/n",info.ns);
	err = ERR_NOT_YCBCR_221111;
	goto error;
    }
    for (i = 0; i < info.ns; i++) {
	dscans[i].cid = getbyte();
	tdc = getbyte();
	tac = tdc & 15;
	tdc >>= 4;
	if (tdc > 1 || tac > 1) {
	    err = ERR_QUANT_TABLE_SELECTOR;
	    goto error;
	}
	for (j = 0; j < info.nc; j++)
	    if (comps[j].cid == dscans[i].cid)
		break;
	if (j == info.nc) {
	    err = ERR_UNKNOWN_CID_IN_SCAN;
	    goto error;
	}
	dscans[i].hv = comps[j].hv;
	dscans[i].tq = comps[j].tq;
	dscans[i].hudc.dhuff = dec_huffdc + tdc;
	dscans[i].huac.dhuff = dec_huffac + tac;
    }

    i = getbyte();
    j = getbyte();
    m = getbyte();

    if (i != 0 || j != 63 || m != 0) {
    	printf("hmm FW error,not seq DCT ??\n");
    }
   // printf("ext huffman table %d \n",isInitHuffman);
    if(!isInitHuffman) {
    	if(huffman_init() < 0)
		return -ERR_BAD_TABLES;
	}
/*
    if (dscans[0].cid != 1 || dscans[1].cid != 2 || dscans[2].cid != 3) {
	err = ERR_NOT_YCBCR_221111;
	goto error;
    }

    if (dscans[1].hv != 0x11 || dscans[2].hv != 0x11) {
	err = ERR_NOT_YCBCR_221111;
	goto error;
    }
*/    
    /* if internal width and external are not the same or heigth too 
       and pic not allocated realloc the good size and mark the change 
       need 1 macroblock line more ?? */
    if (intwidth != *width || intheight != *height || *pic == NULL) {
	*width = intwidth;
	*height = intheight;
	// BytesperPixel 2 yuyv , 3 rgb24 
	*pic =
	    (unsigned char *) realloc((unsigned char *) *pic,
				      (size_t) intwidth * (intheight +
							   8) * 2);
    }


    switch (dscans[0].hv) {
    case 0x22: // 411
    	mb=6;
	mcusx = *width >> 4;
	mcusy = *height >> 4;
	bpp=2;
	xpitch = 16 * bpp;
	pitch = *width * bpp; // YUYV out
	ypitch = 16 * pitch;
	convert = yuv420pto422;	
	break;
    case 0x21: //422
   // printf("find 422 %dx%d\n",*width,*height);
    	mb=4;
	mcusx = *width >> 4;
	mcusy = *height >> 3;
	bpp=2;	
	xpitch = 16 * bpp;
	pitch = *width * bpp; // YUYV out
	ypitch = 8 * pitch;
	convert = yuv422pto422;	
	break;
    case 0x11: //444
	mcusx = *width >> 3;
	mcusy = *height >> 3;
	bpp=2;
	xpitch = 8 * bpp;
	pitch = *width * bpp; // YUYV out
	ypitch = 8 * pitch;
	 if (info.ns==1) {
    		mb = 1;
		convert = yuv400pto422;
	} else {
		mb=3;
		convert = yuv444pto422;	
	}
        break;
    default:
	err = ERR_NOT_YCBCR_221111;
	goto error;
	break;
    }

    idctqtab(quant[dscans[0].tq], decdata->dquant[0]);
    idctqtab(quant[dscans[1].tq], decdata->dquant[1]);
    idctqtab(quant[dscans[2].tq], decdata->dquant[2]);
    setinput(&in, datap);
    dec_initscans();

    dscans[0].next = 2;
    dscans[1].next = 1;
    dscans[2].next = 0;	/* 4xx encoding */
    for (my = 0,y=0; my < mcusy; my++,y+=ypitch) {
	for (mx = 0,x=0; mx < mcusx; mx++,x+=xpitch) {
	    if (info.dri && !--info.nm)
		if (dec_checkmarker()) {
		    err = ERR_WRONG_MARKER;
		    goto error;
		}
	switch (mb){
	    case 6: {
		decode_mcus(&in, decdata->dcts, mb, dscans, max);
		idct(decdata->dcts, decdata->out, decdata->dquant[0],
		     IFIX(128.5), max[0]);
		idct(decdata->dcts + 64, decdata->out + 64,
		     decdata->dquant[0], IFIX(128.5), max[1]);
		idct(decdata->dcts + 128, decdata->out + 128,
		     decdata->dquant[0], IFIX(128.5), max[2]);
		idct(decdata->dcts + 192, decdata->out + 192,
		     decdata->dquant[0], IFIX(128.5), max[3]);
		idct(decdata->dcts + 256, decdata->out + 256,
		     decdata->dquant[1], IFIX(0.5), max[4]);
		idct(decdata->dcts + 320, decdata->out + 320,
		     decdata->dquant[2], IFIX(0.5), max[5]);
	  
	    } break;
	    case 4:
	    {
		decode_mcus(&in, decdata->dcts, mb, dscans, max);
		idct(decdata->dcts, decdata->out, decdata->dquant[0],
		     IFIX(128.5), max[0]);
		idct(decdata->dcts + 64, decdata->out + 64,
		     decdata->dquant[0], IFIX(128.5), max[1]);
		idct(decdata->dcts + 128, decdata->out + 256,
		     decdata->dquant[1], IFIX(0.5), max[4]);
		idct(decdata->dcts + 192, decdata->out + 320,
		     decdata->dquant[2], IFIX(0.5), max[5]);
	   	   
	    }
	    break;
	    case 3:
	    	 decode_mcus(&in, decdata->dcts, mb, dscans, max);
		idct(decdata->dcts, decdata->out, decdata->dquant[0],
		     IFIX(128.5), max[0]);		     
		idct(decdata->dcts + 64, decdata->out + 256,
		     decdata->dquant[1], IFIX(0.5), max[4]);
		idct(decdata->dcts + 128, decdata->out + 320,
		     decdata->dquant[2], IFIX(0.5), max[5]);
	    
		         
	    break;
	    case 1:
	    	 decode_mcus(&in, decdata->dcts, mb, dscans, max);
		idct(decdata->dcts, decdata->out, decdata->dquant[0],
		     IFIX(128.5), max[0]);
		  
	    break;
	    
	} // switch enc411
	convert(decdata->out,*pic+y+x,pitch); 
	}
    }

    m = dec_readmarker(&in);
    if (m != M_EOI) {
	err = ERR_NO_EOI;
	goto error;
    }
    if (decdata)
	free(decdata);
    return 0;
  error:
    if (decdata)
	free(decdata);
    return err;
}

/****************************************************************/
/**************       huffman decoder             ***************/
/****************************************************************/
static int huffman_init(void)
{    	int tc, th, tt;
 	const unsigned char *ptr= JPEGHuffmanTable ;
	int i, j, l;
	l = JPG_HUFFMAN_TABLE_LENGTH ;
	    while (l > 0) {
		int hufflen[16], k;
		unsigned char huffvals[256];

		tc = *ptr++;
		th = tc & 15;
		tc >>= 4;
		tt = tc * 2 + th;
		if (tc > 1 || th > 1)
		    return -ERR_BAD_TABLES;
		for (i = 0; i < 16; i++)
		    hufflen[i] = *ptr++;
		l -= 1 + 16;
		k = 0;
		for (i = 0; i < 16; i++) {
		    for (j = 0; j < hufflen[i]; j++)
			huffvals[k++] = *ptr++;
		    l -= hufflen[i];
		}
		dec_makehuff(dhuff + tt, hufflen, huffvals);
	    }
	    return 0;
}

static int fillbits __P((struct in *, int, unsigned int));
static int dec_rec2
__P((struct in *, struct dec_hufftbl *, int *, int, int));

static void setinput(in, p)
struct in *in;
unsigned char *p;
{
    in->p = p;
    in->left = 0;
    in->bits = 0;
    in->marker = 0;
}

static int fillbits(in, le, bi)
struct in *in;
int le;
unsigned int bi;
{
    int b, m;

    if (in->marker) {
	if (le <= 16)
	    in->bits = bi << 16, le += 16;
	return le;
    }
    while (le <= 24) {
	b = *in->p++;
	if (b == 0xff && (m = *in->p++) != 0) {
	    if (m == M_EOF) {
		if (in->func && (m = in->func(in->data)) == 0)
		    continue;
	    }
	    in->marker = m;
	    if (le <= 16)
		bi = bi << 16, le += 16;
	    break;
	}
	bi = bi << 8 | b;
	le += 8;
    }
    in->bits = bi;		/* tmp... 2 return values needed */
    return le;
}

static int dec_readmarker(in)
struct in *in;
{
    int m;

    in->left = fillbits(in, in->left, in->bits);
    if ((m = in->marker) == 0)
	return 0;
    in->left = 0;
    in->marker = 0;
    return m;
}

#define LEBI_DCL	int le, bi
#define LEBI_GET(in)	(le = in->left, bi = in->bits)
#define LEBI_PUT(in)	(in->left = le, in->bits = bi)

#define GETBITS(in, n) (					\
  (le < (n) ? le = fillbits(in, le, bi), bi = in->bits : 0),	\
  (le -= (n)),							\
  bi >> le & ((1 << (n)) - 1)					\
)

#define UNGETBITS(in, n) (	\
  le += (n)			\
)


static int dec_rec2(in, hu, runp, c, i)
struct in *in;
struct dec_hufftbl *hu;
int *runp;
int c, i;
{
    LEBI_DCL;

    LEBI_GET(in);
    if (i) {
	UNGETBITS(in, i & 127);
	*runp = i >> 8 & 15;
	i >>= 16;
    } else {
	for (i = DECBITS;
	     (c = ((c << 1) | GETBITS(in, 1))) >= (hu->maxcode[i]); i++);
	if (i >= 16) {
	    in->marker = M_BADHUFF;
	    return 0;
	}
	i = hu->vals[hu->valptr[i] + c - hu->maxcode[i - 1] * 2];
	*runp = i >> 4;
	i &= 15;
    }
    if (i == 0) {		/* sigh, 0xf0 is 11 bit */
	LEBI_PUT(in);
	return 0;
    }
    /* receive part */
    c = GETBITS(in, i);
    if (c < (1 << (i - 1)))
	c += (-1 << i) + 1;
    LEBI_PUT(in);
    return c;
}

#define DEC_REC(in, hu, r, i)	 (	\
  r = GETBITS(in, DECBITS),		\
  i = hu->llvals[r],			\
  i & 128 ?				\
    (					\
      UNGETBITS(in, i & 127),		\
      r = i >> 8 & 15,			\
      i >> 16				\
    )					\
  :					\
    (					\
      LEBI_PUT(in),			\
      i = dec_rec2(in, hu, &r, r, i),	\
      LEBI_GET(in),			\
      i					\
    )					\
)

static void decode_mcus(in, dct, n, sc, maxp)
struct in *in;
int *dct;
int n;
struct scan *sc;
int *maxp;
{
    struct dec_hufftbl *hu;
    int i, r, t;
    LEBI_DCL;

    memset(dct, 0, n * 64 * sizeof(*dct));
    LEBI_GET(in);
    while (n-- > 0) {
	hu = sc->hudc.dhuff;
	*dct++ = (sc->dc += DEC_REC(in, hu, r, t));

	hu = sc->huac.dhuff;
	i = 63;
	while (i > 0) {
	    t = DEC_REC(in, hu, r, t);
	    if (t == 0 && r == 0) {
		dct += i;
		break;
	    }
	    dct += r;
	    *dct++ = t;
	    i -= r + 1;
	}
	*maxp++ = 64 - i;
	if (n == sc->next)
	    sc++;
    }
    LEBI_PUT(in);
}

static void dec_makehuff(hu, hufflen, huffvals)
struct dec_hufftbl *hu;
int *hufflen;
unsigned char *huffvals;
{
    int code, k, i, j, d, x, c, v;
    for (i = 0; i < (1 << DECBITS); i++)
	hu->llvals[i] = 0;

/*
 * llvals layout:
 *
 * value v already known, run r, backup u bits:
 *  vvvvvvvvvvvvvvvv 0000 rrrr 1 uuuuuuu
 * value unknown, size b bits, run r, backup u bits:
 *  000000000000bbbb 0000 rrrr 0 uuuuuuu
 * value and size unknown:
 *  0000000000000000 0000 0000 0 0000000
 */
    code = 0;
    k = 0;
    for (i = 0; i < 16; i++, code <<= 1) {	/* sizes */
	hu->valptr[i] = k;
	for (j = 0; j < hufflen[i]; j++) {
	    hu->vals[k] = *huffvals++;
	    if (i < DECBITS) {
		c = code << (DECBITS - 1 - i);
		v = hu->vals[k] & 0x0f;	/* size */
		for (d = 1 << (DECBITS - 1 - i); --d >= 0;) {
		    if (v + i < DECBITS) {	/* both fit in table */
			x = d >> (DECBITS - 1 - v - i);
			if (v && x < (1 << (v - 1)))
			    x += (-1 << v) + 1;
			x = x << 16 | (hu->vals[k] & 0xf0) << 4 |
			    (DECBITS - (i + 1 + v)) | 128;
		    } else
			x = v << 16 | (hu->vals[k] & 0xf0) << 4 |
			    (DECBITS - (i + 1));
		    hu->llvals[c | d] = x;
		}
	    }
	    code++;
	    k++;
	}
	hu->maxcode[i] = code;
    }
    hu->maxcode[16] = 0x20000;	/* always terminate decode */
}

/****************************************************************/
/**************             idct                  ***************/
/****************************************************************/


#define IMULT(a, b) (((a) * (b)) >> ISHIFT)
#define ITOINT(a) ((a) >> ISHIFT)

#define S22 ((PREC)IFIX(2 * 0.382683432))
#define C22 ((PREC)IFIX(2 * 0.923879532))
#define IC4 ((PREC)IFIX(1 / 0.707106781))

static unsigned char zig2[64] = {
    0, 2, 3, 9, 10, 20, 21, 35,
    14, 16, 25, 31, 39, 46, 50, 57,
    5, 7, 12, 18, 23, 33, 37, 48,
    27, 29, 41, 44, 52, 55, 59, 62,
    15, 26, 30, 40, 45, 51, 56, 58,
    1, 4, 8, 11, 19, 22, 34, 36,
    28, 42, 43, 53, 54, 60, 61, 63,
    6, 13, 17, 24, 32, 38, 47, 49
};

inline static void idct(int *in, int *out, int *quant, long off, int max)
{
    long t0, t1, t2, t3, t4, t5, t6, t7;	// t ;
    long tmp0, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6;
    long tmp[64], *tmpp;
    int i, j, te;
    unsigned char *zig2p;

    t0 = off;
    if (max == 1) {
	t0 += in[0] * quant[0];
	for (i = 0; i < 64; i++)
	    out[i] = ITOINT(t0);
	return;
    }
    zig2p = zig2;
    tmpp = tmp;
    for (i = 0; i < 8; i++) {
	j = *zig2p++;
	t0 += in[j] * (long) quant[j];
	j = *zig2p++;
	t5 = in[j] * (long) quant[j];
	j = *zig2p++;
	t2 = in[j] * (long) quant[j];
	j = *zig2p++;
	t7 = in[j] * (long) quant[j];
	j = *zig2p++;
	t1 = in[j] * (long) quant[j];
	j = *zig2p++;
	t4 = in[j] * (long) quant[j];
	j = *zig2p++;
	t3 = in[j] * (long) quant[j];
	j = *zig2p++;
	t6 = in[j] * (long) quant[j];


	if ((t1 | t2 | t3 | t4 | t5 | t6 | t7) == 0) {

	    tmpp[0 * 8] = t0;
	    tmpp[1 * 8] = t0;
	    tmpp[2 * 8] = t0;
	    tmpp[3 * 8] = t0;
	    tmpp[4 * 8] = t0;
	    tmpp[5 * 8] = t0;
	    tmpp[6 * 8] = t0;
	    tmpp[7 * 8] = t0;

	    tmpp++;
	    t0 = 0;
	    continue;
	}
	//IDCT;
	tmp0 = t0 + t1;
	t1 = t0 - t1;
	tmp2 = t2 - t3;
	t3 = t2 + t3;
	tmp2 = IMULT(tmp2, IC4) - t3;
	tmp3 = tmp0 + t3;
	t3 = tmp0 - t3;
	tmp1 = t1 + tmp2;
	tmp2 = t1 - tmp2;
	tmp4 = t4 - t7;
	t7 = t4 + t7;
	tmp5 = t5 + t6;
	t6 = t5 - t6;
	tmp6 = tmp5 - t7;
	t7 = tmp5 + t7;
	tmp5 = IMULT(tmp6, IC4);
	tmp6 = IMULT((tmp4 + t6), S22);
	tmp4 = IMULT(tmp4, (C22 - S22)) + tmp6;
	t6 = IMULT(t6, (C22 + S22)) - tmp6;
	t6 = t6 - t7;
	t5 = tmp5 - t6;
	t4 = tmp4 - t5;

	tmpp[0 * 8] = tmp3 + t7;	//t0;
	tmpp[1 * 8] = tmp1 + t6;	//t1;
	tmpp[2 * 8] = tmp2 + t5;	//t2;
	tmpp[3 * 8] = t3 + t4;	//t3;
	tmpp[4 * 8] = t3 - t4;	//t4;
	tmpp[5 * 8] = tmp2 - t5;	//t5;
	tmpp[6 * 8] = tmp1 - t6;	//t6;
	tmpp[7 * 8] = tmp3 - t7;	//t7;
	tmpp++;
	t0 = 0;
    }
    for (i = 0, j = 0; i < 8; i++) {
	t0 = tmp[j + 0];
	t1 = tmp[j + 1];
	t2 = tmp[j + 2];
	t3 = tmp[j + 3];
	t4 = tmp[j + 4];
	t5 = tmp[j + 5];
	t6 = tmp[j + 6];
	t7 = tmp[j + 7];
	if ((t1 | t2 | t3 | t4 | t5 | t6 | t7) == 0) {
	    te = ITOINT(t0);
	    out[j + 0] = te;
	    out[j + 1] = te;
	    out[j + 2] = te;
	    out[j + 3] = te;
	    out[j + 4] = te;
	    out[j + 5] = te;
	    out[j + 6] = te;
	    out[j + 7] = te;
	    j += 8;
	    continue;
	}
	//IDCT;
	tmp0 = t0 + t1;
	t1 = t0 - t1;
	tmp2 = t2 - t3;
	t3 = t2 + t3;
	tmp2 = IMULT(tmp2, IC4) - t3;
	tmp3 = tmp0 + t3;
	t3 = tmp0 - t3;
	tmp1 = t1 + tmp2;
	tmp2 = t1 - tmp2;
	tmp4 = t4 - t7;
	t7 = t4 + t7;
	tmp5 = t5 + t6;
	t6 = t5 - t6;
	tmp6 = tmp5 - t7;
	t7 = tmp5 + t7;
	tmp5 = IMULT(tmp6, IC4);
	tmp6 = IMULT((tmp4 + t6), S22);
	tmp4 = IMULT(tmp4, (C22 - S22)) + tmp6;
	t6 = IMULT(t6, (C22 + S22)) - tmp6;
	t6 = t6 - t7;
	t5 = tmp5 - t6;
	t4 = tmp4 - t5;

	out[j + 0] = ITOINT(tmp3 + t7);
	out[j + 1] = ITOINT(tmp1 + t6);
	out[j + 2] = ITOINT(tmp2 + t5);
	out[j + 3] = ITOINT(t3 + t4);
	out[j + 4] = ITOINT(t3 - t4);
	out[j + 5] = ITOINT(tmp2 - t5);
	out[j + 6] = ITOINT(tmp1 - t6);
	out[j + 7] = ITOINT(tmp3 - t7);
	j += 8;
    }

}

static unsigned char zig[64] = {
    0, 1, 5, 6, 14, 15, 27, 28,
    2, 4, 7, 13, 16, 26, 29, 42,
    3, 8, 12, 17, 25, 30, 41, 43,
    9, 11, 18, 24, 31, 40, 44, 53,
    10, 19, 23, 32, 39, 45, 52, 54,
    20, 22, 33, 38, 46, 51, 55, 60,
    21, 34, 37, 47, 50, 56, 59, 61,
    35, 36, 48, 49, 57, 58, 62, 63
};

static PREC aaidct[8] = {
    IFIX(0.3535533906), IFIX(0.4903926402),
    IFIX(0.4619397663), IFIX(0.4157348062),
    IFIX(0.3535533906), IFIX(0.2777851165),
    IFIX(0.1913417162), IFIX(0.0975451610)
};


static void idctqtab(qin, qout)
unsigned char *qin;
PREC *qout;
{
    int i, j;

    for (i = 0; i < 8; i++)
	for (j = 0; j < 8; j++)
	    qout[zig[i * 8 + j]] = qin[zig[i * 8 + j]] *
		IMULT(aaidct[i], aaidct[j]);
}

#define  FOUR_TWO_TWO 2		//Y00 Cb Y01 Cr


/* translate YUV422Packed to rgb24 */

unsigned int
Pyuv422torgb24(unsigned char * input_ptr, unsigned char * output_ptr, unsigned int image_width, unsigned int image_height)
{
	unsigned int i, size;
	unsigned char Y, Y1, U, V;
	unsigned char *buff = input_ptr;
	unsigned char *output_pt = output_ptr;
	size = image_width * image_height /2;
	for (i = size; i > 0; i--) {
		/* bgr instead rgb ?? */
		Y = buff[0] ;
		U = buff[1] ;
		Y1 = buff[2];
		V = buff[3];
		buff += 4;
		*output_pt++ = R_FROMYV(Y,V);
		*output_pt++ = G_FROMYUV(Y,U,V); //b
		*output_pt++ = B_FROMYU(Y,U); //v
			
		*output_pt++ = R_FROMYV(Y1,V);
		*output_pt++ = G_FROMYUV(Y1,U,V); //b
		*output_pt++ = B_FROMYU(Y1,U); //v
	}
	
	return FOUR_TWO_TWO;
} 

static void yuv420pto422(int * out,unsigned char *pic,int width)
{
    int j, k;
    unsigned char *pic0, *pic1;
    int *outy, *outu, *outv;
    int outy1 = 0;
    int outy2 = 8;


    pic0 = pic;
    pic1 = pic + width;
    outy = out;
    outu = out + 64 * 4;
    outv = out + 64 * 5;    
	for (j = 0; j < 8; j++) {
	    for (k = 0; k < 8; k++) {
	    if( k == 4) { 
	    	outy1 += 56;
	    	outy2 += 56;
	    }
	    *pic0++ = CLIP(outy[outy1]);
	    *pic0++ = CLIP(128 + *outu);
	    *pic0++ = CLIP(outy[outy1+1]);
	    *pic0++ = CLIP(128 + *outv);
	    *pic1++ = CLIP(outy[outy2]);
	    *pic1++ = CLIP(128 + *outu);
	    *pic1++ = CLIP(outy[outy2+1]);
	    *pic1++ = CLIP(128 + *outv);
	   outy1 +=2; outy2 += 2; outu++; outv++;
	  }
	  if(j==3) {
	  outy = out + 128;
	  } else {
	  outy += 16;
	  }
	    outy1 = 0;
	    outy2 = 8;
	    pic0 += 2 * (width -16);
	    pic1 += 2 * (width -16);
	    
	}
    
}
static void yuv422pto422(int * out,unsigned char *pic,int width)
{
    int j, k;
    unsigned char *pic0, *pic1;
    int *outy, *outu, *outv;
    int outy1 = 0;
    int outy2 = 8;
    int outu1 = 0;
    int outv1 = 0;
 

    pic0 = pic;
    pic1 = pic + width;
    outy = out;
    outu = out + 64 * 4;
    outv = out + 64 * 5;    
	for (j = 0; j < 4; j++) {
	    for (k = 0; k < 8; k++) {
	    if( k == 4) { 
	    	outy1 += 56;
	    	outy2 += 56;
	    }
	    *pic0++ = CLIP(outy[outy1]);
	    *pic0++ = CLIP(128 + outu[outu1]);
	    *pic0++ = CLIP(outy[outy1+1]);
	    *pic0++ = CLIP(128 + outv[outv1]);
	    *pic1++ = CLIP(outy[outy2]);
	    *pic1++ = CLIP(128 + outu[outu1+8]);
	    *pic1++ = CLIP(outy[outy2+1]);
	    *pic1++ = CLIP(128 + outv[outv1+8]);
	    outv1 += 1; outu1 += 1;
	    outy1 +=2; outy2 +=2;
	   
	  }
	  
	    outy += 16;outu +=8; outv +=8;
	    outv1 = 0; outu1=0;
	    outy1 = 0;
	    outy2 = 8;
	    pic0 += 2 * (width -16);
	    pic1 += 2 * (width -16);
	    
	}
    
}
static void yuv444pto422(int * out,unsigned char *pic,int width)
{
    int j, k;
    unsigned char *pic0, *pic1;
    int *outy, *outu, *outv;
    int outy1 = 0;
    int outy2 = 8;
    int outu1 = 0;
    int outv1 = 0;

    pic0 = pic;
    pic1 = pic + width;
    outy = out;
    outu = out + 64 * 4; // Ooops where did i invert ??
    outv = out + 64 * 5;    
	for (j = 0; j < 4; j++) {
	    for (k = 0; k < 4; k++) {
	    
	    *pic0++ =CLIP( outy[outy1]);
	    *pic0++ =CLIP( 128 + outu[outu1]);
	    *pic0++ =CLIP( outy[outy1+1]);
	    *pic0++ =CLIP( 128 + outv[outv1]);
	    *pic1++ =CLIP( outy[outy2]);
	    *pic1++ =CLIP( 128 + outu[outu1+8]);
	    *pic1++ =CLIP( outy[outy2+1]);
	    *pic1++ =CLIP( 128 + outv[outv1+8]);
	    outv1 += 2; outu1 += 2;
	    outy1 +=2; outy2 +=2;	   
	  }	  
	    outy += 16;outu +=16; outv +=16;
	    outv1 = 0; outu1=0;
	    outy1 = 0;
	    outy2 = 8;
	    pic0 += 2 * (width -8);
	    pic1 += 2 * (width -8);	    
	}
    
}
static void yuv400pto422(int * out,unsigned char *pic,int width)
{
    int j, k;
    unsigned char *pic0, *pic1;
    int *outy ;
    int outy1 = 0;
    int outy2 = 8;
    pic0 = pic;
    pic1 = pic + width;
    outy = out;
      
	for (j = 0; j < 4; j++) {
	    for (k = 0; k < 4; k++) {	    
	    *pic0++ = CLIP(outy[outy1]);
	    *pic0++ = 128 ;
	    *pic0++ = CLIP(outy[outy1+1]);
	    *pic0++ = 128 ;
	    *pic1++ = CLIP(outy[outy2]);
	    *pic1++ = 128 ;
	    *pic1++ = CLIP(outy[outy2+1]);
	    *pic1++ = 128 ;
	     outy1 +=2; outy2 +=2;  
	  }	  
	    outy += 16;
	    outy1 = 0;
	    outy2 = 8;
	    pic0 += 2 * (width -8);
	    pic1 += 2 * (width -8);	    
	}
    
}

int 
is_huffman(unsigned char *buf)
{
unsigned char *ptbuf;
int i = 0;
ptbuf = buf;
while (((ptbuf[0] << 8) | ptbuf[1]) != 0xffda){	
	if(i++ > 2048) 
		return 0;
	if(((ptbuf[0] << 8) | ptbuf[1]) == 0xffc4)
		return 1;
	ptbuf++;
}
return 0;
}
static void
 getPictureName (char *Picture, int fmt)
{
char temp[80];
  char *myext[] = { "pnm", "jpg" };
  int i;
  time_t curdate;
  struct tm *tdate;
  memset (temp, '\0', sizeof (temp));
  time (&curdate);
  tdate = localtime (&curdate);
  snprintf (temp, 26, "P-%02d:%02d:%04d-%02d:%02d:%02d.%s\0",
	    tdate->tm_mon + 1, tdate->tm_mday, tdate->tm_year + 1900,
	    tdate->tm_hour, tdate->tm_min, tdate->tm_sec, myext[fmt]);

  memcpy (Picture, temp, strlen (temp));
}
int 
get_picture(unsigned char *buf,int size)
{
FILE *file;
unsigned char *ptdeb,*ptcur = buf;
int sizein;
char *name = NULL;
name = calloc(80,1);
getPictureName (name, 1);
file = fopen(name, "wb");
if (file != NULL) {
	if(!is_huffman(buf)){
	ptdeb = ptcur = buf;
	while (((ptcur[0] << 8) | ptcur[1]) != 0xffc0)
	    	ptcur++;
	sizein = ptcur-ptdeb;
	fwrite(buf,
		sizein, 1, file);
	fwrite(dht_data,
		DHT_SIZE, 1, file);
	fwrite(ptcur,size-sizein,1,file); 
	} else {
	fwrite(ptcur,size,1,file); /* ptcur was uninit -wsr */
	}     
	fclose(file);
	}
if(name)
	free(name);
return 0;		
}

int
get_pictureYV2(unsigned char *buf,int width,int height)
{
FILE *foutpict;
unsigned char *picture = NULL;
char *name = NULL;
name = calloc(80,1);
getPictureName (name, 0);
picture = (unsigned char *)malloc(width*height*3*sizeof(char));
if(picture){
	Pyuv422torgb24(buf, picture, width, height);
}else{
	printf(" no room to take a picture \n");
	return 0;
}
if(name){
	foutpict = fopen (name, "wb");
	fprintf (foutpict, "P6\n%d %d\n255\n", width, height);
	fwrite (picture, sizeof (char), width * height * 3, foutpict);
	fclose (foutpict);
	free(name);
}
free(picture);
picture = NULL;
return 0;
}
