//
// bitmap.c
//
// handle MS bitmap I/O. For portability, we don't use the data structure defined in Windows.h
// However, there is some strange thing, the side of our structure is different from what it
// should though we define it in the same way as MS did. So, there is a hack, we use the hardcoded
// constanr, 14, instead of the sizeof to calculate the size of the structure.
// You are not supposed to worry about this part. However, I will appreciate if you find out the
// reason and let me know. Thanks.
//
#include "bitmap.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define BMP_BI_RGB        0L

typedef unsigned short BMP_WORD;
typedef unsigned int   BMP_DWORD;
typedef int            BMP_LONG;

typedef struct {
	BMP_WORD	bfType;
	BMP_DWORD	bfSize;
	BMP_WORD	bfReserved1;
	BMP_WORD	bfReserved2;
	BMP_DWORD	bfOffBits;
} BMP_BITMAPFILEHEADER;

typedef struct {
	BMP_DWORD	biSize;
	BMP_LONG	biWidth;
	BMP_LONG	biHeight;
	BMP_WORD	biPlanes;
	BMP_WORD	biBitCount;
	BMP_DWORD	biCompression;
	BMP_DWORD	biSizeImage;
	BMP_LONG	biXPelsPerMeter;
	BMP_LONG	biYPelsPerMeter;
	BMP_DWORD	biClrUsed;
	BMP_DWORD	biClrImportant;
} BMP_BITMAPINFOHEADER;

BMP_BITMAPFILEHEADER bmfh;
BMP_BITMAPINFOHEADER bmih;

unsigned char* read_bmp(const char *fname, int* width, int* height, int* step) {
	FILE* file=fopen(fname, "rb");
	if (!file) return NULL;

	// I am doing fread( &bmfh, sizeof(BMP_BITMAPFILEHEADER), 1, file ) in a safe way.
	fread( &(bmfh.bfType), 2, 1, file);
	fread( &(bmfh.bfSize), 4, 1, file);
	fread( &(bmfh.bfReserved1), 2, 1, file);
	fread( &(bmfh.bfReserved2), 2, 1, file);
	fread( &(bmfh.bfOffBits), 4, 1, file);

	BMP_DWORD pos = bmfh.bfOffBits;

	fread( &bmih, sizeof(BMP_BITMAPINFOHEADER), 1, file );

	// error checking
	// "BM" actually
	if ( bmfh.bfType!= 0x4d42 ) return NULL;
	if ( bmih.biBitCount != 24 ) return NULL;
	fseek( file, pos, SEEK_SET );

	*width = bmih.biWidth;
	*height = bmih.biHeight;

	int padWidth = *width * 3;
	int pad = 0;
	if ( padWidth % 4 != 0 ) {
		pad = 4 - (padWidth % 4);
		padWidth += pad;
	}
	*step = padWidth;
	int bytes = *height * padWidth;

	unsigned char* data = malloc(bytes);

	int foo = fread( data, bytes, 1, file );

	if (!foo) {
		free(data);
		return NULL;
	}

	fclose( file );
	
	// shuffle bitmap data such that it is (R,G,B) tuples in row-major order
	int i, j;
	j = 0;
	unsigned char temp;
	unsigned char* in;
	unsigned char* out;

	in = data;
	out = data;

	for ( j = 0; j < *height; ++j ) {
		for ( i = 0; i < *width; ++i ) {
			out[1] = in[1];
			temp = in[2];
			out[2] = in[0];
			out[0] = temp;

			in += 3;
			out += 3;
		}
		in += pad;
	}

	return data;
}

void write_bmp(const char *iname, int width, int height, unsigned char *data) {
	int bytes, pad;
	bytes = width * 3;
	pad = (bytes%4) ? 4-(bytes%4) : 0;
	bytes += pad;
	bytes *= height;

	bmfh.bfType = 0x4d42;    // "BM"
	bmfh.bfSize = sizeof(BMP_BITMAPFILEHEADER) + sizeof(BMP_BITMAPINFOHEADER) + bytes;
	bmfh.bfReserved1 = 0;
	bmfh.bfReserved2 = 0;
	bmfh.bfOffBits = /*hack sizeof(BMP_BITMAPFILEHEADER)=14, sizeof doesn't work?*/
					 14 + sizeof(BMP_BITMAPINFOHEADER);

	bmih.biSize = sizeof(BMP_BITMAPINFOHEADER);
	bmih.biWidth = width;
	bmih.biHeight = height;
	bmih.biPlanes = 1;
	bmih.biBitCount = 24;
	bmih.biCompression = BMP_BI_RGB;
	bmih.biSizeImage = 0;
	bmih.biXPelsPerMeter = (int)(100 / 2.54 * 72);
	bmih.biYPelsPerMeter = (int)(100 / 2.54 * 72);
	bmih.biClrUsed = 0;
	bmih.biClrImportant = 0;

	FILE *foo=fopen(iname, "wb");

	//	fwrite(&bmfh, sizeof(BMP_BITMAPFILEHEADER), 1, foo);
	fwrite( &(bmfh.bfType), 2, 1, foo);
	fwrite( &(bmfh.bfSize), 4, 1, foo);
	fwrite( &(bmfh.bfReserved1), 2, 1, foo);
	fwrite( &(bmfh.bfReserved2), 2, 1, foo);
	fwrite( &(bmfh.bfOffBits), 4, 1, foo);

	fwrite(&bmih, sizeof(BMP_BITMAPINFOHEADER), 1, foo);

	bytes /= height;
	unsigned char* scanline = malloc(bytes);
	int i, j;
	for (j = 0; j < height; ++j ) {
		memcpy( scanline, data + j*3*width, bytes );
		for (i = 0; i < width; ++i ) {
			unsigned char temp = scanline[i*3];
			scanline[i*3] = scanline[i*3+2];
			scanline[i*3+2] = temp;
		}
		fwrite( scanline, bytes, 1, foo);
	}

	free(scanline);
	fclose(foo);
}
