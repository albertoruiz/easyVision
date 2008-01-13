//
// bitmap.h
//
// header file for MS bitmap format
//
//

#ifndef BITMAP_H
#define BITMAP_H

unsigned char* read_bmp(const char* fname, int* width, int* height, int* step);
void write_bmp(const char *iname, int width, int height, unsigned char *data);

#endif
