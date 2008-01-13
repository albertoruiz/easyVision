#ifndef _BURNS_H
#define _BURNS_H

#include <ipp.h>

/**
 * Straight line segment.
 */
typedef struct Line {
	/** Coordinates of endpoints. */
	double x1, y1, x2, y2;
	/** Length of line. */
	double length;
	/** Number of pixels in the line's support. */
	unsigned int num_pixels;
	/** Number of pixels who voted for line. */
	unsigned int num_votes;
	/** Slope in the x-direction. */
	double tangent_x;
	/** Slope in the y-direction. */
	double tangent_y;
	/** Midpoint in the x-direction. */
	double midpoint_x;
	/** Midpoint in the y-direction. */
	double midpoint_y;
} Line;

void print_lines(Line* line, int num_lines);

void free_lines(Line* line);

/**
 * Find straight line segments in a grayscale image.
 * @Article{ Burns1,
 * 	title = "Extracting Straight Lines",
 * 	author = "J. B. Burns and A. R. Hanson and E. M. Riseman",
 * 	journal = "IEEE Transactions on Pattern Analysis and Machine Intelligence",
 * 	pages = "425--455",
 * 	month = jul,
 * 	year = "1986"
 * }
 *
 * @param pixels Array of bytes, the image data.
 * @param step Interval in bytes between consecutive rows in the image.
 * @param width Width of region of interest.
 * @param height Height of region of interest.
 * @param roi Region of interest, the size of the image.
 * @param num_buckets Increasing the number of buckets decreases the tolerance
 *        for variations in the gradient orientation along a line. Recommended
 *        value: 8.
 * @param min_gradient Treat gradients below this magnitude (taxi cab distance [0..255])
 *        as zero. Increasing this value eliminates lines with fuzzy boundaries.
 * @param min_length Do not return lines below this length.
 */
void burns_line_extraction(Ipp8u *image, int step, int width, int height,
		int num_buckets, int min_gradient, double min_length,
		Line** lines, int* num_lines);

void burns_line_extraction_demo(Line** lines, int* num_lines);

#endif /* _BURNS_H */
