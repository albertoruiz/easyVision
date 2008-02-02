#include "burns.h"
#include <math.h>
#include <stdlib.h>
#include <stdio.h>

/**
 * Convenient way to group image parameters which tend to go together.
 */
typedef struct Image8u {
	/** The actual pixel data of the image. */
	Ipp8u* pixels;
	/**
	 * Interval in bytes between consecutive rows. This may be different
	 * than the image width because ippiMalloc adds padding for better
	 * alignment.
	 */
	int step;
	/**
	 * Region of interest (image size).
	 */
	IppiSize roi;
} Image8u;

/**
 * Allocate a new image.
 */
static Image8u new_Image8u(int width, int height) {
	int step;
	Ipp8u* pixels = ippiMalloc_8u_C1(width, height, &step);
	IppiSize roi = {width, height};
	Image8u out = {pixels, step, roi};
	return out;
}

/**
 * Free memory associated with an image.
 */
static void free_Image8u(Image8u image) {
	ippiFree(image.pixels);
}

/**
 * Convenient way to group image parameters which tend to go together.
 */
typedef struct Image16s {
	/** The actual pixel data of the image. */
	Ipp16s* pixels;
	/**
	 * Interval in pixels between consecutive rows. Multiply this by
	 * sizeof(Ipp16s) before using in ippi functions.
	 */
	int step;
	IppiSize roi;
} Image16s;

/**
 * Allocate a new image.
 */
static Image16s new_Image16s(int width, int height) {
	int step;
	Ipp16s* pixels = ippiMalloc_16s_C1(width, height, &step);
	IppiSize roi = {width, height};
	Image16s out = {pixels, step/sizeof(Ipp16s), roi};
	return out;
}

/**
 * Free memory associated with an image.
 */
static void free_Image16s(Image16s image) {
	ippiFree(image.pixels);
}

/**
 * Allocate a new Line object.
 */
static Line* new_Line() {
	Line* out = malloc(sizeof(Line));
	out->num_pixels = 0;
	out->num_votes = 0;
}

void print_lines(Line* lines, int num_lines) {
	int i;
	printf("%d Lines:\n", num_lines);
	for (i = 0; i < num_lines; ++i) {
		Line* line = &lines[i];
		printf("Line from (%f %f) to (%f %f)\n",
			line->x1, line->y1,
			line->x2, line->y2);
	}
}

void free_lines(Line* lines) {
	free(lines);
}

/**
 * Part of a connected component of an image. Connected components are
 * represented by tree loops (i.e. n-ary trees where the root points to itself)
 * where each node is a Region.
 */
typedef struct Region {
	/** Pointer to the parent region. */
	struct Region* parent;
	/**
	 * Rank is a measure of the complexity of a connected component.
	 * It is used when merging two connected components to decide which
	 * will be the root node.
	 */
	int rank;
	/** Line associated with the region. */
	Line* line;
	/**
	 * Regions are allocated in a linked list for easy iteration, this
	 * points to the next region in the list.
	 */
	struct Region* next;
} Region;

/**
 * Allocate a new region / connected component.
 */
static Region* new_Region() {
	Region* out = malloc(sizeof(Region));
	out->parent = out;
	out->rank = 0;
	out->line = NULL;
	out->next = NULL;
	return out;
}

/**
 * Find the root Region of a connected component. As a side-effect this updates
 * the tree so that all the Regions on the path to the root point directly to
 * the root, speeding up future lookups.
 */
static Region* region_root(Region* c) {
	if (c->parent == c) return c;
	return c->parent = region_root(c->parent);
}

/**
 * Merge two connected components by linking one's root to the other. The
 * smaller component is always linked to the larger one, to avoid
 * badly unbalanced trees.
 */
static void union_regions(Region* a, Region* b) {
	if (a == b) return;
	Region* aroot = region_root(a);
	Region* broot = region_root(b);
	if (aroot->rank > broot->rank) {
		broot->parent = aroot;
	} else if (aroot->rank < broot->rank) {
		aroot->parent = broot;
	} else if (aroot != broot) {
		broot->parent = aroot;
		++aroot->rank;
	}
}

/**
 * Map of pixels to regions. This mainly exists for compatibility with the AT
 * macro.
 */
typedef struct ImageR {
	/** Array of pointers to regions. */
	Region** pixels;
	/** Interval (in units of sizeof(void*)) between consecutive rows. */
	int step;
	/** Region of interest; size of the image. */
	IppiSize roi;
} ImageR;

/**
 * Allocate a new region map.
 */
static ImageR new_ImageR(int width, int height) {
	IppiSize roi = {width, height};
	ImageR out = {calloc(width*height, sizeof(Region*)), width, roi};
	return out;
}

/**
 * Free data associated with a region map.
 */
static void free_ImageR(ImageR image) {
	free(image.pixels);
}

/**
 * Return the element at coordinates (x,y) as measured from the top left in an
 * Image8u-like struct.
 */
#define AT(image, x, y) (image).pixels[(y)*(image).step + (x)]

/**
 * Print the contents of a region map for debugging purposes.
 */
static void print_ImageR(ImageR image) {
	int x, y;
	for (y = 0; y < image.roi.height; ++y) {
		for (x = 0; x < image.roi.width; ++x) {
			printf("%p ", AT(image, x, y));
		}
		printf("\n");
	}
}

/**
 * Print the contents of a grayscale image for debugging purposes.
 */
static void print_Image8u(Image8u image) {
	int x, y;
	for (y = 0; y < image.roi.height; ++y) {
		for (x = 0; x < image.roi.width; ++x) {
			printf("%d", AT(image, x, y)/26);
		}
		printf("\n");
	}
}

/**
 * Calculate 4-connected components of the image based on gradient orientation.
 * See http://people.csail.mit.edu/rahimi/connected/ for more details.
 */
static ImageR line_support(Image8u gradient_m, Image8u gradient_q, Region** rtail) {
	int x, y;
	ImageR out = new_ImageR(gradient_m.roi.width, gradient_m.roi.height);

	Region* rhead = *rtail;

	// first pixel
	if (AT(gradient_m, 0, 0)) {
		AT(out, 0, 0) = (*rtail) = (*rtail)->next = new_Region();
	}
	// first row
	for (x = 1; x < gradient_m.roi.width; ++x) {
		if (AT(gradient_m, x, 0)) {
			if (AT(out, x-1, 0) && AT(gradient_q,x,0) == AT(gradient_q,x-1,0)) {
				// matches left pixel
				AT(out, x, 0) = AT(out, x-1, 0);
			} else {
				AT(out, x, 0) = (*rtail) = (*rtail)->next = new_Region();
			}
		}
	}
	// remaining rows
	for (y = 1; y < gradient_m.roi.height; ++y) {
		// first pixel of row
		if (AT(gradient_m, 0, y)) {
			if (AT(out, 0, y-1) && AT(gradient_q,0,y) == AT(gradient_q,0,y-1)) {
				// matches top pixel
				AT(out, 0, y) = AT(out, 0, y-1);
			} else {
				AT(out, 0, y) = (*rtail) = (*rtail)->next = new_Region();
			}
		}
		// remaining pixels of row
		for (x = 1; x < gradient_m.roi.width; ++x) {
			if (AT(gradient_m, x, y)) {
				if (AT(out, x-1, y) && AT(gradient_q,x,y) == AT(gradient_q,x-1,y)) {
					// matches left pixel
					AT(out, x, y) = AT(out, x-1, y);
					if (AT(out, x, y-1) && AT(gradient_q,x,y) == AT(gradient_q,x,y-1)) {
						// matches top and left pixels
						union_regions(AT(out, x, y), AT(out, x, y-1));
						AT(out, x, y) = AT(out, x, y-1);
					}
				} else if (AT(out, x, y-1) && AT(gradient_q,x,y-1) == AT(gradient_q,x,y)) {
					// matches top pixel
					AT(out, x, y) = AT(out, x, y-1);
				} else {
					// matches neither top nor left pixels
					AT(out, x, y) = (*rtail) = (*rtail)->next = new_Region();
				}
			}
		}
	}

	// Merge regions and compute dimensions of each region
	for (y = 0; y < gradient_m.roi.height; ++y) {
		for (x = 0; x < gradient_m.roi.width; ++x) {
			if (!AT(out, x, y)) continue;
			// Find the root for this connected component.
			AT(out, x, y) = region_root(AT(out, x, y));
			Region* region = AT(out, x, y);
			if (!region->line) {
				region->line = new_Line();
				region->line->x1 = region->line->x2 = x+1;
				region->line->y1 = region->line->y2 = y+1;
			} else {
				if (x+1 > region->line->x2) region->line->x2 = x+1;
				if (x+1 < region->line->x1) region->line->x1 = x+1;
				region->line->y2 = y+1;
			}
			++region->line->num_pixels;
		}
	}

	// remove temporary regions from the linked list
	Region* rtmp;
	while (rhead->next != NULL) {
		if (!rhead->next->line) {
			rtmp = rhead->next->next;
			free(rhead->next);
			rhead->next = rtmp;
		} else {
			rhead = rhead->next;
		}
	}
	*rtail = rhead;
	return out;
}

static void estimate_line_properties(ImageR regions, Image8u gradient_m, Image16s gradient_x, Image16s gradient_y) {
	int x, y;
	// calculate correct line parameters for good lines
	for (y = 0; y < regions.roi.height; ++y) {
		for (x = 0; x < regions.roi.width; ++x) {
			Region* region = AT(regions, x, y);
			if (!region) continue;
			Line* line = region->line;
			if (!line) continue;
			int new_num_pixels = line->num_pixels + 1;
			double weight = hypot(line->tangent_x, line->tangent_y) * line->num_pixels;
			double sx = AT(gradient_x, x, y);
			double sy = AT(gradient_y, x, y);
			line->tangent_x = line->tangent_x * line->num_pixels / (double)new_num_pixels + sx / new_num_pixels;
			line->tangent_y = line->tangent_y * line->num_pixels / (double)new_num_pixels + sy / new_num_pixels;
			double new_weight = hypot(line->tangent_x, line->tangent_y) * new_num_pixels;
			double current_weight = hypot(sx, sy);
			line->midpoint_x = (line->midpoint_x * weight + (x+1) * current_weight) / (weight + current_weight);
			line->midpoint_y = (line->midpoint_y * weight + (y+1) * current_weight) / (weight + current_weight);
			line->num_pixels = new_num_pixels;
		}
	}
}

static int min(int a, int b) {
	if (a <= b) return a;
	else return b;
}
static int max(int a, int b) {
	if (a >= b) return a;
	else return b;
}

void burns_line_extraction(Ipp8u* pixels, int step, int width, int height,
	int num_buckets, int min_gradient, double min_length,
	Line** lines, int* num_lines)
{
	IppiSize roi = {width, height};
	Image8u image = {pixels, step, roi};
	int x, y;

	// The edges of the image do not have a well-defined gradient (using
	// the Sobel filter), so we will ignore them for the purposes of this
	// algorithm.
	roi.height -= 2;
	roi.width -= 2;
	Image16s gradient_x = new_Image16s(roi.width, roi.height);
	ippiFilterSobelHoriz_8u16s_C1R(&AT(image, 1, 1), image.step, gradient_x.pixels, gradient_x.step*sizeof(Ipp16s), gradient_x.roi, ippMskSize3x3);
	Image16s gradient_y = new_Image16s(roi.width, roi.height);
	ippiFilterSobelVert_8u16s_C1R(&AT(image, 1, 1), image.step, gradient_y.pixels, gradient_y.step*sizeof(Ipp16s), gradient_y.roi, ippMskSize3x3);

	// Gradient magnitude and direction (in integer multiples of pi/8).
	// We could do this with ippiFilter but since we have to iterate
	// through the image anyways, doing the filtering at the same time
	// avoids unnecessary memory allocation.
	Image8u gradient_m = new_Image8u(roi.width, roi.height);
	// Gradient direction quantized into buckets starting at 0 degrees.
	Image8u gradient_q1 = new_Image8u(roi.width, roi.height);
	// Gradient direction quantized into buckets starting at 180/num_buckets
	// degrees (i.e. offset by half a bucket from gradient_q1).
	Image8u gradient_q2 = new_Image8u(roi.width, roi.height);
	for (y = 0; y < gradient_m.roi.height; ++y) {
		for (x = 0; x < gradient_m.roi.width; ++x) {
			int hgrad = AT(gradient_x, x, y);
			int vgrad = AT(gradient_y, x, y);
			int gmag = abs(hgrad)/4 + abs(vgrad)/4;
			if (gmag > min_gradient) {
				double theta = atan2((double)vgrad, (double)hgrad);
				int bucket = (int)floor((theta+M_PI) * (double)num_buckets/M_PI);
				AT(gradient_m, x, y) = gmag;
				AT(gradient_q1, x, y) = bucket/2;
				AT(gradient_q2, x, y) = ((bucket+1)%(2*num_buckets))/2;
			} else {
				AT(gradient_m, x, y) = 0;
				AT(gradient_q1, x, y) = 0;
				AT(gradient_q2, x, y) = 0;
			}
		}
	}

	// Find the line-support regions of the image; regions will be
	// collected in a linked list so we can iterate through them later.
	Region* rhead = new_Region();
	Region* rtail = rhead;
	ImageR regions1 = line_support(gradient_m, gradient_q1, &rtail);
	ImageR regions2 = line_support(gradient_m, gradient_q2, &rtail);
	// free temporary list head
	rtail = rhead;
	rhead = rhead->next;
	free(rtail);
	// gradient orientations no longer needed
	free_Image8u(gradient_q1);
	free_Image8u(gradient_q2);

	// approximate length of each region's line
	for (rtail = rhead; rtail != NULL; rtail = rtail->next) {
		Line* line = rtail->line;
		line->length = abs(line->x1 - line->x2)/2 + abs(line->y1 - line->y2)/2;
	}

	// vote on regions
	for (y = 0; y < regions1.roi.height; ++y) {
		for (x = 0; x < regions1.roi.width; ++x) {
			if (!AT(regions1, x, y)) continue;
			Region* c1 = AT(regions1, x, y);
			Region* c2 = AT(regions2, x, y);
			if (c1->line->length >= c2->line->length) {
				++c1->line->num_votes;
			} else {
				++c2->line->num_votes;
			}
		}
	}

	// discard bad (undervoted) lines
	for (rtail = rhead; rtail != NULL; rtail = rtail->next) {
		Line* line = rtail->line;
		if (line) {
			if (line->length < min_length || line->num_votes < line->num_pixels/2) {
				free(line);
				rtail->line = NULL;
			} else {
				// reset pixel counter for averages
				line->num_pixels = 0;
			}
		}
	}

	estimate_line_properties(regions1, gradient_m, gradient_x, gradient_y);
	estimate_line_properties(regions2, gradient_m, gradient_x, gradient_y);

	// calculate more accurate endpoints for lines
	for (rtail = rhead; rtail != NULL; rtail = rtail->next) {
		Line* line = rtail->line;
		if (line) {
			double xmin = line->x1; double xmax = line->x2;
			double ymin = line->y1; double ymax = line->y2;

			line->x1 = line->midpoint_x - (line->midpoint_y - ymin)*line->tangent_x/line->tangent_y;
			if (line->x1 < xmin) {
				line->x1 = xmin;
				line->y1 = line->midpoint_y - (line->midpoint_x - xmin)*line->tangent_y/line->tangent_x;
			} else if (line->x1 > xmax) {
				line->x1 = xmax;
				line->y1 = line->midpoint_y + (xmax - line->midpoint_x)*line->tangent_y/line->tangent_x;
			}
			line->x2 = line->midpoint_x + (ymax - line->midpoint_y)*line->tangent_x/line->tangent_y;
			if (line->x2 < xmin) {
				line->x2 = xmin;
				line->y2 = line->midpoint_y - (line->midpoint_x - xmin)*line->tangent_y/line->tangent_x;
			} else if (line->x2 > xmax) {
				line->x2 = xmax;
				line->y2 = line->midpoint_y + (xmax - line->midpoint_x)*line->tangent_y/line->tangent_x;
			}

			// re-calculate length
			line->length = hypot(line->x1 - line->x2, line->y1 - line->y2);
			// discard too-short lines
			if (line->length < min_length) {
				free(line);
				rtail->line = NULL;
			}
		}
	}

	// count lines
	*num_lines = 0;
	for (rtail = rhead; rtail != NULL; rtail = rtail->next) {
		Line* line = rtail->line;
		if (line) ++(*num_lines);
	}

	// gradient information no longer needed
	free_Image8u(gradient_m);
	free_Image16s(gradient_y);
	free_Image16s(gradient_x);
	// region maps no longer needed
	free_ImageR(regions1);
	free_ImageR(regions2);

	// Lines will be collected in an array
	*lines = malloc(*num_lines * sizeof(Line));
	int i = 0;

	// build output list and free regions
	while (rhead != NULL) {
		Line* line = rhead->line;
		if (line) {
			// found the root of a connected component with a good line
			(*lines)[i++] = *line;
			free(line);
		}
		// free the region and move to the next one
		rtail = rhead;
		rhead = rhead->next;
		free(rtail);
	}
}
