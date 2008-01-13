#include "burns.h"
#include "bitmap.h"
#include <stdio.h>
#include <math.h>
#include <ippcc.h>
#include <GL/gl.h>
#include <GL/glut.h>

Ipp8u* rgb_pixels;
int rgb_step;
IppiSize rgb_roi;

Ipp8u* gray_pixels;
int gray_step;
IppiSize gray_roi;

void display() {
	glClear(GL_COLOR_BUFFER_BIT);
	glLineWidth(2);
	glRasterPos2f(0,0);
	glDrawPixels(gray_roi.width, gray_roi.height, GL_LUMINANCE, GL_UNSIGNED_BYTE, gray_pixels);

	Line* lines;
	int num_lines;
	int i;
	burns_line_extraction(gray_pixels, gray_step, gray_roi.width, gray_roi.height,
		8, 10, 5,
		&lines, &num_lines);

	glColor3f(1,0,0);
	glBegin(GL_LINES);
	for (i = 0; i < num_lines; ++i) {
		Line* line = &lines[i];
		glVertex3d(line->x1, line->y1, 0);
		glVertex3d(line->x2, line->y2, 0);
	}
	glEnd();

	glPointSize(3);
	glColor3f(0,0,1);
	glBegin(GL_POINTS);
	for (i = 0; i < num_lines; ++i) {
		Line* line = &lines[i];
		glVertex3d(line->midpoint_x, line->midpoint_y, 0);
	}
	glEnd();

	free_lines(lines, num_lines);
}

int main(int argc, char **argv) {
	IppiSize rgb_roi;
	rgb_pixels = read_bmp("demo.bmp",  &rgb_roi.width, &rgb_roi.height, &rgb_step);
	gray_pixels = ippiMalloc_8u_C1(rgb_roi.width, rgb_roi.height, &gray_step);
	gray_roi = rgb_roi;

	ippiRGBToGray_8u_C3C1R(rgb_pixels, rgb_step, gray_pixels, gray_step, rgb_roi);

	glutInit(&argc, argv);
	glutInitWindowSize(gray_roi.width, gray_roi.height);
	glutInitWindowPosition(0, 0);
	glutInitDisplayMode(GLUT_RGB);
	glutCreateWindow("window");
	glClearColor(0.0, 0.0, 0.0, 0.0);
	glMatrixMode(GL_MODELVIEW);
	glLoadIdentity();
	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();
	glOrtho(0, gray_roi.width, 0, gray_roi.height, -1, 1);
	glutDisplayFunc(display);
	glutMainLoop();
	return 0;
}
