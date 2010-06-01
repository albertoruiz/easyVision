/*******************************************************************************
#	 	uvcview: Sdl video Usb Video Class grabber           .         #
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

#include <stdlib.h>
#include <math.h>
#include <float.h>

#include "v4l2uvc.h"
#include "utils.h"

#define ARRAY_SIZE(a)		(sizeof(a) / sizeof((a)[0]))
#define FOURCC_FORMAT		"%c%c%c%c"
#define FOURCC_ARGS(c)		(c) & 0xFF, ((c) >> 8) & 0xFF, ((c) >> 16) & 0xFF, ((c) >> 24) & 0xFF
	

static int debug = 0;



static int init_v4l2(struct vdIn *vd);

static int float_to_fraction_recursive(double f, double p, int *num, int *den)
{
	int whole = (int)f;
	f = fabs(f - whole);

	if(f > p) {
		int n, d;
		int a = float_to_fraction_recursive(1 / f, p + p / f, &n, &d);
		*num = d;
		*den = d * a + n;
	}
	else {
		*num = 0;
		*den = 1;
	}
	return whole;
}

static void float_to_fraction(float f, int *num, int *den)
{
	int whole = float_to_fraction_recursive(f, FLT_EPSILON, num, den);
	*num += whole * *den;
}

int check_videoIn(struct vdIn *vd, char *device)
{
	int ret;
	if (vd == NULL || device == NULL)
		return -1;
	vd->videodevice = (char *) calloc(1, 16 * sizeof(char));
	snprintf(vd->videodevice, 12, "%s", device);
	printf("Device information:\n");
	printf("  Device path:  %s\n", vd->videodevice);
	if ((vd->fd = open(vd->videodevice, O_RDWR)) == -1) {
		perror("ERROR opening V4L interface");
		exit(1);
	}
	memset(&vd->cap, 0, sizeof(struct v4l2_capability));
	ret = ioctl(vd->fd, VIDIOC_QUERYCAP, &vd->cap);
	if (ret < 0) {
		printf("Error opening device %s: unable to query device.\n",
				vd->videodevice);
		goto fatal;
	}
	if ((vd->cap.capabilities & V4L2_CAP_VIDEO_CAPTURE) == 0) {
		printf("Error opening device %s: video capture not supported.\n",
				vd->videodevice);
	}
	if (!(vd->cap.capabilities & V4L2_CAP_STREAMING)) {
		printf("%s does not support streaming i/o\n", vd->videodevice);
	}
	if (!(vd->cap.capabilities & V4L2_CAP_READWRITE)) {
		printf("%s does not support read i/o\n", vd->videodevice);
	}
	enum_frame_formats(vd->fd, NULL, 0);
fatal:    
	close(vd->fd);
	free(vd->videodevice);
	return 0;
}
int
init_videoIn(struct vdIn *vd, char *device, int width, int height, float fps,
	     int format, int grabmethod, char *avifilename)
{
   int ret = -1;
    int i;
    if (vd == NULL || device == NULL)
	return -1;
    if (width == 0 || height == 0)
	return -1;
    if (grabmethod < 0 || grabmethod > 1)
	grabmethod = 1;		//mmap by default;
    vd->videodevice = NULL;
    vd->status = NULL;
    vd->pictName = NULL;
    vd->videodevice = (char *) calloc(1, 16 * sizeof(char));
    vd->status = (char *) calloc(1, 100 * sizeof(char));
    vd->pictName = (char *) calloc(1, 80 * sizeof(char));
    snprintf(vd->videodevice, 12, "%s", device);
	printf("Device information:\n");
	printf("  Device path:  %s\n", vd->videodevice);
    vd->toggleAvi = 0;
    vd->avifile = NULL;
    vd->avifilename = avifilename;
    vd->recordtime = 0;
    vd->framecount = 0;
    vd->recordstart = 0;
    vd->getPict = 0;
    vd->signalquit = 1;
    vd->width = width;
    vd->height = height;
    vd->fps = fps;
    vd->formatIn = format;
    vd->grabmethod = grabmethod;
    vd->fileCounter = 0;
    vd->rawFrameCapture = 0;
    vd->rfsBytesWritten = 0;
    vd->rfsFramesWritten = 0;
    vd->captureFile = NULL;
    vd->bytesWritten = 0;
    vd->framesWritten = 0;
    if (init_v4l2(vd) < 0) {
	printf(" Init v4L2 failed !! exit fatal\n");
	goto error;;
    }
    /* alloc a temp buffer to reconstruct the pict */
    vd->framesizeIn = (vd->width * vd->height << 1);
    switch (vd->formatIn) {
    case V4L2_PIX_FMT_MJPEG:
	vd->tmpbuffer =
	    (unsigned char *) calloc(1, (size_t) vd->framesizeIn);
	if (!vd->tmpbuffer)
	    goto error;
	vd->framebuffer =
	    (unsigned char *) calloc(1,
				     (size_t) vd->width * (vd->height +
							   8) * 2);
	break;
    case V4L2_PIX_FMT_YUYV:
	vd->framebuffer =
	    (unsigned char *) calloc(1, (size_t) vd->framesizeIn);
	break;
    default:
	printf(" should never arrive exit fatal !!\n");
	goto error;
	break;
    }
    if (!vd->framebuffer)
	goto error;
    return 0;
  error:
    free(vd->videodevice);
    free(vd->status);
    free(vd->pictName);
    close(vd->fd);
    return -1;
}
int enum_controls(int vd) //struct vdIn *vd)
{    
  struct v4l2_queryctrl queryctrl;
  struct v4l2_querymenu querymenu;
  struct v4l2_control   control_s;
  struct v4l2_input*    getinput;

  //Name of the device
  getinput=(struct v4l2_input *) calloc(1, sizeof(struct v4l2_input));
  memset(getinput, 0, sizeof(struct v4l2_input));
  getinput->index=0;
  ioctl(vd,VIDIOC_ENUMINPUT , getinput);
  printf ("Available controls of device '%s' (Type 1=Integer 2=Boolean 3=Menu 4=Button)\n", getinput->name);

  //subroutine to read menu items of controls with type 3
  void enumerate_menu (void) {
    printf ("  Menu items:\n");
    memset (&querymenu, 0, sizeof (querymenu));
    querymenu.id = queryctrl.id;
    for (querymenu.index = queryctrl.minimum;
         querymenu.index <= queryctrl.maximum;
         querymenu.index++) {
      if (0 == ioctl (vd, VIDIOC_QUERYMENU, &querymenu)) {
        printf ("  index:%d name:%s\n", querymenu.index, querymenu.name);
	SDL_Delay(10);
      } else {
        printf ("error getting control menu");
        break;
      }
    }
  }

  //predefined controls
  printf ("V4L2_CID_BASE         (predefined controls):\n");
  memset (&queryctrl, 0, sizeof (queryctrl));
  for (queryctrl.id = V4L2_CID_BASE;
       queryctrl.id < V4L2_CID_LASTP1;
       queryctrl.id++) {
    if (0 == ioctl (vd, VIDIOC_QUERYCTRL, &queryctrl)) {
      if (queryctrl.flags & V4L2_CTRL_FLAG_DISABLED)
        continue;
      control_s.id=queryctrl.id;
      ioctl(vd, VIDIOC_G_CTRL, &control_s);
      SDL_Delay(10);
      printf (" index:%-10d name:%-32s type:%d min:%-5d max:%-5d step:%-5d def:%-5d now:%d\n",
              queryctrl.id, queryctrl.name, queryctrl.type, queryctrl.minimum,
              queryctrl.maximum, queryctrl.step, queryctrl.default_value, control_s.value);
      if (queryctrl.type == V4L2_CTRL_TYPE_MENU)
        enumerate_menu ();
    } else {
      if (errno == EINVAL)
        continue;
      perror ("error getting base controls");
      goto fatal_controls;
    }
  }

  //driver specific controls
  printf ("V4L2_CID_PRIVATE_BASE (driver specific controls):\n");
  for (queryctrl.id = V4L2_CID_PRIVATE_BASE;;
       queryctrl.id++) {
    if (0 == ioctl (vd, VIDIOC_QUERYCTRL, &queryctrl)) {
      if (queryctrl.flags & V4L2_CTRL_FLAG_DISABLED)
        continue;
      control_s.id=queryctrl.id;
      ioctl(vd, VIDIOC_G_CTRL, &control_s);
      SDL_Delay(20);
      printf (" index:%-10d name:%-32s type:%d min:%-5d max:%-5d step:%-5d def:%-5d now:%d\n",
              queryctrl.id, queryctrl.name, queryctrl.type, queryctrl.minimum,
              queryctrl.maximum, queryctrl.step, queryctrl.default_value, control_s.value);
      if (queryctrl.type == V4L2_CTRL_TYPE_MENU)
        enumerate_menu ();
    } else {
      if (errno == EINVAL)
        break;
      perror ("error getting private base controls");
      goto fatal_controls;
      }
  }
  return 0;
 fatal_controls:
  return -1;  
}
int save_controls(int vd)
{ 
  struct v4l2_queryctrl queryctrl;
  struct v4l2_control   control_s;
  FILE *configfile;
  memset (&queryctrl, 0, sizeof (queryctrl));
  memset (&control_s, 0, sizeof (control_s));
  configfile = fopen("luvcview.cfg", "w");
  if ( configfile == NULL) {
    perror("saving configfile luvcview.cfg failed");
  }
  else {
    fprintf(configfile, "id         value      # luvcview control settings configuration file\n");
    for (queryctrl.id = V4L2_CID_BASE;
         queryctrl.id < V4L2_CID_LASTP1;
         queryctrl.id++) {
      if (0 == ioctl (vd, VIDIOC_QUERYCTRL, &queryctrl)) {
        if (queryctrl.flags & V4L2_CTRL_FLAG_DISABLED)
          continue;
        control_s.id=queryctrl.id;
        ioctl(vd, VIDIOC_G_CTRL, &control_s);
        SDL_Delay(10);
        fprintf (configfile, "%-10d %-10d # name:%-32s type:%d min:%-5d max:%-5d step:%-5d def:%d\n",
                 queryctrl.id, control_s.value, queryctrl.name, queryctrl.type, queryctrl.minimum,
                 queryctrl.maximum, queryctrl.step, queryctrl.default_value);
        printf ("%-10d %-10d # name:%-32s type:%d min:%-5d max:%-5d step:%-5d def:%d\n",
                queryctrl.id, control_s.value, queryctrl.name, queryctrl.type, queryctrl.minimum,
                queryctrl.maximum, queryctrl.step, queryctrl.default_value);
        SDL_Delay(10);
      }
    }
    for (queryctrl.id = V4L2_CID_PRIVATE_BASE;;
         queryctrl.id++) {
      if (0 == ioctl (vd, VIDIOC_QUERYCTRL, &queryctrl)) {
        if (queryctrl.flags & V4L2_CTRL_FLAG_DISABLED)
          continue;
        if ((queryctrl.id==134217735) || (queryctrl.id==134217736))
          continue;
        control_s.id=queryctrl.id;
        ioctl(vd, VIDIOC_G_CTRL, &control_s);
        SDL_Delay(10);
        fprintf (configfile, "%-10d %-10d # name:%-32s type:%d min:%-5d max:%-5d step:%-5d def:%d\n",
                 queryctrl.id, control_s.value, queryctrl.name, queryctrl.type, queryctrl.minimum,
                 queryctrl.maximum, queryctrl.step, queryctrl.default_value);
        printf ("%-10d %-10d # name:%-32s type:%d min:%-5d max:%-5d step:%-5d def:%d\n",
                queryctrl.id, control_s.value, queryctrl.name, queryctrl.type, queryctrl.minimum,
                queryctrl.maximum, queryctrl.step, queryctrl.default_value);
      } else {
        if (errno == EINVAL)
          break;
      }
    }
    fflush(configfile);
    fclose(configfile);
    SDL_Delay(100);
  }
}


int load_controls(int vd) //struct vdIn *vd)
{
  struct v4l2_control   control;
  FILE *configfile;
  memset (&control, 0, sizeof (control));
  configfile = fopen("luvcview.cfg", "r");
  if ( configfile == NULL) {
    perror("configfile luvcview.cfg open failed");
  }
  else {
    printf("loading controls from luvcview.cfg\n");
    char buffer[512]; 
    fgets(buffer, sizeof(buffer), configfile);
    while (NULL !=fgets(buffer, sizeof(buffer), configfile) )
      {
        sscanf(buffer, "%i%i", &control.id, &control.value);
        if (ioctl(vd, VIDIOC_S_CTRL, &control))
          printf("ERROR id:%d val:%d\n", control.id, control.value);
        else
          printf("OK    id:%d val:%d\n", control.id, control.value);
        SDL_Delay(20);
      }   
    fclose(configfile);
  }
}

static int init_v4l2(struct vdIn *vd)
{
	int i;
	int ret = 0;

	if ((vd->fd = open(vd->videodevice, O_RDWR)) == -1) {
		perror("ERROR opening V4L interface");
		exit(1);
	}
	memset(&vd->cap, 0, sizeof(struct v4l2_capability));
	ret = ioctl(vd->fd, VIDIOC_QUERYCAP, &vd->cap);
	if (ret < 0) {
		printf("Error opening device %s: unable to query device.\n",
				vd->videodevice);
		goto fatal;
	}

	if ((vd->cap.capabilities & V4L2_CAP_VIDEO_CAPTURE) == 0) {
		printf("Error opening device %s: video capture not supported.\n",
				vd->videodevice);
		goto fatal;;
	}
	if (vd->grabmethod) {
		if (!(vd->cap.capabilities & V4L2_CAP_STREAMING)) {
			printf("%s does not support streaming i/o\n", vd->videodevice);
			goto fatal;
		}
	} else {
		if (!(vd->cap.capabilities & V4L2_CAP_READWRITE)) {
			printf("%s does not support read i/o\n", vd->videodevice);
			goto fatal;
		}
	}

	printf("Stream settings:\n");

	// Enumerate the supported formats to check whether the requested one
	// is available. If not, we try to fall back to YUYV.
	unsigned int device_formats[16] = { 0 };	// Assume no device supports more than 16 formats
	int requested_format_found = 0, fallback_format = -1;
	if(enum_frame_formats(vd->fd, device_formats, ARRAY_SIZE(device_formats))) {
		printf("Unable to enumerate frame formats");
		goto fatal;
	}
	for(i = 0; i < ARRAY_SIZE(device_formats) && device_formats[i]; i++) {
		if(device_formats[i] == vd->formatIn) {
			requested_format_found = 1;
			break;
		}
		if(device_formats[i] == V4L2_PIX_FMT_MJPEG || device_formats[i] == V4L2_PIX_FMT_YUYV)
			fallback_format = i;
	}
	if(requested_format_found) {
		// The requested format is supported
		printf("  Frame format: "FOURCC_FORMAT"\n", FOURCC_ARGS(vd->formatIn));
	}
	else if(fallback_format >= 0) {
		// The requested format is not supported but there's a fallback format
		printf("  Frame format: "FOURCC_FORMAT" ("FOURCC_FORMAT
			" is not supported by device)\n",
			FOURCC_ARGS(device_formats[0]), FOURCC_ARGS(vd->formatIn));
		vd->formatIn = device_formats[0];
	}
	else {
		// The requested format is not supported and no fallback format is available
		printf("ERROR: Requested frame format "FOURCC_FORMAT" is not available "
			"and no fallback format was found.\n", FOURCC_ARGS(vd->formatIn));
		goto fatal;
	}

	// Set pixel format and frame size
	memset(&vd->fmt, 0, sizeof(struct v4l2_format));
	vd->fmt.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
	vd->fmt.fmt.pix.width = vd->width;
	vd->fmt.fmt.pix.height = vd->height;
	vd->fmt.fmt.pix.pixelformat = vd->formatIn;
	vd->fmt.fmt.pix.field = V4L2_FIELD_ANY;
	ret = ioctl(vd->fd, VIDIOC_S_FMT, &vd->fmt);
	if (ret < 0) {
		perror("Unable to set format");
		goto fatal;
	}
	if ((vd->fmt.fmt.pix.width != vd->width) ||
		(vd->fmt.fmt.pix.height != vd->height)) {
		printf("  Frame size:   %ux%u (requested size %ux%u is not supported by device)\n",
			vd->fmt.fmt.pix.width, vd->fmt.fmt.pix.height, vd->width, vd->height);
		vd->width = vd->fmt.fmt.pix.width;
		vd->height = vd->fmt.fmt.pix.height;
		/* look the format is not part of the deal ??? */
		//vd->formatIn = vd->fmt.fmt.pix.pixelformat;
	}
	else {
		printf("  Frame size:   %dx%d\n", vd->width, vd->height);
	}

	/* set framerate */
	struct v4l2_streamparm* setfps;  
	setfps=(struct v4l2_streamparm *) calloc(1, sizeof(struct v4l2_streamparm));
	memset(setfps, 0, sizeof(struct v4l2_streamparm));
	setfps->type = V4L2_BUF_TYPE_VIDEO_CAPTURE;

	// Convert the frame rate into a fraction for V4L2
	int n = 0, d = 0;
	float_to_fraction(vd->fps, &n, &d);
	setfps->parm.capture.timeperframe.numerator = d;
	setfps->parm.capture.timeperframe.denominator = n;

	ret = ioctl(vd->fd, VIDIOC_S_PARM, setfps);
	if(ret == -1) {
		perror("Unable to set frame rate");
		goto fatal;
	}
	ret = ioctl(vd->fd, VIDIOC_G_PARM, setfps); 
	if(ret == 0) {
		float confirmed_fps = (float)setfps->parm.capture.timeperframe.denominator / (float)setfps->parm.capture.timeperframe.numerator;
		if (confirmed_fps != (float)n / (float)d) {
			printf("  Frame rate:   %g fps (requested frame rate %g fps is "
				"not supported by device)\n",
				confirmed_fps,
				vd->fps);
			vd->fps = confirmed_fps;
		}
		else {
			printf("  Frame rate:   %g fps\n", vd->fps);
		}
	}
	else {
		perror("Unable to read out current frame rate");
		goto fatal;
	}

	/* request buffers */
	memset(&vd->rb, 0, sizeof(struct v4l2_requestbuffers));
	vd->rb.count = NB_BUFFER;
	vd->rb.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
	vd->rb.memory = V4L2_MEMORY_MMAP;

	ret = ioctl(vd->fd, VIDIOC_REQBUFS, &vd->rb);
	if (ret < 0) {
		perror("Unable to allocate buffers");
		goto fatal;
	}
	/* map the buffers */
	for (i = 0; i < NB_BUFFER; i++) {
		memset(&vd->buf, 0, sizeof(struct v4l2_buffer));
		vd->buf.index = i;
		vd->buf.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
		vd->buf.memory = V4L2_MEMORY_MMAP;
		ret = ioctl(vd->fd, VIDIOC_QUERYBUF, &vd->buf);
		if (ret < 0) {
			perror("Unable to query buffer");
			goto fatal;
		}
		if (debug)
			printf("length: %u offset: %u\n", vd->buf.length,
					vd->buf.m.offset);
		vd->mem[i] = mmap(0 /* start anywhere */ ,
				vd->buf.length, PROT_READ, MAP_SHARED, vd->fd,
				vd->buf.m.offset);
		if (vd->mem[i] == MAP_FAILED) {
			perror("Unable to map buffer");
			goto fatal;
		}
		if (debug)
			printf("Buffer mapped at address %p.\n", vd->mem[i]);
	}
	/* Queue the buffers. */
	for (i = 0; i < NB_BUFFER; ++i) {
		memset(&vd->buf, 0, sizeof(struct v4l2_buffer));
		vd->buf.index = i;
		vd->buf.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
		vd->buf.memory = V4L2_MEMORY_MMAP;
		ret = ioctl(vd->fd, VIDIOC_QBUF, &vd->buf);
		if (ret < 0) {
			perror("Unable to queue buffer");
			goto fatal;;
		}
	}
	return 0;
fatal:
	return -1;

}

static int video_enable(struct vdIn *vd)
{
    int type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
    int ret;

    ret = ioctl(vd->fd, VIDIOC_STREAMON, &type);
    if (ret < 0) {
	perror("Unable to start capture");
	return ret;
    }
    vd->isstreaming = 1;
    return 0;
}

static int video_disable(struct vdIn *vd)
{
    int type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
    int ret;

    ret = ioctl(vd->fd, VIDIOC_STREAMOFF, &type);
    if (ret < 0) {
	perror("Unable to stop capture");
	return ret;
    }
    vd->isstreaming = 0;
    return 0;
}


int uvcGrab(struct vdIn *vd)
{
#define HEADERFRAME1 0xaf
    int ret;

    if (!vd->isstreaming)
	if (video_enable(vd))
	    goto err;
    memset(&vd->buf, 0, sizeof(struct v4l2_buffer));
    vd->buf.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
    vd->buf.memory = V4L2_MEMORY_MMAP;
    ret = ioctl(vd->fd, VIDIOC_DQBUF, &vd->buf);
    if (ret < 0) {
	perror("Unable to dequeue buffer");
	goto err;
    }

	/* Capture a single raw frame */
	if (vd->rawFrameCapture && vd->buf.bytesused > 0) {
		FILE *frame = NULL;
		char filename[13];
		int ret;

		/* Disable frame capturing unless we're in frame stream mode */
		if(vd->rawFrameCapture == 1)
			vd->rawFrameCapture = 0;

		/* Create a file name and open the file */
		sprintf(filename, "frame%03u.raw", vd->fileCounter++ % 1000);
		frame = fopen(filename, "wb");
		if(frame == NULL) {
			perror("Unable to open file for raw frame capturing");
			goto end_capture;
		}
		
		/* Write the raw data to the file */
		ret = fwrite(vd->mem[vd->buf.index], vd->buf.bytesused, 1, frame);
		if(ret < 1) {
			perror("Unable to write to file");
			goto end_capture;
		}
		printf("Saved raw frame to %s (%u bytes)\n", filename, vd->buf.bytesused);
		if(vd->rawFrameCapture == 2) {
			vd->rfsBytesWritten += vd->buf.bytesused;
			vd->rfsFramesWritten++;
		}


		/* Clean up */
end_capture:
		if(frame)
			fclose(frame);
	}

   

	/* Capture raw stream data */
	if (vd->captureFile && vd->buf.bytesused > 0) {
		int ret;
		ret = fwrite(vd->mem[vd->buf.index], vd->buf.bytesused, 1, vd->captureFile);
		if (ret < 1) {
			perror("Unable to write raw stream to file");
			fprintf(stderr, "Stream capturing terminated.\n");
			fclose(vd->captureFile);
			vd->captureFile = NULL;
			vd->framesWritten = 0;
			vd->bytesWritten = 0;
		} else {
			vd->framesWritten++;
			vd->bytesWritten += vd->buf.bytesused;
			if (debug)
				printf("Appended raw frame to stream file (%u bytes)\n", vd->buf.bytesused);
		}
	}

    switch (vd->formatIn) {
    case V4L2_PIX_FMT_MJPEG:
        if(vd->buf.bytesused <= HEADERFRAME1) {	/* Prevent crash on empty image */
/*	    if(debug)*/
	        printf("Ignoring empty buffer ...\n");
	    return 0;
        }
	memcpy(vd->tmpbuffer, vd->mem[vd->buf.index],vd->buf.bytesused);
	 /* avi recording is toggled on */
    if (vd->toggleAvi) {
        /* if vd->avifile is NULL, then we need to initialize it */
        if (vd->avifile == NULL) {
            vd->avifile = AVI_open_output_file(vd->avifilename);

            /* if avifile is NULL, there was an error */
            if (vd->avifile == NULL ) {
                fprintf(stderr,"Error opening avifile %s\n",vd->avifilename);
            }
            else {
                /* we default the fps to 15, we'll reset it on close */
                AVI_set_video(vd->avifile, vd->width, vd->height,
                    15, "MJPG");
                printf("recording to %s\n",vd->avifilename);
            }
        } else {
        /* if we have a valid avifile, record the frame to it */
            AVI_write_frame(vd->avifile, vd->tmpbuffer,
                vd->buf.bytesused, vd->framecount);
            vd->framecount++;
        }
    }
	if (jpeg_decode(&vd->framebuffer, vd->tmpbuffer, &vd->width,
	     &vd->height) < 0) {
	    printf("jpeg decode errors\n");
	    goto err;
	}
	if (debug)
	    printf("bytes in used %d\n", vd->buf.bytesused);
	break;
    case V4L2_PIX_FMT_YUYV:
	if (vd->buf.bytesused > vd->framesizeIn)
	    memcpy(vd->framebuffer, vd->mem[vd->buf.index],
		   (size_t) vd->framesizeIn);
	else
	    memcpy(vd->framebuffer, vd->mem[vd->buf.index],
		   (size_t) vd->buf.bytesused);
	break;
    default:
	goto err;
	break;
    }
    ret = ioctl(vd->fd, VIDIOC_QBUF, &vd->buf);
    if (ret < 0) {
	perror("Unable to requeue buffer");
	goto err;
    }

    return 0;
  err:
    vd->signalquit = 0;
    return -1;
}
int close_v4l2(struct vdIn *vd)
{
    if (vd->isstreaming)
	video_disable(vd);
    if (vd->tmpbuffer)
	free(vd->tmpbuffer);
    vd->tmpbuffer = NULL;
    free(vd->framebuffer);
    vd->framebuffer = NULL;
    free(vd->videodevice);
    free(vd->status);
    free(vd->pictName);
    vd->videodevice = NULL;
    vd->status = NULL;
    vd->pictName = NULL;
}

/* return >= 0 ok otherwhise -1 */
static int isv4l2Control(struct vdIn *vd, int control,
			 struct v4l2_queryctrl *queryctrl)
{
int err =0;
    queryctrl->id = control;
    if ((err= ioctl(vd->fd, VIDIOC_QUERYCTRL, queryctrl)) < 0) {
	perror("ioctl querycontrol error");
    } else if (queryctrl->flags & V4L2_CTRL_FLAG_DISABLED) {
	printf("control %s disabled\n", (char *) queryctrl->name);
    } else if (queryctrl->flags & V4L2_CTRL_TYPE_BOOLEAN) {
	return 1;
    } else if (queryctrl->type & V4L2_CTRL_TYPE_INTEGER) {
	return 0;
    } else {
	printf("contol %s unsupported\n", (char *) queryctrl->name);
    }
    return -1;
}

int v4l2GetControl(struct vdIn *vd, int control)
{
    struct v4l2_queryctrl queryctrl;
    struct v4l2_control control_s;
    int err;
    if (isv4l2Control(vd, control, &queryctrl) < 0)
	return -1;
    control_s.id = control;
    if ((err = ioctl(vd->fd, VIDIOC_G_CTRL, &control_s)) < 0) {
	printf("ioctl get control error\n");
	return -1;
    }
    return control_s.value;
}

int v4l2SetControl(struct vdIn *vd, int control, int value)
{
    struct v4l2_control control_s;
    struct v4l2_queryctrl queryctrl;
    int min, max, step, val_def;
    int err;
    if (isv4l2Control(vd, control, &queryctrl) < 0)
	return -1;
    min = queryctrl.minimum;
    max = queryctrl.maximum;
    step = queryctrl.step;
    val_def = queryctrl.default_value;
    if ((value >= min) && (value <= max)) {
	control_s.id = control;
	control_s.value = value;
	if ((err = ioctl(vd->fd, VIDIOC_S_CTRL, &control_s)) < 0) {
	    printf("ioctl set control error\n");
	    return -1;
	}
    }
    return 0;
}
int v4l2UpControl(struct vdIn *vd, int control)
{
    struct v4l2_control control_s;
    struct v4l2_queryctrl queryctrl;
    int min, max, current, step, val_def;
    int err;

    if (isv4l2Control(vd, control, &queryctrl) < 0)
	return -1;
    min = queryctrl.minimum;
    max = queryctrl.maximum;
    step = queryctrl.step;
    val_def = queryctrl.default_value;
    current = v4l2GetControl(vd, control);
    current += step;
    printf("max %d, min %d, step %d, default %d ,current %d\n",max,min,step,val_def,current);
    if (current <= max) {
	control_s.id = control;
	control_s.value = current;
	if ((err = ioctl(vd->fd, VIDIOC_S_CTRL, &control_s)) < 0) {
	    printf("ioctl set control error\n");
	    return -1;
	}            
        printf ("Control name:%s set to value:%d\n", queryctrl.name, control_s.value);
    } else {
      printf ("Control name:%s already has max value:%d\n", queryctrl.name, max); 
    }
     return control_s.value;
}
int v4l2DownControl(struct vdIn *vd, int control)
{
    struct v4l2_control control_s;
    struct v4l2_queryctrl queryctrl;
    int min, max, current, step, val_def;
    int err;
    if (isv4l2Control(vd, control, &queryctrl) < 0)
	return -1;
    min = queryctrl.minimum;
    max = queryctrl.maximum;
    step = queryctrl.step;
    val_def = queryctrl.default_value;
    current = v4l2GetControl(vd, control);
    current -= step;
    printf("max %d, min %d, step %d, default %d ,current %d\n",max,min,step,val_def,current);
    if (current >= min) {
	control_s.id = control;
	control_s.value = current;
	if ((err = ioctl(vd->fd, VIDIOC_S_CTRL, &control_s)) < 0) {
	    printf("ioctl set control error\n");
	    return -1;
	}
    printf ("Control name:%s set to value:%d\n", queryctrl.name, control_s.value);
    }
    else {
      printf ("Control name:%s already has min value:%d\n", queryctrl.name, min); 
    }
    return control_s.value;
}
int v4l2ToggleControl(struct vdIn *vd, int control)
{
    struct v4l2_control control_s;
    struct v4l2_queryctrl queryctrl;
    int current;
    int err;
    if (isv4l2Control(vd, control, &queryctrl) != 1)
	return -1;
    current = v4l2GetControl(vd, control);
    control_s.id = control;
    control_s.value = !current;
    if ((err = ioctl(vd->fd, VIDIOC_S_CTRL, &control_s)) < 0) {
	printf("ioctl toggle control error\n");
	return -1;
    }
    return control_s.value;
}
int v4l2ResetControl(struct vdIn *vd, int control)
{
    struct v4l2_control control_s;
    struct v4l2_queryctrl queryctrl;
    int val_def;
    int err;
    if (isv4l2Control(vd, control, &queryctrl) < 0)
	return -1;
    val_def = queryctrl.default_value;
    control_s.id = control;
    control_s.value = val_def;
    if ((err = ioctl(vd->fd, VIDIOC_S_CTRL, &control_s)) < 0) {
	printf("ioctl reset control error\n");
	return -1;
    }

    return 0;
}

int v4l2ResetPan(struct vdIn *vd)
{
	int control = V4L2_CID_PAN_RESET;
	struct v4l2_control control_s;
	struct v4l2_queryctrl queryctrl;
	int err;

	if (isv4l2Control(vd, control, &queryctrl) < 0)
		return -1;

	control_s.id = control;
	control_s.value = 1;
	if ((err = ioctl(vd->fd, VIDIOC_S_CTRL, &control_s)) < 0) {
		printf("ERROR: Unable to reset pan (error = %d)\n", errno);
		return -1;
	}

	return 0;
}

int v4l2ResetTilt(struct vdIn *vd)
{
	int control = V4L2_CID_TILT_RESET;
	struct v4l2_control control_s;
	struct v4l2_queryctrl queryctrl;
	int err;

	if (isv4l2Control(vd, control, &queryctrl) < 0)
		return -1;

	control_s.id = control;
	control_s.value = 1;
	if ((err = ioctl(vd->fd, VIDIOC_S_CTRL, &control_s)) < 0) {
		printf("ERROR: Unable to reset tilt (error = %d)\n", errno);
		return -1;
	}

	return 0;
}

int v4l2ResetPanTilt(struct vdIn *vd)
{
	int control = V4L2_CID_PANTILT_RESET;
	struct v4l2_control control_s;
	struct v4l2_queryctrl queryctrl;
	unsigned char val;
	int err;

	if (isv4l2Control(vd, control, &queryctrl) < 0)
		return -1;

	control_s.id = control;
	control_s.value = 3;
	if ((err = ioctl(vd->fd, VIDIOC_S_CTRL, &control_s)) < 0) {
		printf("ERROR: Unable to reset pan/tilt (error = %d)\n", errno);
		return -1;
	}

	return 0;
}

int v4L2UpDownPan(struct vdIn *vd, short inc)
{
	int control = V4L2_CID_PAN_RELATIVE;
	struct v4l2_control control_s;
	struct v4l2_queryctrl queryctrl;
	int err;

	if (isv4l2Control(vd, control, &queryctrl) < 0)
		return -1;
	control_s.id = control;
	control_s.value = inc;
	if ((err = ioctl(vd->fd, VIDIOC_S_CTRL, &control_s)) < 0) {
		printf("ioctl pan updown control error\n");
		return -1;
	}
	return 0;
}

int v4L2UpDownTilt(struct vdIn *vd, short inc)
{   int control = V4L2_CID_TILT_RELATIVE;
    struct v4l2_control control_s;
    struct v4l2_queryctrl queryctrl;
    int err;

    if (isv4l2Control(vd, control, &queryctrl) < 0)
	return -1;  
    control_s.id = control;
    control_s.value = inc;
    if ((err = ioctl(vd->fd, VIDIOC_S_CTRL, &control_s)) < 0) {
	printf("ioctl tiltupdown control error\n");
	return -1;
    }
    return 0;
}

int v4L2UpDownPanTilt(struct vdIn *vd, short inc_p, short inc_t) {
  int p_control = V4L2_CID_PAN_RELATIVE;
  int t_control = V4L2_CID_TILT_RELATIVE;
  struct v4l2_ext_controls control_s_array;
  struct v4l2_queryctrl queryctrl;
  struct v4l2_ext_control control_s[2];
  int err;

  if(isv4l2Control(vd, p_control, &queryctrl) < 0 ||
     isv4l2Control(vd, t_control, &queryctrl) < 0)
    return -1;
  control_s_array.count = 2;
  control_s_array.ctrl_class = V4L2_CTRL_CLASS_USER;
  control_s_array.reserved[0] = 0;
  control_s_array.reserved[1] = 0;
  control_s_array.controls = control_s;

  control_s[0].id = p_control;
  control_s[0].value = inc_p;
  control_s[1].id = t_control;
  control_s[1].value = inc_t;

  if ((err = ioctl(vd->fd, VIDIOC_S_EXT_CTRLS, &control_s_array)) < 0) {
    printf("ioctl pan-tilt updown control error\n");
    return -1;
  }
  return 0;
}

#if 0

union pantilt {
	struct {
		short pan;
		short tilt;
	} s16;
	int value;
} __attribute__((packed)) ;
	
int v4L2UpDownPan(struct vdIn *vd, short inc)
{   int control = V4L2_CID_PANTILT_RELATIVE;
    struct v4l2_control control_s;
    struct v4l2_queryctrl queryctrl;
    int err;
    
   union pantilt pan;
   
       control_s.id = control;
     if (isv4l2Control(vd, control, &queryctrl) < 0)
        return -1;

  pan.s16.pan = inc;
  pan.s16.tilt = 0;
 
	control_s.value = pan.value ;
    if ((err = ioctl(vd->fd, VIDIOC_S_CTRL, &control_s)) < 0) {
	printf("ioctl pan updown control error\n");
	return -1;
	}
	return 0;
}

int v4L2UpDownTilt(struct vdIn *vd, short inc)
{   int control = V4L2_CID_PANTILT_RELATIVE;
    struct v4l2_control control_s;
    struct v4l2_queryctrl queryctrl;
    int err;
     union pantilt pan;  
       control_s.id = control;
     if (isv4l2Control(vd, control, &queryctrl) < 0)
	return -1;  

    pan.s16.pan= 0;
    pan.s16.tilt = inc;
  
	control_s.value = pan.value;
    if ((err = ioctl(vd->fd, VIDIOC_S_CTRL, &control_s)) < 0) {
	printf("ioctl tiltupdown control error\n");
	return -1;
	}
	return 0;
}
#endif

int v4l2SetLightFrequencyFilter(struct vdIn *vd, int flt) 
{   int control = V4L2_CID_POWER_LINE_FREQUENCY;
    struct v4l2_control control_s;
    struct v4l2_queryctrl queryctrl;
    int err;
       control_s.id = control;
     if (isv4l2Control(vd, control, &queryctrl) < 0)
	return -1;  

       control_s.value = flt;

    if ((err = ioctl(vd->fd, VIDIOC_S_CTRL, &control_s)) < 0) {
	printf("ioctl set_light_frequency_filter error\n");
	return -1;
	}
	return 0;
}
int enum_frame_intervals(int dev, __u32 pixfmt, __u32 width, __u32 height)
{
	int ret;
	struct v4l2_frmivalenum fival;

	memset(&fival, 0, sizeof(fival));
	fival.index = 0;
	fival.pixel_format = pixfmt;
	fival.width = width;
	fival.height = height;
	printf("\tTime interval between frame: ");
	while ((ret = ioctl(dev, VIDIOC_ENUM_FRAMEINTERVALS, &fival)) == 0) {
		if (fival.type == V4L2_FRMIVAL_TYPE_DISCRETE) {
				printf("%u/%u, ",
						fival.discrete.numerator, fival.discrete.denominator);
		} else if (fival.type == V4L2_FRMIVAL_TYPE_CONTINUOUS) {
				printf("{min { %u/%u } .. max { %u/%u } }, ",
						fival.stepwise.min.numerator, fival.stepwise.min.numerator,
						fival.stepwise.max.denominator, fival.stepwise.max.denominator);
				break;
		} else if (fival.type == V4L2_FRMIVAL_TYPE_STEPWISE) {
				printf("{min { %u/%u } .. max { %u/%u } / "
						"stepsize { %u/%u } }, ",
						fival.stepwise.min.numerator, fival.stepwise.min.denominator,
						fival.stepwise.max.numerator, fival.stepwise.max.denominator,
						fival.stepwise.step.numerator, fival.stepwise.step.denominator);
				break;
		}
		fival.index++;
	}
	printf("\n");
	if (ret != 0 && errno != EINVAL) {
		perror("ERROR enumerating frame intervals");
		return errno;
	}

	return 0;
}
int enum_frame_sizes(int dev, __u32 pixfmt)
{
	int ret;
	struct v4l2_frmsizeenum fsize;

	memset(&fsize, 0, sizeof(fsize));
	fsize.index = 0;
	fsize.pixel_format = pixfmt;
	while ((ret = ioctl(dev, VIDIOC_ENUM_FRAMESIZES, &fsize)) == 0) {
		if (fsize.type == V4L2_FRMSIZE_TYPE_DISCRETE) {
			printf("{ discrete: width = %u, height = %u }\n",
					fsize.discrete.width, fsize.discrete.height);
			ret = enum_frame_intervals(dev, pixfmt,
					fsize.discrete.width, fsize.discrete.height);
			if (ret != 0)
				printf("  Unable to enumerate frame sizes.\n");
		} else if (fsize.type == V4L2_FRMSIZE_TYPE_CONTINUOUS) {
			printf("{ continuous: min { width = %u, height = %u } .. "
					"max { width = %u, height = %u } }\n",
					fsize.stepwise.min_width, fsize.stepwise.min_height,
					fsize.stepwise.max_width, fsize.stepwise.max_height);
			printf("  Refusing to enumerate frame intervals.\n");
			break;
		} else if (fsize.type == V4L2_FRMSIZE_TYPE_STEPWISE) {
			printf("{ stepwise: min { width = %u, height = %u } .. "
					"max { width = %u, height = %u } / "
					"stepsize { width = %u, height = %u } }\n",
					fsize.stepwise.min_width, fsize.stepwise.min_height,
					fsize.stepwise.max_width, fsize.stepwise.max_height,
					fsize.stepwise.step_width, fsize.stepwise.step_height);
			printf("  Refusing to enumerate frame intervals.\n");
			break;
		}
		fsize.index++;
	}
	if (ret != 0 && errno != EINVAL) {
		perror("ERROR enumerating frame sizes");
		return errno;
	}

	return 0;
}

int enum_frame_formats(int dev, unsigned int *supported_formats, unsigned int max_formats)
{
	int ret;
	struct v4l2_fmtdesc fmt;

	memset(&fmt, 0, sizeof(fmt));
	fmt.index = 0;
	fmt.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
	while ((ret = ioctl(dev, VIDIOC_ENUM_FMT, &fmt)) == 0) {
		if(supported_formats == NULL) {
			printf("{ pixelformat = '%c%c%c%c', description = '%s' }\n",
					fmt.pixelformat & 0xFF, (fmt.pixelformat >> 8) & 0xFF,
					(fmt.pixelformat >> 16) & 0xFF, (fmt.pixelformat >> 24) & 0xFF,
					fmt.description);
			ret = enum_frame_sizes(dev, fmt.pixelformat);
			if(ret != 0)
				printf("  Unable to enumerate frame sizes.\n");
		}
		else if(fmt.index < max_formats) {
			supported_formats[fmt.index] = fmt.pixelformat;
		}

		fmt.index++;
	}
	if (errno != EINVAL) {
		perror("ERROR enumerating frame formats");
		return errno;
	}

	return 0;
}
