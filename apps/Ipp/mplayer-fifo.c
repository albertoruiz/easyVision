// Simple mplayer | mencoder output reading in C.
//
// This is GPL'ed software ...blah blah...
//
// Pedro E. López-de-Teruel Alcolea
// PARP Research Group - University of Murcia - Spain
// Minor modifications by A.Ruiz


#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <stdlib.h>
#include <fcntl.h>
#include <stdio.h>

// Even numbers only!
int ROWS;
int COLS;
int MODE; // 0: RGB, 1: Gray, 2: yuv
int frames;
int fifo2;

int openMPlayer(char*filename, int mode, int rows, int cols)
//int main(int argc, char *argv[])
{
    ROWS=rows;
    COLS=cols;
    MODE=mode;
    int file;
    char str[100]="";
    //unsigned char buf[ROWS*COLS*3];
    

    // First, we delete any possible previously created fifo:
    unlink("/tmp/fifo-mplayer-1");
    unlink("/tmp/fifo-mplayer-2");

    // Fifo creation:
    umask(0000);
    if( (mkfifo ("/tmp/fifo-mplayer-1",0600) != 0) ||
        (mkfifo ("/tmp/fifo-mplayer-2",0600) != 0) ) {
        printf("Error creating fifos\n");
        exit(1);
    }

    // mplayer and mencoder execution. 
    if (fork() == 0) { // We create a new process...
      // ... close its stdin, stdout & stderr ...
      file = open("/dev/null",O_RDWR);
      close(0); dup(file);
      close(1); dup(file);
      close(2); dup(file);
      // ... and exec the mplayer command.

      
        sprintf(str,"scale=%d:%d",COLS,ROWS);
        // Of course, we can play with parameters as needed. Here, we just set an arbitrary size,
        // but we could deinterlace, or perform any other filter, for example.
        execlp("mplayer","mplayer",filename,"-vo","yuv4mpeg:file=/tmp/fifo-mplayer-1","-vf",
                str,"-ao","null","-slave","-loop","0",
                "-tv","driver=v4l:width=640:height=480",
                NULL);

      printf("Error executing mplayer\n");
      exit(1);
    }

    if (fork() == 0) { // Same procedure to exec mencoder:
      file = open("/dev/null",O_RDWR);
      close(0); dup(file);
      close(1); dup(file);
      close(2); dup(file);

      char* format0 = "format=rgb24"; // RGB
      char* format1 = "format=y8";    // Gray
      char* format2 = "format=rgb24"; // YUV

      char* format;
            if(MODE==0) {       // RGB
              format = format0;
          } else if (MODE==1){  // Gray
              format = format1;
          } else {
              format = format2; // YUV
          }

      execlp("mencoder","mencoder","/tmp/fifo-mplayer-1","-nosound","-o","/tmp/fifo-mplayer-2",
             "-ovc","raw","-of","rawvideo","-vf",format,NULL);
      printf("Error executing mencoder\n");
      exit(1);
    }

    if( (fifo2 = open("/tmp/fifo-mplayer-2",O_RDONLY)) == -1) {
          printf("Error opening fifo for reading\n");
          exit(1);
    }
    frames = 0;

    return 1; // camera identifier
}

int getFrame(int camera, unsigned char * buf)
{
    int total;

    if(MODE==0) { // RGB
        total = ROWS*COLS*3;
    } else {     // Gray
        total = ROWS*COLS;
    }

    int nbytes,totbytes;
    // Loop to read a frame (because fifos sometimes do not return all the requested bytes
    // in a single read):
    totbytes = 0;
    while(totbytes != total) {
        nbytes = read(fifo2,buf+totbytes,total-totbytes);
        //printf("  partial read = %d\n",nbytes);
        if(nbytes == 0) {
            break;
        }
        totbytes += nbytes;
    }
    if(nbytes == 0) { // The other extreme has been closed; it is the end.
        close(fifo2);
        return 0;
    }
    return 1;
}


int sendCommand(int camera, char* command) {
    printf(command);
    printf("\n");
    return 0;
}