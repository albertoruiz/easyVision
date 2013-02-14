typedef struct {
    /// Arrays for contours data (x coordinates).
    float *x;
    /// Arrays for contours data (y coordinates).
    float *y;
    /// Arrays for contours data (u coordinates).
    float *u;
    /// Arrays for contours data (v coordinates).
    float *v;
    /// Arrays for contours data (b threshold).
    char *b;
    /// Array of sizes for contours data.
    int *cs;
    /// Array of starting indexes for contours data.
    int *ccs;
    /// Total number of points of all contours.
    int pn;
    /// Total number of contours.
    int cn;
} TContours;

