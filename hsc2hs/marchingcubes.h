/* file marchingcubes.h */

typedef struct {
   double x,y,z;
} XYZ_T;

typedef struct {
   XYZ_T p[3];
} Triangle_T;

typedef struct {
   XYZ_T p[8];
   double val[8];
} GridCell_T;
