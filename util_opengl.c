#include <stdio.h>
#include <stdlib.h>
#include "GL/osmesa.h"

static int Width = 400;
static int Height = 400;

struct point{
    float x;
    float y;
};

struct context_buffer{
    OSMesaContext ctx;
    void * buffer;
};

static void rendering_helper_init(){
   glMatrixMode(GL_PROJECTION);
   glLoadIdentity();
   glMatrixMode(GL_MODELVIEW);
   glClear(GL_COLOR_BUFFER_BIT);
   glPushMatrix();
   glColor4f(1.0, 1.0, 1.0, 1.0); //initalize color as white
}

static void rendering_helper_close(){
    glFinish();
}

/*
 * startRendering: an initalization that must be called before drawing
 * any image. Creates Mesa and OpenGL contexts and image buffer.
 */
static struct context_buffer startRendering(){
    OSMesaContext ctx;
    void *buffer;

   /* Create an RGBA-mode context */
   ctx = OSMesaCreateContextExt( OSMESA_RGBA, 16, 0, 0, NULL );
   if (!ctx) {
      printf("OSMesaCreateContext failed!\n");
   }

   /* Allocate the image buffer */
   buffer = malloc( Width * Height * 4 * sizeof(GLubyte) );
   if (!buffer) {
      printf("Alloc image buffer failed!\n");
   }

   /* Bind the buffer to the context and make it current */
   if (!OSMesaMakeCurrent( ctx, buffer, GL_UNSIGNED_BYTE, Width, Height )) {
      printf("OSMesaMakeCurrent failed!\n");
   }

   struct context_buffer cb = {ctx, buffer};

   rendering_helper_init();

   return cb;
}

/*
 * drawCurve: draws segments between a list of points,
 * meaning n-1 segments for n points
 *
 * points: an array of point structs
 * size_arr: the size of points array 
 */
static void drawCurve(struct point points[], int size_arr){
    glPushMatrix();
    glBegin(GL_LINE_STRIP);
    float x1, y1;
    for (int i = 0; i < size_arr; i++){
        x1 = points[i].x;
        y1 = points[i].y;
        glVertex2f(x1, y1);
    }
    glEnd();
    glPopMatrix();

}

/*
 * drawShape: draws segments between a list of points,
 * including the segment connecting the first and last point
 *
 * points: an array of point structs
 * size_arr: the size of points
 * filed: if filled is 1, the shape will be filled with color
 */
static void drawShape(struct point points[], int size_arr, int filled){
    glPushMatrix();
    if (filled==1){
    glBegin(GL_POLYGON);
    }
    else{
        glBegin(GL_LINE_LOOP);
    }
    float x1, y1; 
    for (int i = 0; i < size_arr; i++){
        x1 = points[i].x;
        y1 = points[i].y;
        glVertex2f(x1, y1);
    }   
    glEnd();
    glPopMatrix();
}

/*
 * drawPoint: draws all points without creating segments
 *
 * points: an array of point structs
 * size_arr: the size of points (length of array)
 * point_size: the size of each point
 */
static void drawPoint(struct point points[], int size_arr, int point_size){
    glPushMatrix();
    rendering_helper_init();
    glPointSize(point_size);
    glBegin(GL_POINTS);
    float x1, y1;
    for (int i = 0; i < size_arr; i++){
        x1 = points[i].x;
        y1 = points[i].y;
        glVertex2f(x1, y1);
    }
    glEnd();
    glPopMatrix();
}



/*
 * write_ppm: saves drawing
 *
 * filename: file name
 * buffer: 
 * width: canvas width
 * height: canvas height
 */
static void write_ppm(const char *filename, const GLubyte *buffer, int width, int height){
   const int binary = 0;
   FILE *f = fopen( filename, "w" );
   if (f) {
      int i, x, y;
      const GLubyte *ptr = buffer;
      if (binary) {
         fprintf(f,"P6\n");
         fprintf(f,"# ppm-file created by osdemo.c\n");
         fprintf(f,"%i %i\n", width,height);
         fprintf(f,"255\n");
         fclose(f);
         f = fopen( filename, "ab" );  /* reopen in binary append mode */
         for (y=height-1; y>=0; y--) {
            for (x=0; x<width; x++) {
               i = (y*width + x) * 4;
               fputc(ptr[i], f);   /* write red */
               fputc(ptr[i+1], f); /* write green */
               fputc(ptr[i+2], f); /* write blue */
            }
         }
      }
      else {
         /*ASCII*/
         int counter = 0;
         fprintf(f,"P3\n");
         fprintf(f,"# ascii ppm file created by osdemo.c\n");
         fprintf(f,"%i %i\n", width, height);
         fprintf(f,"255\n");
         for (y=height-1; y>=0; y--) {
            for (x=0; x<width; x++) {
               i = (y*width + x) * 4;
               fprintf(f, " %3d %3d %3d", ptr[i], ptr[i+1], ptr[i+2]);
               counter++;
               if (counter % 5 == 0)
                  fprintf(f, "\n");
            }
         }
      }
      fclose(f);
   }
}

/*
 * endRendering: closes OpenGL and Mesa contexts and saves drawing
 * by calling write_ppm
 */
static void endRendering(struct context_buffer cb){
    char *filename = "myDrawing";
    rendering_helper_close();
    write_ppm(filename, cb.buffer, Width, Height);
    free(cb.buffer);
    OSMesaDestroyContext(cb.ctx);
}

//sample program
int main(int argc, char *argv[]){
    struct context_buffer cb = startRendering();
    
    struct point p1 = {.25, .25};
    struct point p2 = {.75, 1};
    struct point p3 = {1, 0};
    struct point points[3] = {p1, p2, p3};
    int size_arr = 3;

    glColor4f(1.0, 0.5, 1.0, 1.0);
    drawCurve(points, size_arr);

    glColor4f(1.0, 0.5, 0, 1.0);
    glTranslatef(-.5, -.5, 0);
    drawShape(points, size_arr, 1);
   
    glColor4f(0.5, 1, 1.0, 1.0);
    glTranslatef(1, 1, 0);
    drawShape(points, size_arr, 0);

    endRendering(cb);
    return 0;
}
