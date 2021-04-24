#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "GL/osmesa.h"

static int Width = 400;
static int Height = 400;

struct point{
    float x;
    float y;
};

OSMesaContext ctx;
void *buffer;

static void rendering_helper_init(){
   glMatrixMode(GL_PROJECTION);
   glLoadIdentity();
   glMatrixMode(GL_MODELVIEW);
   glClear(GL_COLOR_BUFFER_BIT);
   glPushMatrix();
   glEnableClientState(GL_VERTEX_ARRAY);
   glEnableClientState(GL_COLOR_ARRAY);
   glColor4f(1.0, 1.0, 1.0, 1.0); //initalize color as white
}

static void rendering_helper_close(){
    glFinish();
}

/*
 * startRendering: an initalization that must be called before drawing
 * any image. Creates Mesa and OpenGL contexts and image buffer.
 */
static void startRendering(){
    ctx = OSMesaCreateContextExt(OSMESA_RGBA, 16, 0, 0, NULL);
    if (!ctx){
        printf("OSMesaCreateContext failed!\n");
    }

    buffer = malloc( Width * Height * 4 * sizeof(GLubyte) );
    if (!buffer) {
        printf("Alloc image buffer failed!\n");
    }

    // Bind the buffer to the context and make it current
    if (!OSMesaMakeCurrent(ctx, buffer, GL_UNSIGNED_BYTE, Width, Height)) {
        printf("OSMesaMakeCurrent failed!\n");
    }
    
    rendering_helper_init();
}

/*
 * drawCurve: draws segments between a list of points,
 * meaning n-1 segments for n points
 *
 * points: an array of points, with a point (x,y) located at [2i, 2i+1]
 * colors: an array of colors, with a the RGBA values of a point located at [4i, 4i+1, 4i+2, 4i+3]
 * size_arr: the number of points
 * color_mode: 0 -> between points i and i+1, the color of the segment is the color of point i+1
 *             1 -> each point has its own color. The segment between each point is a gradient between point colors
 */
static void drawCurve(float points[], float colors[], int size_arr, int color_mode){
    glPushMatrix();
    
    if (color_mode == 0){
        glShadeModel(GL_FLAT);
    }
    else{
        glShadeModel(GL_SMOOTH);
    }
    glVertexPointer(2, GL_FLOAT, 0, points);
    glColorPointer(4, GL_FLOAT, 0, colors);
    glDrawArrays(GL_LINE_STRIP, 0, size_arr);
    
    glPopMatrix();
}

/*
 * drawShape: draws segments between a list of points,
 * including the segment connecting the first and last point
 *
 * points: an array of points, with a point (x,y) located at [2i, 2i+1]
 * colors: an array of colors, with a the RGBA values of a point located at [4i, 4i+1, 4i+2, 4i+3]
 * size_arr: the number of points
 * color_mode: 0 -> between points i and i+1, the color of the segment is the color of point i+1
 *             1 -> each point has its own color. The segment between each point is a gradient between point colors
 * filed: 0 -> shape is not filled with color
 *        1 -> shape will be filled with color
 */
static void drawShape(float  points[], float colors[], int size_arr, int color_mode, int filled){
    glPushMatrix();

    if (color_mode == 0){
        glShadeModel(GL_FLAT);
    }
    else{
        glShadeModel(GL_SMOOTH);
    }

    glVertexPointer(2, GL_FLOAT, 0, points);
    glColorPointer(3, GL_FLOAT, 0, colors);

    if (filled==1){
        glDrawArrays(GL_POLYGON, 0, size_arr);
    }
    else{
        glDrawArrays(GL_LINE_LOOP, 0, size_arr);
    }

    glPopMatrix();
}

/*
 * drawPoint: draws all points without creating segments
 *
 * points: an array of points, with a point (x,y) located at [2i, 2i+1]
 * colors: an array of colors, with a the RGBA values of a point located at [4i, 4i+1, 4i+2, 4i+3]
 * size_arr: the number of points
 * point_size: the size of each point
 */
static void drawPoint(float points[], float colors[], int size_arr, int point_size){
    glPushMatrix();
    glVertexPointer(2, GL_FLOAT, 0, points);
    glColorPointer(3, GL_FLOAT, 0, colors);
    glPointSize(point_size);
    glDrawArrays(GL_POINTS, 0, size_arr);
    glPopMatrix();
}

static void clearCanvas(){
    glMatrixMode(GL_MODELVIEW);
    glClear(GL_COLOR_BUFFER_BIT);
}

/*
 * write_ppm: saves drawing
 *
 * filename: file name
 * buffer: 
 * width: canvas width
 * height: canvas height
 */
static void write_ppm(int fileNumber, const GLubyte *buffer, int width, int height){
    //int length = snprintf(NULL, 0, "%d", fileNumber);
    //char* numname = malloc(length + 1);
    //snprintf(numname, length + 1, "%d", fileNumber);
    //itoa(fileNumber, result, 10);
    char b[50];
    char b2[50];
    sprintf(b2, "%d", fileNumber);
    strcpy(b, "pic");
    strcat(b, b2);
    const int binary = 0;
    FILE *f = fopen( b, "w" );
    if (f) {
       int i, x, y;
       const GLubyte *ptr = buffer;
       if (binary) {
          fprintf(f,"P6\n");
          fprintf(f,"# ppm-file created by osdemo.c\n");
          fprintf(f,"%i %i\n", width,height);
          fprintf(f,"255\n");
          fclose(f);
          f = fopen( b, "ab" );  /* reopen in binary append mode */
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
static void endRendering(int fileNumber){
    rendering_helper_close();
    write_ppm(fileNumber, buffer, Width, Height);
    free(buffer);
    OSMesaDestroyContext(ctx);
}

//sample program
//#ifdef BUILD_TEST
int main(int argc, char *argv[]){
    startRendering();
    
    float points[] = {-.5, 0, .5, 0, 0, .5};
    float colors[] = {1.0, 0.5, 1.0, 1.0, 1.0, 0.5, 0, 1.0,  0.5, 1.0, 1.0, 1.0};
    int size_arr = 3;

    int fileNumber = 1;

    startRendering();

    drawCurve(points, colors, size_arr, 1);
    
    glTranslatef(-.2,-.2,0);
    drawCurve(points, colors, size_arr, 0);

    glTranslatef(-.2, -.2, 0);
    drawShape(points, colors, size_arr, 1, 1);

    glTranslatef(.6, 0 , 0);
    drawShape(points, colors, size_arr, 0, 0);

    glTranslatef(-.3, -.2, 0);
    drawPoint(points, colors, size_arr, 5);

    endRendering(0);

    return 0;
}
//#endif
