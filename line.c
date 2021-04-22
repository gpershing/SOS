#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "GL/osmesa.h"
#include "GL/gl.h"
#include "GL/glu.h"


#define SAVE_TARGA

static int Width = 400;
static int Height = 400;

struct point{
    float x;
    float y;
};
/*
static void Line(struct point points[], size_t size_arr){
    printf("%ld", size_arr);
    glBegin(GL_LINES);
    int x1, y1, x2, y2;
    for (int i = 0; i < size_arr; i++){
        x1 = points[i].x;
        y1 = points[i].y;
        //x2 = points[i+1].x;
        //x2 = points[i+1].y;
        glVertex2f(x1, y1);
        //glVertex2f(x2, y2);
    }
    glEnd();
 }
*/

static void SimpleLine(){
    glBegin(GL_LINES);
    glVertex2f(.25, .25);
    glVertex2f(.75, .75);
    glEnd();
 }
static void render_image(){
//static void render_image(struct point points[], size_t size_arr){
   GLfloat light_ambient[] = { 0.0, 0.0, 0.0, 1.0 };
   GLfloat light_diffuse[] = { 1.0, 1.0, 1.0, 1.0 };
   GLfloat light_specular[] = { 1.0, 1.0, 1.0, 1.0 };
   GLfloat light_position[] = { 1.0, 1.0, 1.0, 0.0 };
   GLfloat red_mat[]   = { 1.0, 0.2, 0.2, 1.0 };
   GLfloat green_mat[] = { 0.2, 1.0, 0.2, 1.0 };
   GLfloat blue_mat[]  = { 0.2, 0.2, 1.0, 1.0 };


   glLightfv(GL_LIGHT0, GL_AMBIENT, light_ambient);
   glLightfv(GL_LIGHT0, GL_DIFFUSE, light_diffuse);
   glLightfv(GL_LIGHT0, GL_SPECULAR, light_specular);
   glLightfv(GL_LIGHT0, GL_POSITION, light_position);

   glEnable(GL_LIGHTING);
   glEnable(GL_LIGHT0);
   glEnable(GL_DEPTH_TEST);

   glMatrixMode(GL_PROJECTION);
   glLoadIdentity();
   glOrtho(-2.5, 2.5, -2.5, 2.5, -10.0, 10.0);
   glMatrixMode(GL_MODELVIEW);

   glClear( GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT );

   glPushMatrix();
   glRotatef(20.0, 1.0, 0.0, 0.0);
/*
   glPushMatrix();
   glTranslatef(-0.75, 0.5, 0.0);
   glRotatef(90.0, 1.0, 0.0, 0.0);
   glMaterialfv( GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE, red_mat );
   Line(points, size_arr);
   glPopMatrix();
*/
   glPushMatrix();
   glTranslatef(-0.75, 0.5, 0.0);
   glRotatef(90.0, 1.0, 0.0, 0.0);
   glMaterialfv( GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE, red_mat );
   SimpleLine();
   glPopMatrix();

   glPopMatrix();

   /* This is very important!!!
    * Make sure buffered commands are finished!!!
    */
   glFinish();
}

#ifdef SAVE_TARGA

static void write_targa(const char *filename, const GLubyte *buffer, int width, int height){
   FILE *f = fopen( filename, "w" );
   if (f) {
      int i, x, y;
      const GLubyte *ptr = buffer;
      printf ("osdemo, writing tga file \n");
      fputc (0x00, f);  /* ID Length, 0 => No ID        */
      fputc (0x00, f);  /* Color Map Type, 0 => No color map included   */
      fputc (0x02, f);  /* Image Type, 2 => Uncompressed, True-color Image */
      fputc (0x00, f);  /* Next five bytes are about the color map entries */
      fputc (0x00, f);  /* 2 bytes Index, 2 bytes length, 1 byte size */
      fputc (0x00, f);
      fputc (0x00, f);
      fputc (0x00, f);
      fputc (0x00, f);  /* X-origin of Image    */
      fputc (0x00, f);
      fputc (0x00, f);  /* Y-origin of Image    */
      fputc (0x00, f);
      fputc (Width & 0xff, f);      /* Image Width      */
      fputc ((Width>>8) & 0xff, f);
      fputc (Height & 0xff, f);     /* Image Height     */
      fputc ((Height>>8) & 0xff, f);
      fputc (0x18, f);          /* Pixel Depth, 0x18 => 24 Bits */
      fputc (0x20, f);          /* Image Descriptor     */
      fclose(f);
      f = fopen( filename, "ab" );  /* reopen in binary append mode */
      for (y=height-1; y>=0; y--) {
         for (x=0; x<width; x++) {
            i = (y*width + x) * 4;
            fputc(ptr[i+2], f); /* write blue */
            fputc(ptr[i+1], f); /* write green */
            fputc(ptr[i], f);   /* write red */
         }
      }
   }
}

#else

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

#endif

int main(int argc, char *argv[]){
   OSMesaContext ctx;
   void *buffer;
   char *filename = NULL;

   if (argc < 2) {
      fprintf(stderr, "Usage:\n");
      fprintf(stderr, "[target_file_name] [canvas_width] [canvas_height]\n");
      return 0;
   }

   filename = argv[1];
   if (argc == 4) {
      Width = atoi(argv[2]);
      Height = atoi(argv[3]);
   }

   /* Create an RGBA-mode context */
#if OSMESA_MAJOR_VERSION * 100 + OSMESA_MINOR_VERSION >= 305
   /* specify Z, stencil, accum sizes */
   ctx = OSMesaCreateContextExt( OSMESA_RGBA, 16, 0, 0, NULL );
#else
   ctx = OSMesaCreateContext( OSMESA_RGBA, NULL );
#endif
   if (!ctx) {
      printf("OSMesaCreateContext failed!\n");
      return 0;
   }

   /* Allocate the image buffer */
   buffer = malloc( Width * Height * 4 * sizeof(GLubyte) );
   if (!buffer) {
      printf("Alloc image buffer failed!\n");
      return 0;
   }

   /* Bind the buffer to the context and make it current */
   if (!OSMesaMakeCurrent( ctx, buffer, GL_UNSIGNED_BYTE, Width, Height )) {
      printf("OSMesaMakeCurrent failed!\n");
      return 0;
   }

   {
      int z, s, a;
      glGetIntegerv(GL_DEPTH_BITS, &z);
      glGetIntegerv(GL_STENCIL_BITS, &s);
      glGetIntegerv(GL_ACCUM_RED_BITS, &a);
      printf("Depth=%d Stencil=%d Accum=%d\n", z, s, a);
   }

   struct point p1 = {.25, .25};
   struct point p2 = {.75, .75};
   struct point p3 = {1, 0};
   struct point points[2] = {p1, p2};
   int size = sizeof(points)/sizeof(struct point);
   
   render_image();
   //render_image(points, size);

   if (filename != NULL) {
#ifdef SAVE_TARGA
      write_targa(filename, buffer, Width, Height);
#else
      write_ppm(filename, buffer, Width, Height);
#endif
   }
   else {
      printf("Specify a filename if you want to make an image file\n");
   }

   printf("all done\n");

   /* free the image buffer */
   free( buffer );

   /* destroy the context */
   OSMesaDestroyContext( ctx );

   return 0;
}
