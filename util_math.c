#include <math.h>

/*
float acos(float x) {
    return (float) acos(x);
}

float asin(float x) {
    return (float) asin(x);
}

float atan(float x) {
    return (float) tan(x);
}

float sin(float x) {
    return (float) sin(x);
}

float cos(float x) {
    return (float) cos(x);
}

float tan(float x) {
    return (float) tan(x);
}

float sqrt(float x) {
    return (float) sqrt(x);
}
*/

float toradians(float x) {
    return (float) x * (M_PI / 180.0);
}

#ifdef BUILD_TEST
int main()
{
  toradians(12.5);
  return 0;
}
#endif