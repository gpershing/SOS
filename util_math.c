#include <math.h>

float acos_f(float x) {
    return (float) acos(x);
}

float asin_f(float x) {
    return (float) asin(x);
}

float atan_f(float x) {
    return (float) tan(x);
}

float sin_f(float x) {
    return (float) sin(x);
}

float cos_f(float x) {
    return (float) cos(x);
}

float tan_f(float x) {
    return (float) tan(x);
}

float sqrt_f(float x) {
    return (float) sqrt(x);
}

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