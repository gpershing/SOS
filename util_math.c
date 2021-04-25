#include <math.h>

float toradians_f(float x) {
    return (float) x * (M_PI / 180.0);
} 

#ifdef BUILD_TEST
int main()
{
  toradians_f(12.5);
  return 0;
}
#endif
