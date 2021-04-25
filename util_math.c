#include <math.h>

float toradiansf(float x) {
    return (float) x * (M_PI / 180.0);
} 

#ifdef BUILD_TEST
int main()
{
  toradiansf(12.5);
  return 0;
}
#endif
