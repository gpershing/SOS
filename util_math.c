#include <math.h>

double toradians(double x) {
    return x * (M_PI / 180.0);
}

#ifdef BUILD_TEST
int main()
{
  toradians(12.5);
  return 0;
}
#endif