#include "stdio.h"

double putchard(double x) {
    putchar((char)x);
    fflush(stdout);
    return 0;
}


double printd(double X) {
  printf("%f\n", X);
  return 0;
}