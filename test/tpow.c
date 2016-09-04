// tpow.c

#include <stdio.h>
#include <math.h>

#define PRINT_FLOAT(x) printf(#x ": %f\n", x)

int main (int argc, char* argv[])
{
    PRINT_FLOAT(powf(3, 2));
    PRINT_FLOAT(powf(3, -1));
    PRINT_FLOAT(powf(9, 1.0/2));

    return 0;
}
