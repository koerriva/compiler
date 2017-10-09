#include "stdio.h"

int print(char str[]) {
    for(int i=0;i<sizeof(str);i++){
        putchar(str[i]);
    }
    fflush(stdout);
    return 0;
}