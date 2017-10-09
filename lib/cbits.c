#include "stdio.h"
#include "string.h"

int print(char str[]) {
    for(int i=0;i<strlen(str);i++){
        putchar(str[i]);
    }
    fflush(stdout);
    return 0;
}