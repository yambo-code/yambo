#include <stdio.h>

int gSummand;


void setSummand(int summand) {
  gSummand = summand;
}

int add(int summand) {
  return gSummand + summand;
}

void __attribute__ ((constructor)) initLibrary(void) {
 //
 // Function that is called when the library is loaded
 //
    printf("Library is initialized\n"); 
    gSummand = 0;
}
void __attribute__ ((destructor)) cleanUpLibrary(void) {
 //
 // Function that is called when the library is »closed«.
 //
    printf("Library is exited\n"); 
}
