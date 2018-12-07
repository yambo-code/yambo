#include <stdio.h>
#include "../lib/add.h"
#include "../lib/answer.h"

int main(int argc, char* argv[]) {

  setSummand(5);

  printf("5 + 7 = %d\n", add(7));

  printf("And the answer is: %d\n", answer());

  return 0;
}
