#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <getopt.h>

/** Program to calculate the area and perimeter of 
 * a rectangle using command line arguments
  https://linuxprograms.wordpress.com/2012/06/22/c-getopt_long-example-accessing-command-line-arguments/
 */
void print_usage() {
    printf("Usage: rectangle [ap] -l num -b num\n");
}

int main(int argc, char *argv[]) {
    int opt= 0;
    int area = -1, perimeter = -1, breadth = -1, length =-1;

    //Specifying the expected options
    //The two options l and b expect numbers as argument
    static struct option long_options[] = {
        {"area",      no_argument,       0,  'a' },
        {"perimeter", no_argument,       0,  'p' },
        {"length",    required_argument, 0,  'l' },
        {"breadth",   required_argument, 0,  'b' },
        {0,           0,                 0,  0   }
    };

    int long_index =0;
    while ((opt = getopt_long(argc, argv,"apl:b:", 
                   long_options, &long_index )) != -1) {
        switch (opt) {
             case 'a' : area = 0;
                 break;
             case 'p' : perimeter = 0;
                 break;
             case 'l' : length = atoi(optarg); 
                 break;
             case 'b' : breadth = atoi(optarg);
                 break;
             default: print_usage(); 
                 exit(EXIT_FAILURE);
        }
    }
    if (length == -1 || breadth ==-1) {
        print_usage();
        exit(EXIT_FAILURE);
    }

    // Calculate the area
    if (area == 0) {
        area = length * breadth;
        printf("Area: %d\n",area);
    }

    // Calculate the perimeter
    if (perimeter == 0) {
        perimeter = 2 * (length + breadth);
        printf("Perimeter: %d\n",perimeter);
    }
    return 0;
}
