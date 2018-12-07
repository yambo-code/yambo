#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <getopt.h>

/** Program to calculate the area and perimeter of 
 * a rectangle using command line arguments

 https://linuxprograms.wordpress.com/2012/06/22/c-getopt_long_only-example-accessing-command-line-arguments/
 
 ASCII table

 https://www.cs.cmu.edu/~pattis/15-1XX/common/handouts/ascii.html

 http://www.mathcs.emory.edu/~cheung/Courses/561/Syllabus/2-C/subroutine.html

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
        {"area",      no_argument,       0,  'A' },
        {"perimeter", no_argument,       0,  'B' },
        {"length",    required_argument, 0,  'C' },
        {"breadth",   required_argument, 0,  'D' },
        {0,           0,                 0,  0   }
    };

    int long_index =0;
    while ((opt = getopt_long_only(argc, argv,"", 
                   long_options, &long_index )) != -1) {
        switch (opt) {
             case 'A' : area = 0; printf (" %i ",opt);
                 break;
             case 'B' : perimeter = 0;
                 break;
             case 'C' : length = atoi(optarg); 
                 break;
             case 'D' : breadth = atoi(optarg);
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
