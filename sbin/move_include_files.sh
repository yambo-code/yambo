#!/bin/bash

for file in */*/*.F; do

 if [ "$file" == "interfaces/p2y/num_interpolation_module.F" ]; then continue; fi
 if [ "$file" == "interfaces/p2y/pw_pseudo_module.F" ]; then continue; fi
 if [ "$file" == "src/Ymodules/mod_cufft.F" ]; then continue; fi
 if [ "$file" == "src/Ymodules/mod_cusolverdn_y.F" ]; then continue; fi
 if [ "$file" == "src/Ymodules/mod_interfaces.F" ]; then continue; fi
 if [ "$file" == "src/bse/K_diago_driver.F" ]; then continue; fi
 if [[ "$file" == *"_incl.F"* ]]; then continue; fi

 echo "Checking file $file"

awk '
{
    lines[NR] = $0;  # Store each line in an array
}
END {
    for (i = 1; i <= NR; i++) {
        if (lines[i] ~ /! Authors/) {
            print lines[i];  # Print the line with ! Authors
	    k = 0;
            for (j = 1; j <= NR; j++) {
                if (lines[j] ~ /#include/) {
		    k++ ;
		    if (k == 1) {
		      print "!";
		      print "! headers";
		      print "!";
                      print lines[j];  # Print #include lines
		      line_printed[k++]=lines[j]
		    } else {
		      print_line = 1;
		      for (kp = 1; kp < k; kp++) {
		      if ( lines[j] == line_printed[kp] ){ print_line = 0 }
		      }
		      if (print_line == 1){
			print lines[j]
		        line_printed[k++]=lines[j]
  		      }
		    }
                }
            }
        }
        if (lines[i] !~ /! Authors/ && lines[i] !~ /#include/) {
            print lines[i];  # Print other lines
        }
        if (lines[i] ~ /#include<y_memory.h>/ || lines[i] ~ /#include <y_memory.h>/) {
            print " USE_MEMORY"   ;
	    print " !";
            print " implicit none";  # Resume implicit none
        }
    }
}
' $file > $file.tmp
mv $file.tmp $file
done

