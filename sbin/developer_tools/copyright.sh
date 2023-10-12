#!/bin/bash
#
# License-Identifier: GPL
#
# Copyright (C) 2023 The Yambo Team
#
# Authors (see AUTHORS file for details): AM

if (( $# == 0 )); then
 >&2 echo " copyright.sh -c(check only) -y *(year only) -f (full header)"
 exit
fi
CHECK="no"
YEAR="no"
FULL="no"
# Get the options
while getopts "cyf" option; do
   case $option in
      c) CHECK="yes";;
      y) YEAR="yes";;
      f) FULL="yes";;
   esac
done
#
cat << EOF > base.awk
{
 if (FNR ==1) {
  HEAD=0
  AUT=0
 }
 if ( index(\$0,"Copyright (C)") != 0 && index(\$0,"YAMBO") !=0)  {
  HEAD=1
  NA=split(\$0,a," ")
  NB=split(a[COL],b,"-")
  YEAR=b[1]
  getline
 }
 if ( index(\$0,"Authors") != 0 && index(\$0,"details") !=0) { 
  AUT=1
  gsub(")"," ",\$0)
  NA=split(\$0,a,":")
  AUTHORS=a[2]
  gsub(",","",AUTHORS)
  getline
 }
 if ( index(\$0,"www.gnu.org") != 0 && index(\$0,"gpl.txt") !=0) {
  HEAD=0
  print "STRING License-Identifier: GPL"
  print "STRING"
  print "STRING Copyright (C) "YEAR" The Yambo Team"
  print "STRING"
  print "STRING Authors (see AUTHORS file for details):" AUTHORS
  getline
 }
 if (HEAD==1) {next}
 #
 print \$0 
}
EOF
#
# STRING:
#  # .pm .pl .m4 .sh .mk => COL=4
#  ! .F => COL=4
#    .c .h => COL=3
#
file_list=`find . -type f  | grep -v bz2 | grep -E '\.F|\.pm|\.pl|\.m4|\.h|\.c|\.sh|\.mk' | grep -v '\.swp' | grep -v 'gsl_complex_math.h' | grep -v 'PARSER_math.c' | grep -v 'copyright.sh' `
#
for file in $file_list; do
 NL=`grep Copyr $file | grep YAMBO | wc -l`
 if [ "$YEAR" == "yes" ];  then NL=`grep Copyr $file | grep Yambo | wc -l`; fi
 if [ $NL -gt 0 ]; then 
  isc=`echo $file| grep -E '\.c' |wc -l`
  ish=`echo $file| grep -E '\.h' |wc -l`
  isF=`echo $file| grep -E '\.F' |wc -l`
  ispl=`echo $file| grep -E '\.pl' |wc -l`
  issh=`echo $file| grep -E '\.sh' |wc -l`
  if [ ${isc} != "0" ] && [ "$CHECK" == "yes" ]; then echo "$file is .c"; fi
  if [ ${ish} != "0" ] && [ "$CHECK" == "yes" ]; then echo "$file is .h"; fi
  if [ ${isF} != "0" ] && [ "$CHECK" == "yes" ]; then echo "$file is .F"; fi
  if [ ${issh} != "0" ] && [ "$CHECK" == "yes" ]; then echo "$file is .sh"; fi
  if [ ${ispl} != "0" ] && [ "$CHECK" == "yes" ]; then echo "$file is .pl"; fi
  if [ "$CHECK" == "yes" ]; then continue; fi
  #
  if [ "$YEAR" == "yes" ]; then 
   #=====================
   DATE=`git log --follow --pretty=format:%ad --date=short --date=format:'%Y' "$file" |tail -n 1`
   cat $file | sed "s/Copyright (C) 2000/Copyright (C) $DATE/g" > temporary
   echo $file $DATE >> ../copyright.log
  fi
  #
  if [ "$FULL" == "yes" ]; then 
   #=====================
   if [ ${isc} != "0" ]; then
    cat base.awk | sed 's/COL/3/g' | sed 's/STRING/ /g' > CP.awk
   elif [ ${ish} != "0" ]; then
    cat base.awk | sed 's/COL/3/g' | sed 's/STRING/ /g' > CP.awk
   elif [ ${isF} != "0" ]; then
    cat base.awk | sed 's/COL/4/g' | sed 's/STRING/!/g' > CP.awk
   else
    cat base.awk | sed 's/COL/4/g' | sed 's/STRING/#/g' > CP.awk
   fi
   awk -f CP.awk $file > temporary
  fi
  #
  if [ -f temporary ]; then 
   lines=`diff $file temporary |wc -l`
   if [ ${lines} != "0" ]; then
    echo ${file}
    mv temporary $file
    if [ ${issh} != "0" ]; then chmod +x $file; fi
    if [ ${ispl} != "0" ]; then chmod +x $file; fi
   fi
  fi
  rm -f temporary CP.awk
 fi
done
rm -f base.awk
