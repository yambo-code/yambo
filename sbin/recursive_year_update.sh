#! /bin/sh
#
if [ $# -ne 2 ]; then 
         echo 1>&2 Usage: recursive_year_update.sh check/replace YEAR
         exit 127
fi
#
cat << EOF > check.awk
{
  if ( index(\$0,"Copyright (C)") != 0 && index(\$0,"YAMBO") && index(\$0,"$2") == 0)
  {
   print \$0 >> "temporary"
  }
}
EOF
cat << EOF > normal.awk
{
  if ( index(\$0,"Copyright (C)") != 0 && index(\$0,"YAMBO"))
  {
   gsub ("-$2","-2019")
  }
  print \$0 >> "temporary"
}
EOF
cat << EOF > php.awk
{
  if ( index(\$0,"-$2 Andrea Marini") != 0)
  {
    gsub ("-$2","-2019")
  }
  print \$0 >> "temporary"
}
EOF
#
file_list=`find .  -type f  | grep -v bz2 | grep -E '\.F|\.pm|\.pl|\.php|\.m4|\.h|\.in|\.c' `
for file in $file_list; do
 php=`echo $file| grep -E '\.php' |wc -l`
 if [ ${php} != "0" ]; then
  awk -f php.awk $file
 else
  if [ "$1" = "check" ]; then 
   awk -f check.awk $file 
  else
   awk -f normal.awk $file 
  fi
 fi
 if [ -f temporary ]; then 
  if [ "$1" = "check" ]; then 
   echo -n ${file}:
   cat temporary 
  else
   lines=`diff $file temporary |wc -l`
   if [ ${lines} != "0" ]; then
    echo ${file}
    #diff $file temporary
    mv temporary $file
   fi
  fi
 fi
 rm -f temporary
done
rm -f php.awk normal.awk check.awk
