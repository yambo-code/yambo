#! /bin/sh
#
#        Copyright (C) 2000-2022 the YAMBO team
#              http://www.yambo-code.org
#
# Authors (see AUTHORS file for details): AM
#
# This file is distributed under the terms of the GNU
# General Public License. You can redistribute it and/or
# modify it under the terms of the GNU General Public
# License as published by the Free Software Foundation;
# either version 2, or (at your option) any later version.
#
# This program is distributed in the hope that it will
# be useful, but WITHOUT ANY WARRANTY; without even the
# implied warranty of MERCHANTABILITY or FITNESS FOR A
# PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.
#
# You should have received a copy of the GNU General Public
# License along with this program; if not, write to the Free
# Software Foundation, Inc., 59 Temple Place - Suite 330,Boston,
# MA 02111-1307, USA or visit http://www.gnu.org/copyleft/gpl.txt.
#
WY=`date +%Y`
#
if [ $# -ne 2 ]; then 
         echo 1>&2 Usage: recursive_year_update.sh check/replace YEAR
         echo 1>&2
         echo 1>&2 Current year is is $WY
         exit 127
fi
#
cat << EOF > check.awk
{
  if ( index(\$0,"Copyright (C)") != 0 && index(\$0,"YAMBO") && index(\$0,"$2") != 0)
  {
   print \$0 >> "temporary"
  }
}
EOF
cat << EOF > normal.awk
{
  if ( index(\$0,"Copyright (C)") != 0 && index(\$0,"YAMBO"))
  {
   gsub ("-$2","-$WY")
  }
  print \$0 >> "temporary"
}
EOF
cat << EOF > php.awk
{
  if ( index(\$0,"-$2 Andrea Marini") != 0)
  {
    gsub ("-$2","-$WY")
  }
  print \$0 >> "temporary"
}
EOF
#
file_list=`find .  -type f  | grep -v bz2 | grep -E '\.F|\.pm|\.pl|\.php|\.m4|\.h|\.in|\.c|\.sh' | grep -v '\.swp' | grep -v 'gsl_complex_math.h' | grep -v 'PARSER_math.c' `
for file in $file_list; do
 php=`echo $file| grep -E '\.php' |wc -l`
 sh=`echo $file| grep -E '\.sh' |wc -l`
 pl=`echo $file| grep -E '\.pl' |wc -l`
 if [ ${php} != "0" ]; then
  awk -f php.awk $file
  if [ ${sh} != "0" ]; then chmod +x $file; fi
  if [ ${pl} != "0" ]; then chmod +x $file; fi
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
