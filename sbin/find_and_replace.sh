#! /bin/tcsh
#
#        Copyright (C) 2000-2020 the YAMBO team
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
if ( $#argv == 0 ) goto help

set find_pat = $1
set replace_pat = $2

set DOIT = "no"
set CALL_pat
set GREP_pat
set RM_pat
if ( $#argv > 2 ) then
 if ( "$argv[3]" == "-c" ) then
   set CALL_pat = "$argv[4]"
 endif
 if ( "$argv[3]" == "-d" ) then
   set DOIT = "$argv[4]"
 endif
 if ( "$argv[3]" == "-g" ) then
   set GREP_pat = "$argv[4]"
 endif
 if ( "$argv[3]" == "-r" ) then
   set RM_pat = "$argv[4]"
 endif
endif
if ( $#argv > 4 ) then
 if ( "$argv[5]" == "-c" ) then
   set CALL_pat = "$argv[6]"
 endif
 if ( "$argv[5]" == "-d" ) then
   set DOIT = "$argv[6]"
 endif
 if ( "$argv[5]" == "-g" ) then
   set GREP_pat = "$argv[6]"
 endif
 if ( "$argv[3]" == "-r" ) then
   set RM_pat = "$argv[4]"
 endif
endif
if ( $#argv > 6 ) then
 if ( "$argv[7]" == "-c" ) then
   set CALL_pat = "$argv[8]"
 endif
 if ( "$argv[7]" == "-d" ) then
   set DOIT = "$argv[8]"
 endif
 if ( "$argv[7]" == "-g" ) then
   set GREP_pat = "$argv[8]"
 endif
 if ( "$argv[7]" == "-r" ) then
   set RM_pat = "$argv[8]"
 endif
endif
if ( $#argv > 8 ) then
 if ( "$argv[9]" == "-c" ) then
   set CALL_pat = "$argv[10]"
 endif
 if ( "$argv[9]" == "-d" ) then
   set DOIT = "$argv[10]"
 endif
 if ( "$argv[9]" == "-g" ) then
   set GREP_pat = "$argv[10]"
 endif
 if ( "$argv[9]" == "-r" ) then
   set RM_pat = "$argv[10]"
 endif
endif

set file_list = `find . -name '*.F' -o -name '*.pl' -o -name '*.pm' -o -name '*.c' -o -name '*.h' -o -name '.objects'`

unalias cp
unalias mv
foreach file  ($file_list)

cat << EOF > find_and_replace.awk
{
 find_pat="$find_pat"
 replace_pat="$replace_pat"
 call_pat = "$CALL_pat"
 grep_pat = "$GREP_pat"
 rm_pat = "$RM_pat"
 doit = "$DOIT"
 file_out = "${file}_new"
 line=\$0
 if (NR==1) { report_fname = "yes" }
 find_task="no"

 split(line,line_array)
 n_line=asort(line_array,line_scratch)
 gsub("#"," ",replace_pat)

 loop=1
 if (index ( line , find_pat ) == 0 || index (line_array[1] , "!") > 0 ){ loop =0 }
 if ( grep_pat != "" && index ( line , grep_pat ) == 0){loop=0}
 if ( loop == 0 && doit == "yes" )
 {
  print line > file_out 
  next
 }

 loop=0
 if ( call_pat != "" && index ( line , call_pat ) > 0 ) {loop=1}
 if ( loop == 1 )
 { 
  j=1 
  n_blanks=match(line,line_array[1])
  new_line=""
  while ( j <= n_blanks-2 ){ new_line= new_line " " ; j=j+1} 
  j=1
  while ( j <= n_line )
  {
   piece=line_array[j]
   gsub("\\\("," ",piece)
   gsub("\\\)"," ",piece)
   gsub(","," ",piece)
   gsub(";"," ",piece)
   gsub(":"," ",piece)
   gsub("="," ",piece)
   gsub("%"," ",piece)
   gsub(">"," ",piece)
   gsub("<"," ",piece)
   split(piece,piece_array)
   n_pieces=asort(piece_array,piece_scratch)
   k=1
   while ( k <= n_pieces )
   {
    if ( piece_array[k] == find_pat && line_array[j-1] == call_pat ) {find_task="yes"}
    if ( piece_array[k] == find_pat && piece_array[k-1] == call_pat ) {find_task="yes"}
    k=k+1
   }
   if (find_task == "yes" ) 
   { 
    sub(find_pat,replace_pat,line_array[j]) }
    new_line= new_line " " line_array[j] 
    j=j+1
   }
 }
 
 if ( loop == 0  )
 {
   find_task = "yes"
   new_line=line
   gsub(find_pat,replace_pat,new_line) 
   if ( grep_pat != "" && index ( line , grep_pat ) == 0 ) { new_line=line }
   if ( line == new_line ) { find_task = no}
 }

 print_line = "yes"
 if ( rm_pat != "" && index ( line , rm_pat ) > 0 && index (line_array[1] , "!") ==0 )
 {
  print_line=no
  find_task="yes"
  if ( grep_pat != "" && index ( line , grep_pat ) == 0 )
  {
   print_line=yes
   find_task=no
  }
 }
 
 if (line_scratch[1] == "!")  find_task="no"

 if (find_task == "yes")
 {
  if ( report_fname == "yes" )
  {
    print "### Acting on " FILENAME  " ###"
    report_fname = "no"
  }
  print " Line " FNR
  print " OLD " line 
  if (print_line == "yes") print " NEW " new_line 
  line=new_line
 }
 if (doit == "yes" && print_line == "yes" ) { print line > file_out }
}
EOF
 awk -f ./find_and_replace.awk {$file} 
 if ( $DOIT == "yes" ) then
  set is_different = `diff ${file}_new $file | wc -l`
  if ( $is_different != 0 ) then
    mv ${file}_new $file
  else
    rm -f ${file}_new
  endif
 endif
end
rm -f ./find_and_replace.awk 
exit 0

help:
 echo $0 "find_pat replace_pat -c call_pat -g grep_pat -r rm_pat -d doit"
 echo "#( ) "

