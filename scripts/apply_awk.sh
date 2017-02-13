#! /bin/tcsh
#
set file_list = `find . -name '*.F'`
unalias cp
unalias mv
foreach file  ($file_list)
 awk -f /home/marini/Yambo/sources/git/yambo/branches/SLK/scripts/alloc2.awk < $file > ${file}_new
 mv ${file}_new $file
end
