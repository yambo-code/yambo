#! /bin/tcsh -f
awk -f /home/marini/Yambo/sources/git/yambo/branches/devel-memory/alloc.awk < $1 > NEW
meld NEW $1
mv NEW $1
