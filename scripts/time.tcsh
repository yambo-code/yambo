#! /bin/tcsh -f
awk -f /home/marini/Yambo/sources/git/yambo/branches/devel-memory/scripts/time.awk $1 > NEW
meld NEW $1
mv NEW $1
