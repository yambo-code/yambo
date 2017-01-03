{
if (NR==1){ADDED=0}
if (index($0,"#include")>0 || index($0,"implicit")>0)
{
print "#if defined _TIMING"
print " use timing_m,       ONLY:timing"
print "#endif"
NAME=FILENAME
gsub(".F","",NAME)
print "#if defined _TIMING"
print " call timing('"NAME"',OPR='start')"
print "#endif"
}
if (index($0,"end subroutine")>0 || index($0,"end function")>0 || index($0,"contains")>0)
{
if (ADDED==0){
print "#if defined _TIMING"
print " call timing('"NAME"',OPR='stop')"
print "#endif"
print " !"
ADDED=1
}
}
print $0
}
function repeat( str, n,    rep, i )
{
    for( ; i<n; i++ )
        rep = rep str   
    return rep
}
