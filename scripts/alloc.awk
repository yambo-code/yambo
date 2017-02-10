{
LINE=$0
split(LINE,a)
POS=index(LINE,a[1])
gsub(" ","",LINE)
if (index($0,"deallocate(")>0)
{
#print $0
VAR=substr(LINE, index(LINE,"deallocate("))
gsub("deallocate","Y_MEM_FREE",VAR)
print repeat( " ", POS-1 ) VAR
gsub("Y_MEM_FREE","Y_FREE",VAR)
print repeat( " ", POS-1 ) VAR
}
else if (index($0,"allocate(")>0)
{
#print $0
VAR=substr(LINE, index(LINE,"allocate("))
sub("stat="," ",VAR)
split(VAR,a)
gsub("allocate","Y_ALLOC",a[1])
VAR=substr(a[1],1,length(a[1])-1)
print repeat( " ", POS-1 ) VAR")"
gsub("\\("," ",VAR)
split(VAR,b)
if (index(VAR,"%")==0) {
OBJ=b[2]
}
else
{
OBJ=b[2]
}
print repeat( " ", POS-1 ) "Y_MEM_ALLOC("OBJ")"
}
else
{
 if (index ($0,"implicit")> 0) {
   print "#include<memory.h>"
 }
 else if (index ($0,"mem_est(")==0) {print $0}
}
}
function repeat( str, n,    rep, i )
{
    for( ; i<n; i++ )
        rep = rep str   
    return rep
}
