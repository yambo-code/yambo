{
LINE=$0
split(LINE,a)
POS=index(LINE,a[1])
gsub(" ","",LINE)
if (index($0,"deallocate(")>0)
{
#print $0
VAR=substr(LINE, index(LINE,"deallocate("))
gsub("deallocate","YAMBO_FREE",VAR)
print repeat( " ", POS-1 ) VAR
}
else if (index($0,"allocate(")>0)
{
#print $0
VAR=substr(LINE, index(LINE,"allocate("))
sub("stat="," ",VAR)
split(VAR,a)
gsub("allocate","YAMBO_ALLOC",a[1])
VAR=substr(a[1],1,length(a[1])-1)

A=VAR
B=substr(VAR,1,index(VAR,"%"))
gsub("\\("," ",A)
split(A,a)
if (index(B,")")>0)
{sub(a[3],a[3]",",VAR)}
else
{sub(a[2],a[2]",",VAR)}

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
