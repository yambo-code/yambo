{
LINE=$0
if (index($0,"Y_ALLOC")>0)
{
A=$0
B=substr($0,1,index($0,"%"))
gsub("\\("," ",A)
split(A,a)
if (index(B,")")>0)
{sub(a[3],a[3]",",$0)}
else
{sub(a[2],a[2]",",$0)}
gsub("Y_ALLOC","YAMBO_ALLOC",$0)
print $0
}
else if (index($0,"Y_FREE")>0)
{
gsub("Y_FREE","YAMBO_FREE",$0)
print $0
}
else if (index($0,"Y_FREE_P")>0)
{
gsub("Y_FREE_P","YAMBO_FREE_P",$0)
print $0
}
else if (index($0,"Y_MEM_ALLOC")>0)
{}
else if (index($0,"Y_MEM_FREE")>0)
{}
else
{
print $0
}
}
