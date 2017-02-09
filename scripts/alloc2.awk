{
LINE=$0
if (index($0,"Y_FREE_P")>0)
{
A=$0
gsub("Y_FREE_P","Y_MEM_FREE",A)
print A
print $0
}
else if (index($0,"Y_FREE")>0)
{
A=$0
gsub("Y_FREE","Y_MEM_FREE",A)
print A
print $0
}else{
print $0
}
}

