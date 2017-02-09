{
LINE=$0
if (index($0,"Y_FREE")>0)
{
print $0
gsub("Y_FREE","Y_MEM_FREE",$0)
print $0
}else{
print $0
}
}

