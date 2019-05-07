{
na=split($0,a,",")
nb=split(a[1],b," ")
VAR=$0;
gsub("{","",VAR)
na=split(VAR,a,",")
if (b[1] == "#if")
{
 gsub("#if defined","",$0)
 gsub("_ELPH","ph",$0)
 gsub("_RT","rt",$0)
 gsub("_SC","sc",$0)
 gsub("_QED","qed",$0)
 gsub("_NL","nl",$0)
 gsub("_MAGNETIC","magnetic",$0)
 gsub("_ELECTRIC","electric",$0)
 gsub("_TEST_MAIN","test",$0)
 PJ=$0
 next
}
if (b[1] == "#endif")
{
 PJ=""
 next
}
if (a[2] == "NULL" || index($0,"{") == 0 ){next}

if (a[2] == "\"DESC\"")
{
 print "DESC" a[4]
}else{
 print "*i_opt=*i_opt+1;"
 print "options[*i_opt].short_desc="a[4]";"
 print "options[*i_opt].long_opt="a[1]";"
 print "options[*i_opt].short_opt="a[3]";"
 print "iptions[*i_opt].bin=\"a2y" PJ"\";"
 print "options[*i_opt].yambo_string="a[2]";"
 print "options[*i_opt].section=\"Interface\";"
}
}

