#! /bin/tcsh
#
unalias mv rm cp

######### AWK SECTION ####################

cat << EOF > AWK_split
{
 na = split (\$0,a)
 if (NR==1) {
  N=1
  start="no"
  contains="no"
 }
 if (substr(a[1],1,1) == "!" ) {
  print \$0  >> "MODULE_"N
  next
 }
 if (a[1]=="module" || a[1]=="MODULE") {nextfile}
 if (a[1]=="contains") {
  contains="yes"
  print \$0 >> "MODULE_"N
  next
 }
 if (a[1]=="function" || a[1]=="subroutine" || a[2] == "function" || a[2] == "subroutine") {
  if (contains=="yes") {
   print \$0 >> "MODULE_"N
   next
  }
  if (start=="no") 
  {
   start="yes"
   #print N \$0  
  }
  else
  {
   start="no"
   print \$0 >> "MODULE_"N
   N=N+1
   next
   #print N \$0  
  }
 }
 print \$0 >> "MODULE_"N
}
EOF

cat << EOF > AWK_nl
{
 na = split (\$0,a)
 if (substr(a[1],1,1) == "!" ) {next}
 if (NR==1) {
  line=\$0
  next
  na = split (\$0,a)
 }
 if (a[1] == "&" ) {
  for (i = 2; i <= na; i++)  { 
    line = line " " a[i]
   }
 }else{
  na = split (line,a)
  if (na==0) {
   line=\$0
   next
  }
  if( a[1]=="function" || a[1]=="subroutine" || a[2] == "function" || a[2] == "subroutine") {
    print line  >> "HEADER"
   }else{
    print line >> "PP"}
  line=\$0
 }
}
EOF

cat << EOF > AWK_separate
{
 if (NR==1) found_call="no"
 na = split (\$0,a)
 var_line = "yes"
 if (index(\$0,"use")>0 && index (\$0,"only")==0 &&  index (\$0,"ONLY")==0 ) var_line = "no"
 if (index(\$0,"call")>0) found_call = "yes"
 if (index(\$0,"&")==0 && index(\$0,"real")==0  &&
     index(\$0,"integer")==0 && index(\$0,"complex")==0  &&
     index(\$0,"logical")==0 && index(\$0,"use")==0 && 
     index(\$0,"type")==0    && index(\$0,"character")==0 ) {  var_line = "no" }
 if (var_line=="yes" && index(\$0,"write ")>0) { var_line = "no" }
 if (var_line=="yes" && index(\$0,"=")>0) { var_line = "no" }
 if (var_line=="yes" && index(\$0,"*")>0) { var_line = "no" }
 if (var_line=="yes" && found_call=="no") print \$0 > "VARIABLES"
 if (var_line=="no" || found_call=="yes") print \$0 > "BODY"
}
EOF

cat << EOF > AWK_analyze
{
 line=\$0
 if (NR ==1 ) { 
  NV=0
  V_not_found=""
 }
 if (index(line,"EOF")!=0) { 
  for (i = 1; i <= NV; i++) { 
    V_not_found=V_not_found" "V[i]
  }
  print V_not_found
 }
 if (index(line,"ONLY:")!=0) { line=substr(\$0,index(line,"ONLY:")+5) }
 if (index(line,"ONLY :")!=0) { line=substr(\$0,index(line,"ONLY :")+6) }
 if (index(line,"only:")!=0) { line=substr(\$0,index(line,"only:")+5) }
 if (index(line,"only :")!=0) { line=substr(\$0,index(line,"only :")+6) }
 if (index(line,"::")!=0) { 
   line=substr(\$0,index(line,"::")+2)
  }
 gsub(","," ",line)
 gsub("&","",line)
 na = split (line,a)
 for (i = 1; i <= na; i++)  { 
  if (index(a[i],"(")>0) { 
   a[i]=substr(a[i],1,index(a[i],"(")-1) 
   if (index(a[i+2],")")>0) {
    a[i+1]=" "
    a[i+2]=" "
   }
   if (index(a[i+1],")")>0) {a[i+1]=" "}
  }
  gsub("\\\("," ",a[i])
  gsub("\\\)"," ",a[i])
  gsub("\\\["," ",a[i])
  gsub("\\\]"," ",a[i])
  gsub("="," ",a[i])
  gsub("'"," ",a[i])
  gsub("/"," ",a[i])
  gsub("!"," ",a[i])
  gsub(":"," ",a[i])
  gsub("*"," ",a[i])
  gsub(" ","",a[i])
  tmp_var=a[i]
  for (j = 1; j <= 100; j++)  { 
   gsub("0","",tmp_var)
   gsub("1","",tmp_var)
   gsub("2","",tmp_var)
   gsub("3","",tmp_var)
   gsub("4","",tmp_var)
   gsub("5","",tmp_var)
   gsub("6","",tmp_var)
   gsub("7","",tmp_var)
   gsub("8","",tmp_var)
   gsub("9","",tmp_var)
  }
  if (length(a[i]) > 0 && length(tmp_var) >0 ) 
  {
   NV++
   V[NV]=a[i]
   #print "|"a[i]"|"tmp_var"|"
  }
 }
}
EOF

######### AWK SECTION ####################
#
# clean_unused_variables.tcsh clean
# clean_unused_variables.tcsh list  all/changed [dir/file]
# clean_unused_variables.tcsh clean all/changed [dir/file]
#

if ( $argv[1] =~ "clean" && $#argv == 1 ) then
 git ls-files --others | xargs rm -f
 exit 0
endif

set OBJ="."
if ( $#argv == 3 ) then
 set OBJ = $argv[3]
endif
if ( $#argv > 1 ) then
 set filter=$argv[2]
endif

echo "ACTION   :" $argv[1]
echo "FILTER   :" $filter
echo "OBJ/FILE :" $OBJ

set FILES = (  )

if (-d $OBJ) then
 foreach kind ( "modified" "new" "renamed" )  
  git status -uno $OBJ | grep $kind | grep -v '\.pl' | grep  -v '\.pm'  | grep -v '\.c' |grep -v '\.m4' | grep -v '\.git' |grep -v '\.\.\/' > LIST
  cat LIST | grep -v "mod_" | grep -v "Makefile" | grep -v "configure" | grep -v "\.h" | grep -v "\.object" | grep -v "\.tcsh" > LIST
  sed -i -e 's/new file/new_file/g' LIST
  #echo $kind  
  set FILES=($FILES `awk '{print $2}' "LIST"`)
 end
endif
if ($filter =~ "all" && $OBJ =~ ".") then
 set FILES = `find src -name '*.F' |grep -v "SLK_test" `
 set FILES = ($FILES `find ypp -name '*.F' `)
 set FILES = ($FILES `find interfaces -name '*.F' `)
endif
if ($filter =~ "all" && $OBJ !~ ".") then
 set FILES = `find $OBJ -name '*.F' |grep -v "SLK_test" `
endif

if (-f $OBJ) then
 set FILES=$OBJ
endif

#echo $FILES
#exit

foreach file ($FILES)
 gawk -f AWK_split ${file}
 #echo "processing $file..."
 @ N_unused=0
 foreach source (MODULE_*)
  #echo "$file $source..."
  gawk -f AWK_nl $source
  if (! -f PP) then
   echo
   continue
  endif
  mv PP ${file}"_PP"
  mv HEADER ${file}"_HEADER"
  gawk -f AWK_separate ${file}"_PP"
  mv VARIABLES ${file}"_VARIABLES"
  mv BODY ${file}"_BODY"
  echo "EOF" >>  ${file}"_VARIABLES"
  echo "EOF" >>  ${file}"_BODY"
  #gawk -f AWK_analyze "${file}"_VARIABLES
  set VARIABLES = `gawk -f AWK_analyze "${file}"_VARIABLES`
  set UNUSED = ( )
  foreach var ($VARIABLES)
   set var = `echo $var | sed 's/-//g'`
   set CHECK = `grep $var "${file}"_VARIABLES| wc -l`
   set FOUND = `grep $var "${file}"_BODY| wc -l`
   set HEAD  = `grep $var "${file}"_HEADER| wc -l`
   if ($FOUND == 0 && $CHECK == 1 && $HEAD == 0) then
    @ N_unused ++
    set UNUSED = ($UNUSED $var)
    #echo -n $var $source  $N_unused 
    #echo -n "..."$var"(removed) "

cat << EOF > AWK_replace
{
 na = split (\$0,a)
 if (substr(a[1],1,1) == "!" ) 
 {print \$0
  next}
 gsub("${var},&","&")
 gsub("${var},","")
 gsub(",${var}","")
 gsub("${var}","")
 print \$0
}
EOF
    gawk -f AWK_replace $source > CLEAN  
    mv CLEAN $source
   endif
  end
  #echo " " $#UNUSED "("$N_unused")"
  if ($#UNUSED>0) then
   echo "-----------------------------------------------------------------------------"
   echo $file"/"$source":"$UNUSED
   echo "-----------------------------------------------------------------------------"
  endif
 end
 if ($N_unused>0 && $argv[1] !~ "list") then
  cat MODULE_* > $file
 endif
 rm -f tmp "${file}"_* MODULE_* CLEAN LIST
end

rm -f AWK* 

exit 0
