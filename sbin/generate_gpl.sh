#! /bin/sh
#
if [ $1 = "-h" ] ; then
 echo $0 "OPTIONS[diff,zip,tar]"
 exit 0
fi

BASE="/home/sangalli/data/Lavoro/Codici/yambo/the_wole_project/branches/4.1"
PREV="/home/sangalli/data/Lavoro/Codici/yambo/the_wole_project_gpl/branches/4.1"
TARGET="/home/sangalli/data/Lavoro/Codici/yambo/the_wole_project_gpl/master"
ACTION="update"
if [ $# = 1 ] ; then  ACTION=$1 ; fi
#
#PJ="KERR SURF YPP_SURF ELPH YPP_ELPH FFTW FFTSG OPENMP MPI" 
PJ="KERR ELPH YPP_ELPH FFTW FFTSG OPENMP MPI" 

echo 
echo  "FROM DEV: $BASE"
echo  "PREV GPL: $PREV"
echo  "TARGET  : $TARGET"
echo  "ACTION  : $ACTION"
#sleep 3s

cd $TARGET

if [ $ACTION = "update" ] ; then
 rm -fr *
 cp -R $BASE/*  .
 #find . -name .objects_gpl | grep -v git | gawk '{print "cpp -P " $0 " > A ; mv A " $0 }' > CPP.batch
 #chmod u+x CPP.batch
 #rm -f CPP.batch 
 #./sbin/yamboo.pl -p="KERR SURF YPP_SURF ELPH YPP_ELPH"
 ./sbin/yamboo.pl -p="$PJ"
 chmod u+x delete.batch
 ./delete.batch
 rm -f delete.batch 
 find . -name git | xargs rm -fr
 diff -r . $PREV | \
   grep "Only in" | \
   grep -v branches | \
   grep -v git | \
   grep -v sbin | \
   grep -v yamboo | \
   grep -v README.branches_and_trunk | \
   grep -v OpenMp | \
   grep -v doxygen | \
   grep -v README.configure_libs_install | \
   grep -v commit.msg | \
   grep -v ChangeLog | \
   grep -v ONLY | \
   grep -v Write | \
   gawk '{gsub(": ","/",$0) ;na=split($0,a);print a[3]}'  > ONLY_in_trunk
 diff -r . $PREV | \
   grep "Only in" | \
   grep -v branches | \
   grep -v git | \
   grep -v sbin | \
   grep -v yamboo | \
   grep -v README.branches_and_trunk | \
   grep -v OpenMp | \
   grep -v doxygen | \
   grep -v README.configure_libs_install | \
   grep -v commit.msg | \
   grep -v ChangeLog | \
   grep -v Write | \
   grep -v ONLY | \
   gawk '{gsub(": ","/",$0) ;na=split($0,a);print "git add " a[3]}'  > git_add_in_branch.batch
 diff -r . $PREV | \
   grep "Only in" | \
   grep branches | \
   grep -v git | \
   grep -v sbin | \
   grep -v yamboo | \
   grep -v README.branches_and_trunk | \
   grep -v OpenMp | \
   grep -v doxygen | \
   grep -v README.configure_libs_install | \
   grep -v commit.msg | \
   grep -v ChangeLog | \
   grep -v Write | \
   grep -v ONLY | \
   gawk '{gsub(": ","/",$0) ;na=split($0,a);print a[3]}'  > ONLY_in_branch
 diff -r . $PREV | \
   grep "Only in" | \
   grep branches | \
   grep -v git | \
   grep -v sbin | \
   grep -v yamboo | \
   grep -v README.branches_and_trunk | \
   grep -v OpenMp | \
   grep -v doxygen | \
   grep -v README.configure_libs_install | \
   grep -v commit.msg | \
   grep -v ChangeLog | \
   grep -v Write | \
   grep -v ONLY | \
   gawk '{gsub(": ","/",$0) ;na=split($0,a);print " git rm -f " a[3]}'  > gitdelete_in_branch.batch
 chmod u+x  *.batch
 diff -r . $PREV | grep "diff -r" | grep -v git > DIFF
 cat DIFF | gawk '{na=split($0,a); gsub("\\./","",a[3]) ;print a[3]}' > FILE_LIST
 echo 
 echo "===ONLY_in_trunk files in 1 sec==="
 echo 
 sleep 1s
 cat ONLY_in_trunk
 echo 
 echo "===ONLY_in_branch files in 1 sec==="
 echo 
 sleep 1s
 cat ONLY_in_branch
 echo
 echo "===DIFF files in 1 sec==="
 echo
 sleep 1s
 cat FILE_LIST
fi

if [ $ACTION = "tar" ] ; then
 rm -fr *
 cd $BASE
 STRNG=`./configure --help | grep adapt`
 VERSION=`echo $STRNG | awk '{na=split($0,a); print a[4]}'`
 SRC_NAME="yambo-"${VERSION}
 cd ~/Yambo/WorkSpace
 cp -R $BASE $SRC_NAME
 find . -name '.git' | xargs rm -fr
 tar cvf ${SRC_NAME}.tar $SRC_NAME
 gzip  ${SRC_NAME}.tar
fi

echo $PREV
if [ $ACTION = "diff" ] ; then
 FILES=`cat DIFF | gawk '{na=split($0,a);print a[3]}' `
 for file in $FILES; do meld $file $PREV/$file ; done
fi

if [ $ACTION = "zip" ] ; then
 cat FILE_LIST | zip -@ files_to_update.zip 
 cat ONLY_in_trunk | zip -@ files_to_update.zip 
 FILES=`cat ONLY_in_trunk`
 for file in $FILES; do
  if test -d $file; then
   find $file | grep -v 'git' | zip -@ files_to_update.zip
  fi
 done
fi


