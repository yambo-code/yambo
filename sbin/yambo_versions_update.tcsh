#! /bin/tcsh -f
#
#        Copyright (C) 2000-2016 the YAMBO team
#              http://www.yambo-code.org
#
# Authors (see AUTHORS file for details): AM
# 
# This file is distributed under the terms of the GNU 
# General Public License. You can redistribute it and/or 
# modify it under the terms of the GNU General Public 
# License as published by the Free Software Foundation; 
# either version 2, or (at your option) any later version.
#
# This program is distributed in the hope that it will 
# be useful, but WITHOUT ANY WARRANTY; without even the 
# implied warranty of MERCHANTABILITY or FITNESS FOR A 
# PARTICULAR PURPOSE.  See the GNU General Public License 
# for more details.
#
# You should have received a copy of the GNU General Public 
# License along with this program; if not, write to the Free 
# Software Foundation, Inc., 59 Temple Place - Suite 330,Boston, 
# MA 02111-1307, USA or visit http://www.gnu.org/copyleft/gpl.txt.
#
#==================================================
set awk     = awk
#==================================================
#
if ( $#argv < 1 ) goto HELP
if ( $#argv > 1 ) goto HELP
#
# Get current version & revision
#
set dir=`svn info | grep 'URL'| grep 'yambo-devel' |wc -l`
set dummy=`svn info -r HEAD | grep 'Revision'`
set revision_HEAD=`echo $dummy | $awk '{gsub("Revision: ","");print $0}'`
#
set gpl="yes"
if ( "$dir" == "1" ) set gpl="no"
#
set dummy=`cat include/version.inc | grep 'code_version(1)'`
set version_old=`echo $dummy | $awk '{gsub("code_version\\(1\\)=","");print $0}'`
set dummy=`cat include/version.inc | grep 'code_version(2)'`
set patch_old=`echo $dummy | $awk '{gsub("code_version\\(2\\)=","");print $0}'`
set dummy=`cat include/version.inc | grep 'code_version(3)'`
set sub_old=`echo $dummy | $awk '{gsub("code_version\\(3\\)=","");print $0}'`
set dummy=`cat include/version.inc | grep 'code_revision'`
set revision_old=`echo $dummy | $awk '{gsub("code_revision=","");print $0}'`
set dummy=`cat include/version.inc | grep 'code_GPL_revision'`
set GPL_revision_old=`echo $dummy | $awk '{gsub("code_GPL_revision=","");print $0}'`
#
# Increase counters
#
set version_new = $version_old
set patch_new = $patch_old
set sub_new = $sub_old
set revision_new = $revision_HEAD
#
if ( "$argv[1]" == "v" ) @ version_new ++
if ( "$argv[1]" == "v" ) @ patch_new = 0
if ( "$argv[1]" == "v" ) @ sub_new = 0
if ( "$argv[1]" == "p" ) @ patch_new ++
if ( "$argv[1]" == "p" ) @ sub_new = 0
if ( "$argv[1]" == "s" ) @ sub_new ++
#
if ( "$argv[1]" != "save" ) then
  @ revision_new ++ 
  echo 
  if ( "$gpl" == "yes" ) then
    echo "v."$version_old"."$patch_old"."$sub_old " r."$GPL_revision_old " => " \
         "v."$version_new"."$patch_new"."$sub_new " r."$revision_new
  else
    echo "v."$version_old"."$patch_old"."$sub_old " r."$revision_old " => " \
         "v."$version_new"."$patch_new"."$sub_new " r."$revision_new
  endif
  echo 
else
 set source_dir="yambo-"$version_new"."$patch_new"."$sub_new
 set file_name=$source_dir"-"$revision_new".tar"
 echo "archive of " $source_dir " is " "../"$file_name".gz"
endif
#
echo -n "Confirm ?"
if ($< =~ [Yy]*) then
#
# Version strings
echo 'code_version(1)='$version_new  >  include/version.inc
echo 'code_version(2)='$patch_new    >> include/version.inc
echo 'code_version(3)='$sub_new      >> include/version.inc
if ( "$gpl" == "yes" ) then
 echo 'code_revision='$revision_old >> include/version.inc
 echo 'code_GPL_revision='$revision_new >> include/version.inc
else
 echo 'code_revision='$revision_new >> include/version.inc
 echo 'code_GPL_revision='$GPL_revision_old >> include/version.inc
endif
#
# Prepare new configure script
#
if ( "$gpl" == "yes" ) then
cat << EOF > ss.awk
{
 gsub("$version_old\\\.$patch_old\\\.$sub_old r\\\.$GPL_revision_old",
      "$version_new.$patch_new.$sub_new r.$revision_new",\$0)
 gsub("SVERSION=\"$version_old\"","SVERSION=\"$version_new\"",\$0)
 gsub("SPATCHLEVEL=\"$patch_old\"","SPATCHLEVEL=\"$patch_new\"",\$0)
 gsub("SSUBLEVEL=\"$sub_old\"","SSUBLEVEL=\"$sub_new\"",\$0)
 gsub("SREVISION=\"$GPL_revision_old\"","SREVISION=\"$revision_new\"",\$0)
 print \$0 > "NEW"
}
EOF
else
cat << EOF > ss.awk
{
 gsub("$version_old\\\.$patch_old\\\.$sub_old r\\\.$revision_old",
      "$version_new.$patch_new.$sub_new r.$revision_new",\$0)
 gsub("SVERSION=\"$version_old\"","SVERSION=\"$version_new\"",\$0)
 gsub("SPATCHLEVEL=\"$patch_old\"","SPATCHLEVEL=\"$patch_new\"",\$0)
 gsub("SSUBLEVEL=\"$sub_old\"","SSUBLEVEL=\"$sub_new\"",\$0)
 gsub("SREVISION=\"$revision_old\"","SREVISION=\"$revision_new\"",\$0)
 print \$0 > "NEW"
}
EOF
endif
#
# Version Update
#
#
if ( "$argv[1]" != "save" ) then
  $awk -f ss.awk ./config/configure.ac
  mv NEW ./config/configure.ac
  $awk -f ss.awk configure
  mv NEW configure
  chmod a+x configure
endif
rm -fr ss.awk
#
# Backup
#
if ( "$argv[1]" == "save" ) then
 cd ..
 echo -n " Tar ..."
 ln -s trunk $source_dir
 tar -chf $file_name $source_dir
 echo " done"
 gzip $file_name
 rm -f $source_dir
endif

endif

exit 0

HELP:
echo "yambo_versions_update.tcsh [(save)/(r)evision/(v)ersion/(p)atchlevel/(s)ubversion]"