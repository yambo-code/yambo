#! /bin/tcsh -f
#
#        Copyright (C) 2000-2018 the YAMBO team
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
if ( "$argv[1]" != "r" && "$argv[1]" != "v" && "$argv[1]" != "s" && "$argv[1]" != "p" && "$argv[1]" != "save" ) then
  echo "Argoument '"$argv"' not recognised. Options are:"
  goto HELP
endif
#
# Get current version & revision
#
set dir=`git branch | grep 'master' |wc -l`
set dummy=`git rev-list --count HEAD`
set revision_HEAD=`echo $dummy`
@ revision_HEAD= $revision_HEAD + 10000 
set hash_HEAD=`git rev-parse --short HEAD`
#
set gpl="yes"
if ( "$dir" == "1" ) set gpl="no"
#
set dummy=`cat include/version.inc | grep 'code_version(1)'`
set version_old=`echo $dummy | $awk '{gsub("code_version\\(1\\)=","");print $0}'`
set dummy=`cat include/version.inc | grep 'code_version(2)'`
set subver_old=`echo $dummy | $awk '{gsub("code_version\\(2\\)=","");print $0}'`
set dummy=`cat include/version.inc | grep 'code_version(3)'`
set patch_old=`echo $dummy | $awk '{gsub("code_version\\(3\\)=","");print $0}'`
set dummy=`cat include/version.inc | grep 'code_revision'`
set revision_old=`echo $dummy | $awk '{gsub("code_revision=","");print $0}'`
set dummy=`cat include/version.inc | grep 'code_GPL_revision'`
set GPL_revision_old=`echo $dummy | $awk '{gsub("code_GPL_revision=","");print $0}'`
set dummy=`cat include/version.inc | grep 'code_hash'`
set hash_old=`echo $dummy | $awk '{gsub("code_hash=","");print $0}'`
set dummy=$hash_old
set hash_old=`echo $dummy | sed -e "s/'//g" `
#
# Increase counters
#
set version_new = $version_old
set subver_new = $subver_old
set patch_new = $patch_old
set revision_new = $revision_old
set hash_new = $hash_old
#
if ( "$argv[1]" == "v" ) then
  @ version_new ++
  @ subver_new = 0
  @ patch_new  = 0
endif
#
if ( "$argv[1]" == "s" ) then
  @ subver_new ++
  @ patch_new = 0
endif
#
if ( "$argv[1]" == "p" ) @ patch_new ++
#
if ( "$argv[1]" == "r" || "$argv[1]" == "v" || "$argv[1]" == "s" || "$argv[1]" == "p" ) then
  @ revision_HEAD ++
  set revision_new = $revision_HEAD
  set hash_new     = $hash_HEAD
endif
#
if ( "$argv[1]" != "save" ) then
  echo 
  if ( "$gpl" == "yes" ) then
    echo "v."$version_old"."$subver_old"."$patch_old " r."$GPL_revision_old " => " \
         "v."$version_new"."$subver_new"."$patch_new " r."$revision_new
  else
    echo "v."$version_old"."$subver_old"."$patch_old " r."$revision_old " h."$hash_old" => " \
         "v."$version_new"."$subver_new"."$patch_new " r."$revision_new " h."$hash_new""
  endif
  echo 
else
 set source_dir="yambo-"$version_new"."$subver_new"."$patch_new
 set file_name=$source_dir"-"$revision_new".tar"
 echo "archive of " $source_dir " is " "../"$file_name".gz"
endif
#

set update = 0
if ( "$argv[1]" == "v" || "$argv[1]" == "s" || "$argv[1]" == "p" ) then
  set update = 1
  echo -n "Confirm ?"
  if ($< =~ [Yy]*) then
    set update = 0
  endif
endif
#
if( "$update" == "0" ) then
  #
  # Version strings
  #
  echo 'code_version(1)='$version_new  >  include/version.inc
  echo 'code_version(2)='$subver_new   >> include/version.inc
  echo 'code_version(3)='$patch_new    >> include/version.inc
  echo "code_hash='"$hash_new"'"       >> include/version.inc
  #
  # Revision strings
  #
  if ( "$gpl" == "yes" ) then
    echo 'code_revision='$revision_old     >> include/version.inc
    echo 'code_GPL_revision='$revision_new >> include/version.inc
  else
    echo 'code_revision='$revision_new         >> include/version.inc
    echo 'code_GPL_revision='$GPL_revision_old >> include/version.inc
  endif
endif
#
# Prepare new configure script
#
set use_rev_old=$revision_old
set use_rev_new=$revision_new
if ( "$gpl" == "yes" ) then
  set use_rev_old=$GPL_revision_old
  set use_rev_new=$revision_new
endif
#
cat << EOF > ss.awk
{
 gsub("$version_old.$subver_old.$patch_old r.$use_rev_old h.$hash_old",
      "$version_new.$subver_new.$patch_new r.$use_rev_new h.$hash_new",\$0)
 gsub("based on r.$use_rev_old h.$hash_old","based on r.$use_rev_new h.$hash_new",\$0)
 #version
 gsub("SVERSION=\"$version_old\""  ,"SVERSION=\"$version_new\""  ,\$0)
 gsub("SSUBVERSION=\"$subver_old\"","SSUBVERSION=\"$subver_new\""   ,\$0)
 gsub("SPATCHLEVEL=\"$patch_old\"","SPATCHLEVEL=\"$patch_new\"",\$0)
 #revision
 gsub("SREVISION=\"$use_rev_old\"" ,"SREVISION=\"$use_rev_new\"" ,\$0)
 gsub("BASE_REV=\"$use_rev_old\"" ,"BASE_REV=\"$use_rev_new\"" ,\$0)
 gsub("SHASH=\"$hash_old\""        ,"SHASH=\"$hash_new\""        ,\$0)
 print \$0 > "NEW"
}
EOF
#
#
if ( "$argv[1]" != "save" ) then
  $awk -f ss.awk ./config/version.m4
  mv NEW ./config/version.m4
  $awk -f ss.awk ./config/version.m4_gpl
  mv NEW ./config/version.m4_gpl
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

exit 0

HELP:
echo "yambo_versions_update.tcsh [(save) / (v)ersion/(s)ubversion/(p)atchlevel/@(r)evision.hash]"
