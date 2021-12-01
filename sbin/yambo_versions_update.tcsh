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
  echo "Argument '"$argv"' not recognised. Options are:"
  goto HELP
endif
#
# Get current version & revision
#
set repo=`git remote -v | grep push`
#
set dummy="_VERSION"
set version_old=`cat include/driver/version.h | grep $dummy | $awk '{split($0,frags);print frags[3]}'`
set dummy="_SUBVERSION"
set subver_old=`cat include/driver/version.h | grep $dummy | $awk '{split($0,frags);print frags[3]}'`
set dummy="_PATCHLEVEL"
set patch_old=`cat include/driver/version.h | grep $dummy | $awk '{split($0,frags);print frags[3]}'`
set dummy="_REVISION"
set revision_old=`cat include/driver/version.h | grep $dummy | $awk '{split($0,frags);print frags[3]}'`
set dummy="_HASH"
set hash_old=`cat include/driver/version.h | grep $dummy | $awk '{gsub("\""," ");split($0,frags);print frags[3]}'`
#
set dummy1=`git rev-list --count HEAD`
@ dummy1= $dummy1 + 10000 
if ( "$dummy1" >= "$revision_old" ) set revision_HEAD=`echo $dummy1`
if ( "$dummy1" <  "$revision_old" ) set revision_HEAD=`echo $revision_old`
set hash_HEAD=`git rev-parse --short HEAD`
#
echo "Detected version" $version_old"."$subver_old"."$patch_old "Rev.(CURRENT)" $revision_old "(HEAD)" $revision_HEAD "Hash" $hash_old
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
set v_string_old=$version_old"."$subver_old"."$patch_old" r."$revision_old" h."$hash_old
set v_string_new=$version_new"."$subver_new"."$patch_new" r."$revision_new" h."$hash_new
#
if ( "$argv[1]" != "save" ) then
  echo 
  echo $v_string_old "=>" $v_string_new
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
# Prepare new configure script
#
set use_rev_old=$revision_old
set use_rev_new=$revision_new
#
if ( $version_old != $version_new ) then
cat << EOF > configure.awk
{
 gsub("version $version_old","version $version_new",\$0)
 print \$0
}
EOF
endif
cat << EOF > version.h.awk
{
 gsub("_VERSION $version_old"  ,"_VERSION $version_new"  ,\$0)
 gsub("_SUBVERSION $subver_old","_SUBVERSION $subver_new",\$0)
 gsub("_PATCHLEVEL $patch_old" ,"_PATCHLEVEL $patch_new", \$0)
 gsub("_REVISION $use_rev_old" ,"_REVISION $use_rev_new" ,\$0)
 gsub("_HASH \"$hash_old\""    ,"_HASH \"$hash_new\"" ,   \$0)
 print \$0 > "NEW"
}
EOF
cat << EOF > version.m4.awk
{
 gsub("$v_string_old","$v_string_new",\$0)
 gsub("SVERSION=\"$subver_old\"","SVERSION=\"$subver_new\"",\$0)
 gsub("SPATCHLEVEL=\"$patch_old\"" ,"SPATCHLEVEL=\"$patch_new\"", \$0)
 gsub("SREVISION=\"$use_rev_old\"" ,"SREVISION=\"$use_rev_new\"" ,\$0)
 gsub("SHASH=\"$hash_old\""    ,"SHASH=\"$hash_new\"" ,   \$0)
 print \$0 > "NEW"
}
EOF
#
if ( "$argv[1]" != "save" ) then
 if (  $version_old != $version_new ) then
   $awk -f configure.awk configure
   mv NEW configure
   chmod a+x configure
 endif
 $awk -f version.h.awk include/driver/version.h
 mv NEW include/driver/version.h
 $awk -f version.m4.awk config/version/version.m4
 mv NEW config/version/version.m4
endif
rm -fr version.*.awk configure.awk 
#
exit 0

HELP:
echo "yambo_versions_update.tcsh [(save) / (v)ersion/(s)ubversion/(p)atchlevel/@(r)evision.hash]"
