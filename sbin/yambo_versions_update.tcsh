#! /bin/tcsh -f
#
# License-Identifier: GPL
#
# Copyright (C) 2008 The Yambo Team
#
# Authors (see AUTHORS file for details): AM
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
set  version_old=`cat include/version/version.m4 | grep SVERSION | $awk '{split($0,frags,"SVERSION=");gsub("\"","",frags[2]);print frags[2]}'`
set   subver_old=`cat include/version/version.m4 | grep SSUBVERSION | $awk '{split($0,frags,"SSUBVERSION=");gsub("\"","",frags[2]);print frags[2]}'`
set revision_old=`cat include/version/version.m4 | grep SREVISION | $awk '{split($0,frags,"SREVISION=");gsub("\"","",frags[2]);print frags[2]}'`
set    patch_old=`cat include/version/version.m4 | grep SPATCHLEVEL | $awk '{split($0,frags,"SPATCHLEVEL=");gsub("\"","",frags[2]);print frags[2]}'`
set     hash_old=`cat include/version/version.m4 | grep SHASH | $awk '{split($0,frags,"SHASH=");gsub("\"","",frags[2]);print frags[2]}'`
#
set dummy1=`git rev-list --count HEAD`
@ dummy1= $dummy1 + 10000 
if ( "$dummy1" >= "$revision_old" ) set revision_HEAD=`echo $dummy1`
if ( "$dummy1" <  "$revision_old" ) set revision_HEAD=`echo $revision_old`
set hash_HEAD=`git rev-parse --short HEAD`
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
 $awk -f version.m4.awk include/version/version.m4
 mv NEW include/version/version.m4
endif
rm -fr version.*.awk configure.awk 
#
exit 0

HELP:
echo "yambo_versions_update.tcsh [(save) / (v)ersion/(s)ubversion/(p)atchlevel/@(r)evision.hash]"
