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
sub add_a_database_line
#========================
{
 $local_id=@_[0];
 $file= $DB_file;
 if (@_[2]) {$filename = basename("@_[2]")};
 $old = $file;
 $new1 = "$file.tmp1.$$";
 $new2 = "$file.tmp2.$$";
 $new3 = "$file.tmp3.$$";
 $new_line = "$local_id @_[1] $filename";
 $add_it="yes";
 open(OLD, "< $old")         or die "can't open $old: $!";
 open(NEW1, "> $new1")       or die "can't open $new1: $!";
 open(NEW2, "> $new2")       or die "can't open $new2: $!";
 open(NEW3, "> $new3")       or die "can't open $new3: $!";
 if ( "@_[1]" =~ "description"){
  $local_description_file="$HOME/.ydb/${local_id}_description";
  for($ik = 1; $ik < 100; $ik++) {
   if (exists($RUN_description[$IRUN_in][$ik])){
    $return_value = system("echo '$RUN_description[$IRUN_in][$ik]' >> $local_description_file"); 
   }
  }
  $return_value = system("vim $local_description_file"); 
  &remote_sftp_cmd("put $local_description_file $path/$RUN_material[$IRUN_in]/$local_id/description");
  $n_to_remove++;
  $FILE_to_remove[$n_to_remove]="$local_description_file";
 }
 while (<OLD>) {
  @line = split(' ',$_);
  if ( $line[0] < $local_id) {
   print NEW1 $_ if (!$quiet) ;
   next;
  }
  if ( $line[0] > $local_id) {
   print NEW3 $_ if (!$quiet) ;
   next;
  }
  $replace_it="no";
  if ( "@line" eq "$new_line" ){ $replace_it = "yes" };
  if ( "$line[1]" =~ "date" and "@_[1]" =~ "date"){ $replace_it = "yes"};
  if ( "$line[1]" =~ "father" and "@_[1]" =~ "father"){ $replace_it = "yes"};
  if ( "$line[1]" =~ "material" and "@_[1]" =~ "material"){ $replace_it = "yes"};
  #
  if ( "$line[1]" =~ "running" and "@_[1]" =~ "running"){ 
   $replace_it = "yes";
   &remote_ssh_cmd("echo $running > $path/$RUN_material[$IRUN_in]/$local_id/running");
  }
  #
  if ( "$line[1]" =~ "description" and "@_[1]" =~ "description"){ 
   open(DESC_file,"<","$local_description_file");
   @DESC=<DESC_file>;
   $ik=0;
   foreach $desc_line (@DESC) {
    $ik++;
    chomp($desc_line);
    if (length($desc_line) > 0 ) {
      if ($ik eq 1) { $new_line = "$local_id @_[1] $desc_line"};
      if ($ik >  1) { $new_line = "${new_line}NEWLINE$desc_line"};
    }
   }
   close(DESC_file);
   $replace_it = "yes";
  }
  if ( "@_[1]" =~ "tag"){
   if ("$line[1]" =~ "tag" and !$input and !$output and !$database) {$replace_it = "yes"};
   if ("$line[1]" eq "output" and $output) {
    if ("$line[2]" eq "$output") { 
     $new_line = "$line[0] $line[1] $line[2] $user_tags";
     $replace_it = "yes";
     &remote_ssh_cmd("echo $user_tags > '$path/$RUN_material[$IRUN_in]/$local_id/outputs/$line[2].tags'");
    }
   }
   if ("$line[1]" eq "input" and $input) {
    if ("$line[2]" eq "$input") { 
     $new_line = "$line[0] $line[1] $line[2] $user_tags";
     $replace_it = "yes";
     &remote_ssh_cmd("echo $user_tags > '$path/$RUN_material[$IRUN_in]/$local_id/inputs/$line[2].tags'");
    }
   }
   if ("$line[1]" eq "database" and $database) {
    if ("$line[2]" eq "$database") { 
     $new_line = "$line[0] $line[1] $line[2] $user_tags";
     $replace_it = "yes";
     &remote_ssh_cmd("echo $user_tags > '$path/$RUN_material[$IRUN_in]/$local_id/databases/$line[2].tags'");
    }
   }
  };
  #
  if ( "$replace_it" =~ "yes" ) { 
   $add_it="no";
   &elemental_add("$new_line","REPLACE");
  }
  #
  if ( "$replace_it" =~ "no" ) { &elemental_add("$_","OLD") };
 }
 if ( "$add_it" =~ "yes"  ) { &elemental_add("$new_line","NEW") };
 #
 close(OLD)                  or die "can't close $old: $!";
 close(NEW1)                 or die "can't close $new1: $!";
 close(NEW2)                 or die "can't close $new2: $!";
 close(NEW3)                 or die "can't close $new3: $!";
 if (!$quiet) {$return_value = system("cat $new1 $new2 $new3 > $file")};
 unlink $new1;
 unlink $new2;
 unlink $new3;
 #
 if ("@_[1]" =~ "material")   { &remote_sftp_cmd("rename $path/$RUN_material[$IRUN_in] $path/$material"); }
 if ("@_[1]" =~ "tag"){ &remote_ssh_cmd("echo $filename > $path/$RUN_material[$IRUN_in]/$local_id/tags"); }
}
sub elemental_add
{
 print "@_[0] (@_[1])\n " if ( $quiet and "@_[1]" ne "OLD");
 print NEW2 "@_[0]"       if (!$quiet and "@_[1]" eq "OLD");
 print NEW2 "@_[0]\n"     if (!$quiet and "@_[1]" ne "OLD");
}
sub delete_database_entry
#========================
{
 $local_id=@_[0];
 $which = @_[1];
 $what  = @_[2];
 $file  = $DB_file;
 $old   = $file;
 $new   = "$file.tmp.$$";
 open(OLD, "< $old")         or die "can't open $old: $!";
 open(NEW, "> $new")         or die "can't open $new: $!";
 while (<OLD>) {
  @line = split(' ',$_);
  if ( "$line[0]" =~ "$local_id") 
  {
   if ($what =~ "all") { 
    print "REMOVED: $_";
    next 
   };
   if ($which eq $line[1]) { 
    if ("$line[2]" eq "$what" ){
     print "REMOVED: $_";
     next;
    }
   };
   print NEW $_ ;
  }else{
   print NEW $_ ;
  } 
 }
 close(OLD)                  or die "can't close $old: $!";
 close(NEW)                  or die "can't close $new: $!";
 if ($quiet) {
  unlink $new;
 }else{
  rename($new, $old)          or die "can't rename $new to $old: $!";
 }
}
1;
