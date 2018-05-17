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
sub local_uncompress{
#===========================
 #
 print "\n Uncompresing $ID_in ...\n";
 #
 foreach $file (<$local_dir/outputs/*gz>,<$local_dir/databases/*gz>) {
  &local_cmd("gunzip -f '$file'");
 }
 foreach $file (<$local_dir/databases/*nc>) {
  my $filename = basename("$file",  ".nc");
  &local_cmd("ncdump < $file > $filename");
 }
}
sub get_the_run{
#===========================
 $RUN_dir="$path/$RUN_material[$IRUN_in]/$ID_in";
 #
 print "\n Fetching RUN $IRUN_in (ID $ID_in) ...\n";
 #
 &print_the_run($ID_in);
 if ($see) { 
  $n_to_remove++;
  $FILE_to_remove[$n_to_remove]="$local_dir/";
  if ($input) {
   $n_to_remove++;
   $FILE_to_remove[$n_to_remove]="$local_dir/inputs";
  }
  if ($output) {
   $n_to_remove++;
   $FILE_to_remove[$n_to_remove]="$local_dir/output";
  }
 }
 #
 for($ik = 1; $ik < 100; $ik++) {
  if (exists($RUN_in[$IRUN_in][$ik]) and $input){
   if ($RUN_in[$IRUN_in][$ik] =~ /$input/ or "$input" =~ "all"){
    if ($see) { 
     $return_value = system("scp $RUN_dir/inputs/$RUN_in[$IRUN_in][$ik] $local_dir/inputs/");
     $return_value = system("vim $local_dir/inputs/$RUN_in[$IRUN_in][$ik]");
     $n_to_remove++;
     $FILE_to_remove[$n_to_remove]="$local_dir/inputs/$RUN_in[$IRUN_in][$ik]";
    }else{
     &remote_sftp_cmd("get $RUN_dir/inputs/$RUN_in[$IRUN_in][$ik] $local_dir/inputs/");
    };
   }
  }
  if (exists($RUN_out[$IRUN_in][$ik]) and $output){
   if ($RUN_out[$IRUN_in][$ik] =~ /$output/ or "$output" =~ "all"){
    if ($see) { 
     $return_value = system("scp $RUN_dir/outputs/$RUN_out[$IRUN_in][$ik].gz $local_dir/outputs/");
     $return_value = system("gunzip -f $local_dir/outputs/$RUN_out[$IRUN_in][$ik].gz");
     $return_value = system("vim $local_dir/outputs/$RUN_out[$IRUN_in][$ik]");
     $n_to_remove++;
     $FILE_to_remove[$n_to_remove]="$local_dir/outputs/$RUN_out[$IRUN_in][$ik]";
    }else{
     &remote_sftp_cmd("get '$RUN_dir/outputs/$RUN_out[$IRUN_in][$ik].gz' $local_dir/outputs/");
    };
   }
  }
  if (exists($RUN_db[$IRUN_in][$ik]) and $database){
    if ($RUN_db[$IRUN_in][$ik] =~ /$database/ or "$database" =~ "all"){
    &remote_sftp_cmd("get '$RUN_dir/databases/$RUN_db[$IRUN_in][$ik].nc.gz' $local_dir/databases/");
   }
  }
 }
}
sub add_command_line_object{
#===========================
 if ($quiet) { print "\n\n New DATABASE entries\n\n"};
 $RUN_dir="$path/$RUN_material[$IRUN_in]/$ID_in";
 if ($input) {
  &remote_sftp_cmd("put $input $RUN_dir/inputs");
  &add_a_database_line($ID_in,"input","$input");
 };
 if ($output) {
  foreach $obj ($output){
   if (-f $obj) {
    &file_add("$obj");
   }
   if (-d $obj) {
    opendir (DIR, $obj) or die $!;
    while (my $el = readdir(DIR)) {
      next if ($el !~ m/^[o-]/ and  $el !~ m/^[r-]/ and $el !~ m/^[l-]/ );
      if ($key){next if ($el !~ /$key/)};
      &file_add("$obj/$el");
    }
   }
  }
 }
 if ($database) {
  foreach $obj ($database){
   if (-f $obj) {
    &db_add("$obj");
   }
   if (-d $obj) {
    opendir (DIR, $obj) or die $!;
    while (my $el = readdir(DIR)) {
      next if ($el !~ m/^[ndb-]/);
      if ($key){next if ($el !~ /$key/)};
      &db_add("$obj/$el");
    }
   }
  }
 }
}
sub file_add
#============
{
 $file="@_";
 chomp $file;
 my ($name, $dir, $ext) = fileparse($file, ".gz");
 $n_to_remove++;
 $FILE_to_remove[$n_to_remove]="$file.gz";
 if ( "$ext" ne ".gz") { &local_cmd("gzip -c '$file' > '$file'.gz")};
 &remote_sftp_cmd("put $file.gz $RUN_dir/outputs");
 &add_a_database_line($ID_in,"output","$file");
}
sub db_add
#==========
{
 $file="@_";
 next if ($file =~ m/[.nc]$/);
 next if ($file =~ m/[.gz]$/);
 $n_to_remove++;
 $FILE_to_remove[$n_to_remove]="$file.nc.gz";
 $n_to_remove++;
 $FILE_to_remove[$n_to_remove]="$file.nc";
 &local_cmd("ncdump $file> $file.nc");
 &local_cmd("gzip $file.nc");
 &remote_sftp_cmd("put $file.nc.gz $RUN_dir/databases");
 &add_a_database_line($ID_in,"database","$file");
}
1;
