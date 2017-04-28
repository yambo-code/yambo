#
#        Copyright (C) 2000-2017 the YAMBO team
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
sub remove_run{
 #
 $todo=" ";
 if (!$input and !$output and !$database) {$todo="all"};
 if ($IRUN_in eq 0) {return};
 if ( "$todo" eq "all") {
  $test=&prompt("\n Do you really want to delete the ID $ID_in DB entry ");
  if ("$test" eq "n") {return};
 }
 #
 print "\n\n Removing RUN $IRUN_in (ID $ID_in) ...\n\n";
 #
 if ($quiet) { print "\n\n Removed DATABASE entries\n\n"};
 #
 for($if = 1; $if < 100; $if++) {
   if (exists($RUN_out[$IRUN_in][$if])){
    if ( "$RUN_out[$IRUN_in][$if]" eq "$output" or "$todo" eq "all") {
     &remote_sftp_cmd("rm $path/$RUN_material[$IRUN_in]/$ID_in/outputs/$RUN_out[$IRUN_in][$if].gz");
     &delete_database_entry($ID_in,"output",$RUN_out[$IRUN_in][$if]);
    }
   }
   if (exists($RUN_in[$IRUN_in][$if])){
    if ( ($input and "$RUN_in[$IRUN_in][$if]" eq "$input") or "$todo" eq "all" or "$input" eq "all") {
     &remote_sftp_cmd("rm $path/$RUN_material[$IRUN_in]/$ID_in/inputs/$RUN_in[$IRUN_in][$if]");
     &delete_database_entry($ID_in,"input",$RUN_in[$IRUN_in][$if]);
    }
   }
   if (exists($RUN_db[$IRUN_in][$if])){
    if ( ($database and "$RUN_db[$IRUN_in][$if]" eq "$database") or "$todo" eq "all" or  "$database" eq "all") {
     &remote_sftp_cmd("rm $path/$RUN_material[$IRUN_in]/$ID_in/databases/$RUN_db[$IRUN_in][$if].nc.gz");
     &delete_database_entry($ID_in,"database",$RUN_db[$IRUN_in][$if]);
    }
   }
 };
 if ("$todo" =~ "all") {
  if ( "$RUN_description[$IRUN_in][1]" ne "") {&remote_sftp_cmd("rm $path/$RUN_material[$IRUN_in]/$ID_in/description")};
  &remote_sftp_cmd("rmdir $path/$RUN_material[$IRUN_in]/$ID_in/inputs");
  &remote_sftp_cmd("rmdir $path/$RUN_material[$IRUN_in]/$ID_in/outputs");
  &remote_sftp_cmd("rmdir $path/$RUN_material[$IRUN_in]/$ID_in/databases");
  &remote_sftp_cmd("rmdir $path/$RUN_material[$IRUN_in]/$ID_in");
  &delete_database_entry($ID_in," ","all");
 }
}
1;
