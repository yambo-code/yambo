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
sub create_new_run{
 #
 # New ID
 $ID = $runs+1;
 #
 $test=&have_run();
 if ($test ne 0 ) {
  &add_a_database_line($test,"description","$create");
  die "\n Found matching run with ID $test\n\n";
  return;
 };
 #
 print "\n\n Creating RUN...";
 #
 #############################
 print "\n ID\t\t:$ID\n";
 if (&have_material() eq 0) {&remote_cmd("mkdir $path/$material")};
 &remote_cmd("mkdir $path/$material/$ID");
 &remote_cmd("mkdir $path/$material/$ID/input");
 &remote_cmd("mkdir $path/$material/$ID/output");
 &remote_cmd("mkdir $path/$material/$ID/databases");
 $RUN_dir="$path/$material/$ID";
 $database_line[0]="$ID material $material";
 $database_line[1]="$ID date $date";
 $database_line[2]="$ID description $create";
 $ic=2;
 foreach $key (@keys) {
  $ic++;
  $database_line[$ic]="$ID key $key";
 };
 $local_description_file="$HOME/.ydb/${ID}_description";
 open(LOCAL_DESC,'>',$local_description_file) or die;
 foreach $line (@database_line) {
  print DB "$line\n";
 }
 print LOCAL_DESC "$create";
 close(LOCAL_DESC);
 &remote_cmd("put $local_description_file $RUN_dir/description");
 $n_to_remove++;
 $FILE_to_remove[$n_to_remove]="$local_description_file";
}
1;
