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
sub add_command_line_object{
#===========================
 $RUN_dir="$path/$RUN_material[$ID_in]/$ID_in";
 if ($input) {
  &remote_cmd("put $input $RUN_dir/input");
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
      if ($key_words){next if ($el !~ /$keys[0]/)};
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
      if ($key_words){next if ($el !~ /$keys[0]/)};
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
 next if ($file =~ m/[.gz]$/);
 $n_to_remove++;
 $FILE_to_remove[$n_to_remove]="$file.gz";
 &local_cmd("gzip -k -f $file");
 &remote_cmd("put $file.gz $RUN_dir/output");
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
 &local_cmd("ncdump $file> $file.nc");
 &local_cmd("gzip -k -f $file.nc");
 &remote_cmd("put $file.nc.gz $RUN_dir/databases");
 &add_a_database_line($ID_in,"database","$file");
}
1;
