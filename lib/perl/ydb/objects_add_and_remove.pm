use lib "/home/marini/Yambo/sources/git/yambo/branches/devel-daddy/lib/perl/ydb";
use print_the_run;
use add_a_database_line;
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
