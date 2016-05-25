#!/usr/bin/perl
#
# Copyright (C) 2016 A. Marini
#
# based on the driver.pl written by Conor and (only partially) me
#
use Getopt::Long;
use File::Find;
use File::Copy;
use File::Basename;
use Time::HiRes qw(gettimeofday tv_interval);   # Not widely supported
use Cwd 'abs_path';
use Net::Domain qw(hostname hostfqdn hostdomain domainname);
#use Net::SFTP::Foreign;
#
#
@months = qw( Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec );
@days = qw(Sun Mon Tue Wed Thu Fri Sat Sun);
#
($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime();
#
$year=$year+1900;
#
# PATHS 
#
my $cwd=abs_path();
$HOME="/home/marini";
#
# Initialize
#
$version="1.0";
$awk=gawk;
#
&GetOptions("help"           => \$help,
            "m=s"            => \$material,
            "k=s"            => \$key_words,
            "c=s"            => \$create,
            "id=i"           => \$ID_in,
            "i=s"            => \$input,
            "o=s"            => \$output,
            "d=s"            => \$database,
            "a"              => \$add,
            "g"              => \$get,
            "del"            => \$del,
            "move"           => \$move,
            "u"              => \$update,
            "list"           => \$list) or die;
#
sub usage {

 print <<EndOfUsage

   Syntax: ydb.pl [OPTIONS]

         [OPTIONS] are:
                   -h                      This help
                   -m      [STRING]        Material definition
                   -k      [Key1,Key2,...] List of keywords
                   -c      [DESC]          Create it using the given DESCRIPTION
                   -id     [ID]            Run ID 
                   -i      <FILE>          Input (not needed with -g)
                   -o      <FILE,DIR>      Output file or entire directory (not needed with -g)
                   -d      <FILE,DIR>      Database file or entire directory (not needed with -g)
                   -a                      Add to run ID (to be used together with -i/-o/-d)
                   -g                      Get the run ID (to be used together with -i/-o/-d)
                   -del                    Delete the run ID
                   -move   [OD]            Move the components of run ID to OD
                   -u                      Update local database
                   -list                   List the content of the database (can be used with -k/-m/-i/-o/-d)

   [X] means X is requested
   <X> means X is optional

EndOfUsage
  ;
  exit;
}
#
# Date
#=====
my $date="$mday-$mon-$year:$hour-$min";
#
# Help
#======
if($help){ usage };
#
# Set the path
#==============
open(CONF,"<","$HOME/.ydb/configuration");
while(<CONF>) {
@words = split(' ',$_);
chomp($words);
if ("$words[0]" =~ "server"){$server=$words[1]};
if ("$words[0]" =~ "path"){$path=$words[1]};
};
close(CONF);
print "\n YdB version $version \n\n";
print   " Remote DB  \t$server:$path \n";
#
# FILES
#=======
$ACTIONS_file="$HOME/.ydb/todo";
$DB_file="$HOME/.ydb/database";
#
$n_to_remove=1;
$FILE_to_remove[1]="$ACTIONS_file";
#
# UPDATE
#========
if ($update or $create)
{
print " Local database synced with server...\n\n";
&local_cmd("get $server:$path/database $HOME/.ydb/$DB_file");
if (not $create) {exit;}
}
#
# DB
#====
$runs=0;
open(DB,"<","$DB_file");
while(<DB>) { 
 @element = split(' ',$_);
 if ($element[0] ne $runs) {
  $runs++;
  $n_keys=0;
  $n_outs=0;
  $n_dbs=0;
 }
 if ($element[1] eq "material")    { $RUN_material[$runs]=$element[2] } ;
 if ($element[1] eq "date")        { $RUN_date[$runs]=$element[2] } ;
 if ($element[1] eq "description") { $RUN_description[$runs]=$element[2] } ;
 if ($element[1] eq "key")         { 
   $n_keys++;
   $RUN_key[$runs,$n_keys]=$element[2];
 } ;
 if ($element[1] eq "output")         { 
   $n_outs++;
   $RUN_out[$runs,$n_outs]=$element[2];
 } ;
 if ($element[1] eq "input")         { 
   $n_ins++;
   $RUN_in[$runs,$n_ins]=$element[2];
 } ;
 if ($element[1] eq "database")         { 
   $n_dbs++;
   $RUN_db[$runs,$n_dbs]=$element[2];
 } ;
};
close(DB);
#
# KeyWords
#==========
if ($key_words) { 
@keys= split(/\s*,\s*/, $key_words); 
print   " Key words\t:@keys\n" ;
};
#
# LIST
#======
if ($list) { 
 print   " DB elemets\t:$runs \n\n";
 $irun=0;
 while ($irun<$runs) {
  $irun++;
  &print_the_run($irun);
 } 
 exit;
};
#
# Files
#=======
open(DB,">>","$DB_file");
open(ACTIONS,">","$ACTIONS_file");
#
# Material
#==========
if ($material) { print   " Material\t:$material \n" }
#
# ADD
#=====
if ($add and $ID_in) 
{
 $RUN_dir="$path/$RUN_material[$ID_in]/$ID_in";
 if ($input) {
  &remote_cmd("put $input $RUN_dir/input");
  &add_entry($ID_in,"input","$input");
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
#
# New ID
#========
#
my $ID = $runs+1;
#
# CREATE
#========
#
if ($create) {
 #
 #if (&have_keys() eq 1) {exit;};
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
#
# SYNC
#######
#
if ($create or ($add and $ID_in))
{
 &remote_cmd("put $DB_file $path/database");
 close(DB);
 close(ACTIONS);
 #$sftp = Net::SFTP::Foreign->new($server, timeout => 240) or $newerr = 1;
 print "\n\n Running SFTP...\n\n";
 &local_cmd("sftp -q -b $ACTIONS_file $server");
}
#
# End
#
close(DB);
#
# Delete
#
for($ik = 1; $ik <= $n_to_remove; $ik++) {
  unlink $FILE_to_remove[$ik] if exists($FILE_to_remove[$ik]);
};
unlink $ACTIONS_file;
#
print "\n\n";
#
exit;
#
sub remote_cmd 
#==============
{
foreach $command (@_) {
 print ACTIONS "$command\n";
}
#
}
sub local_cmd 
#=============
{
 foreach $command (@_) {
   $return_value = system("$command"); 
 }
}
sub print_the_run
#=================
{
 if ($material and $material ne $RUN_material[@_]){exit};
 if ($key_words and $keys[0] ne $RUN_key[@_,1]){exit};
 if ($ID_in and $ID_in ne @_){exit};
 print "\n ID\t\t:@_\n";
 print " Material\t:$RUN_material[@_]\n";
 print " Description\t:$RUN_description[@_]\n";
 print " Date\t\t:$RUN_date[@_]\n";
 print " Keys\t\t:";
 for($ik = 1; $ik < 100; $ik++) {
   print " $RUN_key[@_,$ik]" if exists($RUN_key[@_,$ik]);
 };
 print "\n";
 print " Input\t\t:";
 for($ik = 1; $ik < 100; $ik++) {
   print " $RUN_in[@_,$ik]" if exists($RUN_in[@_,$ik]);
 };
 print "\n";
 print " Outputs\t:";
 for($ik = 1; $ik < 100; $ik++) {
   print " $RUN_out[@_,$ik]" if exists($RUN_out[@_,$ik]);
 };
 print "\n";
 print " Databases\t:";
 for($ik = 1; $ik < 100; $ik++) {
   print " $RUN_db[@_,$ik]" if exists($RUN_db[@_,$ik]);
 };
 print "\n";
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
 &add_entry($ID_in,"output","$file");
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
 &add_entry($ID_in,"database","$file");
}
sub have_material
#=================
{
 $irun=0;
 while ($irun<$runs) {
  $irun++;
  if ("$RUN_material[$irun]" =~ "$material") {
   return 1;
   exit; 
  }
 } 
 return 0;
}
#sub have_keys{
# $irun=0;
# while ($irun<$runs) {
#  $irun++;
#  for($ik = 1; $ik < 100; $ik++) {
#    if exists($RUN_key[$irun,$ik]){
#      print " $RUN_key[$irun,$ik]" ;
#      my @matches = grep { /"$RUN_key[$irun,$ik]"/ } @list_of_strings;
#    }
#  };
#  #if ("$RUN_[$irun]" =~ "$material") {
#  # return 1;
#  # exit; 
# }
# return 0;
#}
sub add_entry
#=============
{
 $local_id=@_[0];
 $local_id_p_1=@_[0]+1;
 $file= $DB_file;
 $filename = basename("@_[2]");
 $old = $file;
 $new = "$file.tmp.$$";
 open(OLD, "< $old")         or die "can't open $old: $!";
 open(NEW, "> $new")         or die "can't open $new: $!";
 $ID_save="0";
 while (<OLD>) {
   @line = split(' ',$_);
   if ( $ID_save =~ "$local_id" and $line[0] =~ "$local_id_p_1" ) {
    print NEW "$local_id @_[1] $filename\n";
   };
   print NEW $_;
   $ID_save=$line[0];
 }
 if ($local_id eq $runs) {
   print NEW "$local_id @_[1] $filename\n";
 }
 close(OLD)                  or die "can't close $old: $!";
 close(NEW)                  or die "can't close $new: $!";
 rename($new, $old)          or die "can't rename $new to $old: $!";
}
