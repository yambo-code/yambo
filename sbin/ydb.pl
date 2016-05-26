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
#
# LIBS 
use lib "/home/marini/Yambo/sources/git/yambo/branches/devel-daddy/lib/perl/ydb";
use print_the_run;
use add_a_database_line;
use load_db;
use objects_add_and_remove;
use create_new_run;
use functions;
#
# Initialize
$version="1.0";
$awk=gawk;
#
# CMD line
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
# PATHS
my $cwd=abs_path();
$HOME="$ENV{HOME}";
#
# Date
@months = qw( Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec );
@days = qw(Sun Mon Tue Wed Thu Fri Sat Sun);
($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime();
$year=$year+1900;
my $date="$mday-$mon-$year:$hour-$min";
#
# Help
if($help){ usage };
#
# DB conf
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
# Files (I)
$ACTIONS_file="$HOME/.ydb/todo";
$DB_file="$HOME/.ydb/database";
$n_to_remove=1;
$FILE_to_remove[1]="$ACTIONS_file";
#
# DB UPDATE
if ($update or $create)
{
print " Local database synced with server...\n\n";
&local_cmd("get $server:$path/database $HOME/.ydb/$DB_file");
if (not $create) {exit;}
}
# Load DB components
&load_db();
#
# KeyWords
if ($key_words) { 
@keys= split(/\s*,\s*/, $key_words); 
print   " Key words\t:@keys\n" ;
};
#
# LIST
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
# Files (II)
open(DB,">>","$DB_file");
open(ACTIONS,">","$ACTIONS_file");
#
# Material
if ($material) { print   " Material\t:$material \n" }
#
# ADD
if ($add and $ID_in) { &add_command_line_object } ;
#
# CREATE
if ($create) { &create_new_run() };
#
# SYNC
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
close(DB);
#
# Delete
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
