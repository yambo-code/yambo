#!/usr/bin/perl
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
# Based on the driver.pl written by Conor and (only partially) me
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
use print_the_run;
use database;
use load_db;
use objects_IO;
use create_new_run;
use functions;
use remove_run;
use prompt;
#
# Initialize
$version="3.0";
$awk=gawk;
local $| = 1;
#
# CMD line
&GetOptions("help"           => \$help,
            "setup"          => \$setup,
            "m=s"            => \$material,
            "k=s"            => \$key,
            "desc"           => \$description,
            "id=i"           => \$ID_in,
            "if=i"           => \$IF_in,
            "i=s"            => \$input,
            "o=s"            => \$output,
            "d=s"            => \$database,
            "a"              => \$add,
            "c"              => \$create,
            "g=s"            => \$get,
            "del"            => \$del,
            "move"           => \$move,
            "u"              => \$update,
            "q"              => \$quiet,
            "b"              => \$rebuild,
            "r=s"            => \$running,
            "s"              => \$see,
            "v+"             => \$verb,
            "t=s"            => \$user_tags,
            "list"           => \$list) or die;
sub usage {

 print <<EndOfUsage

   Syntax: ydb.pl <ARGS>
           < > are variable parameters, [ ] are optional, | indicates choice

   where <ARGS> must include at least one of:
                   -h                      This help

   (general)            
                   -setup                  Initial setup
                   -id     [ID]            Run ID (father)
                   -ic     [ID]            Run ID (child)

   (actions)
                   -a                      Add to run ID (to be used together with -i/-o/-d)
                   -c                      Create the RUN
                   -del                    Delete the run ID
                   -g                      Get the run ID components (to be used together with -i/-o/-d)
                   -move   [OD]            Move the components of run ID to OD
                   -u                      Update local database
                   -b                      Rebuild the database from the remote files list
                   -s                      View
                   -list                   List the content of the database (can be used with -k/-m/-i/-o/-d)

   (new/edit entry )
                   -m      [STRING]        Material definition
                   -desc                   Edit run description
                   -i      [FILE]          Input (not needed with -g) (also use all with -del)
                   -o      [FILE or DIR]   Output file/entire directory (not needed with -g) (also use all with -del)
                   -d      [FILE or DIR]   Database file/entire directory (not needed with -g) (also use all with -del)
                   -r      [STRING]        Running informations

    (list & select)
                   -k      [key]           Key used to select specific groups of files
                   -t      [tag1,tag2,...] User tags
                   -v [-v]                 Verbose output (use -v -v for extra verbosity)
                   -q                      Quiet run (just list the actions)

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
$date="$mday-$mon-$year:$hour-$min";
#
# Help
if($help){ usage };
#
# Setup
if ($setup){
 $return_value = system("mkdir -p ~/.ydb");
 $conf_file="$HOME/.ydb/configuration";
 if (!-e $conf_file) {
  $return_value = system("echo 'server <IP>' > $conf_file");
  $return_value = system("echo 'path <PATH>' >> $conf_file");
  print "\n Add to your shell file the variable PERLLIB <where the perl ydb routines are>\n\n";
 }
 $return_value = system("vim $conf_file");
 exit;
}
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
$ACTIONS_sftp_file="$HOME/.ydb/todo_sftp";
$ACTIONS_ssh_file="$HOME/.ydb/todo_ssh";
$DB_file="$HOME/.ydb/database";
$n_to_remove=2;
$FILE_to_remove[1]="$ACTIONS_ssh_file";
$FILE_to_remove[2]="$ACTIONS_sftp_file";
#
# DB REBUILD
if ($rebuild)
{
# &local_cmd("ssh $server 'find $path > file_list'");
# &local_cmd("scp -qp $server:file_list $HOME/.ydb/file_list");
# &local_cmd("ssh $server 'rm -f file_list'");
 die "\n";
}
#
# DB UPDATE
if ($update or $create)
{
print " Local database synced with server...\n\n";
&local_cmd("scp -qp $server:$path/database $DB_file");
if (not $create) {exit;}
}
# Load DB components
&load_db();
#
# Input report
if ($user_tags) { 
 @tags= split(/\s*,\s*/, $user_tags); 
 print   " Tags\t\t:@tags\n" ;
};
if ($input)       {print   " Input      \t:$input \n"};
if ($output)      {print   " Output(s) \t:$output \n"};
if ($database)    {print " Database(s) \t:$database \n"};
#
# IRUN_in
if ($ID_in) { 
 $id_father=$ID_in;
 if ($IF_in) {$id_father=$IF_in};
 $IRUN_in=&have_run("ID",$ID_in);
}
#
# LIST
if ($list) { 
 &list_all_runs;
 exit;
};
#
# Files (II)
open(DB,">>","$DB_file");
open(ACTIONS_sftp,">","$ACTIONS_sftp_file");
open(ACTIONS_ssh,">","$ACTIONS_ssh_file");
#
# Material
if ($material) { print   " Material\t:$material \n" }
#
# ADD
if ($add and $ID_in) { &add_command_line_object } ;
#
# Remove
if ($del and $ID_in) { &remove_run } ;
#
# Get it
$test=&have_ID($ID_in);
if (($get or $see) and $ID_in and &have_ID($ID_in)==1) { 
 $local_dir="$cwd/$get";
 if ($see) {$local_dir="tmp"};
 &local_cmd("mkdir -p $local_dir");
 if ($input)    {&local_cmd("mkdir -p $local_dir/inputs")};
 if ($output)   {&local_cmd("mkdir -p $local_dir/outputs")};
 if ($database) {&local_cmd("mkdir -p $local_dir/databases")};
 &get_the_run;
} ;
#
# CREATE
if ($create) { 
 if (!$material and !$ID_in and !$IF_in) {die " A material must be provided\n"};
 &create_new_run();
};
#
# UPDATE
if ($ID_in){
 if ($quiet) {print "\n Changes in the database:\n\n"};
 if ($description) {&add_a_database_line($ID_in,"description")};
 if ($material) {&add_a_database_line($ID_in,"material","$material")};
 if ($user_tags) {&add_a_database_line($ID_in,"tag","@tags")};
 if ($running) {&add_a_database_line($ID_in,"running","$running")};
 if ($IF_in) {&add_a_database_line($ID_in,"father","$IF_in")};
 # update the date
 &add_a_database_line($ID_in,"date","$date");
}
#
# SYNC
if ( ($create or ($ID_in and !$get) or $del or $get ) and !$quiet )
{
 if (!$get) {&remote_sftp_cmd("put $DB_file $path/database")};
 close(DB);
 close(ACTIONS_sftp);
 close(ACTIONS_ssh);
 print "\n\n Running SFTP...\n\n";
 &local_cmd("sftp -b $ACTIONS_sftp_file $server");
 &local_cmd("ssh $server 'bash -s' < $ACTIONS_ssh_file");
}
#
# End
close(DB);
#
# Delete Files
if ($quiet) { 
 print "\n\n ACTIONS to be done\n\n";
 &local_cmd("cat $ACTIONS_sftp_file");
 &local_cmd("cat $ACTIONS_ssh_file");
};
for($ik = 1; $ik <= $n_to_remove; $ik++) {
  unlink $FILE_to_remove[$ik] if exists($FILE_to_remove[$ik]);
  if (-d $FILE_to_remove[$ik]) {rmdir $FILE_to_remove[$ik]};
};
#
# Unzip/Dump local files
#
if ($get and ($output or $database)) {&local_uncompress};
#
print "\n\n";
#
exit;
#
sub remote_sftp_cmd 
#===================
{
foreach $command (@_) {
 print ACTIONS_sftp "$command\n";
 }
}
#
sub remote_ssh_cmd 
#===================
{
foreach $command (@_) {
 print ACTIONS_ssh "$command\n";
 }
}
sub local_cmd 
#=============
{
 foreach $command (@_) {
   $return_value = system("$command"); 
 }
}
