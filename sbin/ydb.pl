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
@months = qw( Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec );
@days = qw(Sun Mon Tue Wed Thu Fri Sat Sun);
#
($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime();
#
$year=$year+1900;
#
# Initialize
#
$version=1.0;
$awk=gawk;
#
&GetOptions("help"           => \$help,
            "m"              => \$material,
            "k"              => \$key_words,
            "c"              => \$create,
            "id"             => \$IDs,
            "i"              => \$input,
            "o"              => \$output,
            "d"              => \$db,
            "a"              => \$add,
            "g"              => \$get,
            "del"            => \$del,
            "move"           => \$move,
            "list"           => \$list);
#
sub usage {

 print <<EndOfUsage

   Syntax: ydb.pl [OPTIONS]

         [OPTIONS] are:
                   -h                      This help
                   -m      [STRING]        Material definition
                   -k      [WORD,WORD,...] List of keywords
                   -c                      Create it
                   -id     [ID]            Run ID 
                   -i      <FILE>          Input (not needed with -g)
                   -o      <FILE,DIR>      Output file or entire directory (not needed with -g)
                   -d      <FILE,DIR>      Database file or entire directory (not needed with -g)
                   -a                      Add to run ID (to be used together with -i/-o/-d)
                   -g                      Get the run ID (to be used together with -i/-o/-d)
                   -del                    Delete the run ID
                   -move   [OD]            Move the components of run ID to OD
                   -list                   List the content of the database (can be used with -k/-m/-i/-o/-d)

   [X] means X is requested
   <X> means X is optional

EndOfUsage
  ;
  exit;
}
#
# date
#
my $date="Date:$days[$wday]-$mday-$months[$mon]-$year Time:$hour-$min";
#
# help
#
if($help or not $input_tests){ usage };
#
# set the path
#
open(CONF,"<","~/.ydb/PATH");
close(CONF)
#
