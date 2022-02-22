#!/usr/bin/perl
#
use Getopt::Long;
use File::Find;
use File::Copy;
use Term::ANSIColor qw(:constants);
use File::Basename;
use Time::HiRes qw(gettimeofday tv_interval);   # Not widely supported
use Cwd 'abs_path';
use Net::Domain qw(hostname hostfqdn hostdomain domainname);

#
# CMD line
&GetOptions("help"     => \$help,
            "f=s"      => \$file
) or die;

sub usage {

 print <<EndOfUsage

   Syntax: msg_unit_shift.pl [ARGS]

   where [ARGS] must include at least one of:
                   -h               This help
                   -f  <file>       File

EndOfUsage
  ;
  exit;
}

if (not $file) {usage};

undef $start;
undef $end;
open($fin, '<', $file) ;
while (my $row = <$fin>) {
 chomp $row;
 $line=$row;
 undef $title;
 if ($row =~ /^[a-zA-Z]/) {
   $char=substr($row, 0, 3);
   if ( substr($row, 0, 3) eq "end" ) 
   { 
    $end=1;
    undef($start);
   }else{
    $start=1;
    undef($end);
    $row =~ s/\(/ /g;
    $row =~ s/\)/ /g;
    if ($row =~ /subroutine/) {$title=(split(/\s+/,(split("subroutine",$row))[1]))[1] };
    if ($row =~ /function/) {$title=(split(/\s+/,(split("function",$row))[1]))[1] };
    open($fout, '>', $title.".F") ;
    for (@HEADER){print  $fout "$_\n"};
   };
   if ($start) {print "START: $title\n"};
   if ($end) {
    print "END: $title\n";
    undef($start);
    print $fout $line."\n";
    close($fout);
   };
 }
 if (not $start and not $end) {push(@HEADER, $row)};
 if ($start) {print $fout $line."\n"};
}
close($fin);

