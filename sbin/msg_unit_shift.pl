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

@units= ('fs','cc','iku','ev','meV','eV','cm-3','A','a.u.','o/o','nAmpere','Bohr_magneton','c.c.','Ha/Ry','kV/cm','au','ev/ThZ','iru  /  cc(a.u.)','1/eV');

open($fout, '>', $file."_tmp") ;
open($fin, '<', $file) ;
while (my $row = <$fin>) {
 chomp $row;
 undef $ok;
 if ($row =~ /msg/ and $row =~ /call/) { $ok=1 };
 if (not $ok) {print $fout $row."\n";next};
 undef $ok;
 foreach $u (@units) {  
  $u_new=$u;
  if ("$u" eq "ev") {$u_new="eV"};
  if ("$u" eq "cc") {$u_new="c.c."};
  if ("$u" eq "au") {$u_new="a.u."};
  if ($row =~ /\[$u\]/) { 
   $ok=1;
   $msg=(split("\'",$row))[-2];
   if ("$msg" eq "\[".$u."\]") {
     undef $ok;
   }else{
    $space=" "x(length($u)+2);
    $row =~ s/\[$u\]/$space/g;
    $row =~ s|(.+)\)|$1,"\[$u_new\]"\)|;
   }
  }
 }
 if (not $ok) {print $fout $row."\n";next};
 print $fout $row."\n";
}
close($fin);
close($fout);
system("meld $file $file"."_tmp");
print "\n[$file] Is Ok?";
chomp ($yn = <>);
if ("$yn" eq "y") {system("mv $file"."_tmp $file")};
if ("$yn" eq "n") {system("rm $file"."_tmp")};

