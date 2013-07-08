#!/usr/bin/env perl

# Copyright (C) 2006-2007 M.A.L. Marques
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#  
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.
#  
# You should have received a copy of the GNU Lesser General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

use Getopt::Std;

getopts("hf:s:b:");
$opt_h && usage();
$opt_f || usage();

# Handle options
$top_srcdir   = ($opt_s ? $opt_s : "..");
$top_builddir = ($opt_b ? $opt_b : "..");
$opt_f        =~ s/(.*)/\L$1\E/;

my $tmp_file =  "/tmp/xc.tmp.$$";
my $exec_cmd = "$top_builddir/testsuite/xc-get_data";

# start by reading xc.h to get a list of the defined constants
my %constants;
read_xc_h(\%constants);
$constants{"$opt_f"} || die "Functional '$opt_f' not found";

# check if we have a data file
my $data_file = "$top_srcdir/testsuite/df_repo/$opt_f.data";
(-f $data_file && -r $data_file) || die "Could not read data file '$data_file'";
open DATA, "<$data_file";

my %data, $test_ok, $ntest;
$test_ok = 0;
$ntest   = 0;

while(data_read(*DATA, \%data) != 0){
  my $mpol, @cmp;

  $ntest++;

  $mpol = ($data{"rhoa"}    == $data{"rhob"}    &&
	   $data{"sigmaaa"} == $data{"sigmabb"} &&
	   $data{"sigmaab"} == $data{"sigmabb"}) ? 1 : 2;
  $mpol = 2;

  my $ok;
  for($pol=2;$pol>=$mpol; $pol--){
    $cmd1  = "$exec_cmd ".$constants{"$opt_f"};
    $cmd2  = " ".$data{"rhoa"}." ".$data{"rhob"};
    $cmd2 .= " ".$data{"sigmaaa"}." ".$data{"sigmaab"}." ".$data{"sigmabb"};
    `$cmd1 $pol $cmd2 >$tmp_file`;

    open DATA2, "<$tmp_file";
    my %data2;
    data_read(*DATA2, \%data2) || die "Could not read data file '$tmp_file'";
    close DATA2;

    @cmp = ("zk", "vrhoa", "vsigmaaa");
    if($data2{"v2rhoa2"} != 0.0){
      push @cmp, ("v2rhoa2", "v2rhoasigmaaa", "v2sigmaaa2");
    }

    if($pol == 1){
      my $tmp = $data{"vsigmaaa"};
      $data{"vsigmaaa"}  = ($data{"vsigmaaa"} + $data{"vsigmaab"} + $data{"vsigmabb"})/4.0;

      if($data2{"v2rhoa2"} != 0.0){
	#print $data{"v2rhoa2"}, "\n";
	$data{"v2rhoa2"} = ($data{"v2rhoa2"} + $data{"v2rhoab"} + $data{"v2rhobb"})/2.0;
	#print $data{"v2rhoa2"}, "\n";
      }

      $ok = cmp_data(\%data, \%data2, \@cmp);

      $data{"vsigmaaa"} = $tmp;

    }else{
      if($data{"rhob"} != 0.0){
	# compare both up and down channels
	push @cmp, ("vrhob", "vsigmaab", "vsigmabb");
	if($data2{"v2rhoa2"} != 0.0){
	  push @cmp, ("v2rhoab", "v2rhob2",
		      "v2rhoasigmaab", "v2rhoasigmabb", "v2rhobsigmaaa", "v2rhobsigmaab", "v2rhobsigmabb",
		      "v2sigmaaaab", "v2sigmaaabb", "v2sigmaab2", "v2sigmaabbb", "v2sigmabb2");
	}
      }

      $ok = cmp_data(\%data, \%data2, \@cmp);
    }
  }
  $ok && $test_ok++;
}
close DATA;
unlink $tmp_file;

exit ($ntest - $test_ok);
 

###########################################
sub usage {
  print <<EndOfUsage;

 Copyright (C) 2006 by M.A.L. Marques

Usage: $0 [options] -f functional

    -h    This help message
    -f    Functional to test
    -b    The top level build tree directory, ../ if omitted
    -s    The top level source tree directory, ../ if omitted

Report bugs to <marques\@tddft.org>.
EndOfUsage
  exit 0;
}


###########################################
sub read_xc_h {
  my $c = shift;

  open FILE, "<$top_builddir/src/xc_funcs.h";
  while($_ = <FILE>){
    if(/^#define +(\S*) +(\S*)/){
      my $name = $1;
      my $value = $2;

      $name =~ s/^XC_(.*)/\L$1\E/;
      $$c{$name} = $value;
    }
  }
  close FILE;
}

###########################################
sub data_read {
  my ($FILE, $data) = @_;

  while( ($line = <$FILE>) && !($line =~ /rhoa/) ){}
  $line || return 0;

  $line =~ / rhoa= (\S*) rhob= (\S*) sigmaaa= (\S*) sigmaab= (\S*) sigmabb= (\S*)/;
  $$data{"rhoa"} = $1;
  $$data{"rhob"} = $2;
  $$data{"sigmaaa"} = $3;
  $$data{"sigmaab"} = $4;
  $$data{"sigmabb"} = $5;

  my $n = 0;
  while($n++ < 24){
    $line = <$FILE> || return 0;
    $line =~ /\s*(\S*)\s*=\s*(\S*)/;
    $$data{$1} = $2;
  }
  return 1;
}

sub cmp_data {
  my ($d1, $d2, $what) = @_;
  my $tol = 1e-10, $all_ok;

  $all_ok = 1;
  foreach $var (@$what){
    $ok = (abs($$d1{$var}) < 1e-15 && abs($$d2{$var}) < 1e-15);
    if(!$ok){
      $ok = (abs($$d1{$var} - $$d2{$var}) <= $tol*abs($$d1{$var}));
    }
    if(!$ok){
      print "$var mismatch: ", $$d1{$var}, " != ", $$d2{$var}, "\n";
    }
    $all_ok = $all_ok && $ok;
  }
  return $all_ok;
}
