#!/usr/bin/perl

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

$srcdir = shift;
$top_builddir = shift;

$builddir = "$top_builddir/src";

my @funcs = ("lda", "gga", "hyb_gga", "mgga", "hyb_mgga");

$s0 = ""; $s3 = ""; $s4 = ""; $s5 = "";
foreach $func (@funcs){
  undef %deflist_f;
  undef %deflist_c;

  read_file($srcdir, $func);

  $s1 = ""; $s2 = "";
  foreach $key (sort { $a <=> $b } keys %deflist_f) {
    $s0 .= sprintf "%s %-20s %3s  /*%-60s*/\n", "#define ",
      $deflist_f{$key}, $key, $deflist_c{$key};

    $t = $deflist_f{$key};
    $t =~ s/XC_(.*)/\L$1/;

    $s4 .= ",\n" if($s4);
    $s4 .= sprintf "{\"%s\", %d}", $t, $key;

    $s3 .= sprintf "  %s %-20s = %3s  ! %s\n", "integer, parameter ::",
      $deflist_f{$key}, $key, $deflist_c{$key};

    $s1 .= "extern XC(func_info_type) XC(func_info_$t);\n";
    $s2 .= "  &XC(func_info_$t),\n";
  }

  open(OUT, ">$builddir/funcs_$func.c");
  print OUT <<EOF
#include "util.h"

$s1

const XC(func_info_type) *XC(${func}_known_funct)[] = {
$s2  NULL
};
EOF
    ;
  close OUT;
}

  open(OUT, ">$builddir/funcs_key.c");
print OUT <<EOF
#include "util.h"

XC(functional_key_t) XC(functional_keys)[] = {
$s4,
{"", -1}
};
EOF
  ;

open(OUT, ">$builddir/xc_funcs.h");
print OUT $s0;
print $so;
close OUT;

open(OUT, ">$builddir/libxc_funcs.f90");
print OUT <<EOF
module libxc_funcs_m
  implicit none

  public

$s3
end module libxc_funcs_m
EOF
  ;
close OUT;

sub read_file() {
  my ($dir, $type) = @_;
  $type =~ s/(.*)/\L$1/;

  my $TYPE = $type;
  $TYPE =~ s/(.*)/\U$1/;

  # we remove the hyb from the filenames
  $type =~ s/^hyb_//;

  opendir(DIR, "$dir/") || die "cannot opendir '$dir': $!";
  while($_ = readdir(DIR)){
    next if(!/^${type}_.*\.c$/ && !/^hyb_${type}_.*\.c$/ );

    open(IN, "<$dir/$_");
    while($_=<IN>){
      if(/#define\s+(XC_${TYPE}_\S+)\s+(\S+)\s+\/\*(.*)\*\//){
	$deflist_f{$2} = $1;
	$deflist_c{$2} = $3;
      }
    }
    close(IN);
  }
  closedir DIR;
}
