#!/usr/bin/perl -w
#
# Copyright (C) 2000-2008 C. Hogan and the YAMBO team 
#              http://www.yambo-code.org
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
# Usage
# --------
#
# svn log | perl sbin/svn2cl.pl > ChangeLog
#

require 5.0;
use strict;

my %hackers = (
    "marini"     => "Andrea Marini <andrea.marini\@roma2.infn.it>",
    "cdhogan"     => "Conor Hogan <conor.hogan\@roma2.infn.it>",
);

my $parse_next_line = 0;
my $last_line_empty = 0;
my $last_rev = "";

while (my $entry = <>) {

  # Axe windows style line endings, since we should try to be consistent, and
  # the repos has both styles in its log entries
  $entry =~ s/\r\n$/\n/;

  # Remove trailing whitespace
  $entry =~ s/\s+$/\n/;

  my $this_line_empty = $entry eq "\n";

  # Avoid duplicate empty lines
  next if $this_line_empty and $last_line_empty;

  # Don't fail on valid dash-only lines
  if ($entry =~ /^-+$/ and length($entry) >= 72) {

    # We're at the start of a log entry, so we need to parse the next line
    $parse_next_line = 1;

    # Check to see if the final line of the commit message was blank,
    # if not insert one
    print "\n" if $last_rev ne "" and !$last_line_empty;

  } elsif ($parse_next_line) {

    # Transform from svn style to GNU style
    $parse_next_line = 0;

    my @parts = split (/ /, $entry);
    $last_rev  = $parts[0];
    my $hacker = $parts[2];
    my $tstamp = $parts[4];
    my $time   = $parts[5];

    # Use alias if we can't resolve to name, email
    $hacker = $hackers{$hacker} if defined $hackers{$hacker};

    printf "%s %s %s %s\n", $tstamp, $time, $last_rev, $hacker;

  } elsif ($this_line_empty) {

    print "\n";

  } else {
    print "\t$entry";
  }

  $last_line_empty = $this_line_empty;
}

1;

