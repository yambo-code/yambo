#!/usr/bin/perl -w
#
#        Copyright (C) 2000-2019 the YAMBO team
#              http://www.yambo-code.org
#
# Authors (see AUTHORS file for details): CH
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
# Wonderful script to generate a commit message template (CDH)
# Usage: In the YAMBO root directory
#
# sbin/make_message.pl    or
# sbin/make_message.pl -p="Myrta"   if a patch was supplied by e.g. Myrta
#
use Getopt::Long;
&GetOptions("p=s"          => \$patchname);
#
# Collect the files marked by git status
# 
# ... modified
#
$gitcommand = "git status | sed -n '/not staged/q;p' | grep 'modified' | awk \'{printf\" %s \",\$2 }\' |";
open(GIT, $gitcommand);
$changedfiles = <GIT>;
close(GIT);
# Strip the "src/" (add to this list if needed)
if ($changedfiles) {
$changedfiles =~ s/  / /g;
$changedfiles =~ s/src\///g;
$changedfiles =~ s/interfaces\///g;
print "\nMODIFIED files: $changedfiles\n\n";
}
#
# ... added 
#
$gitcommand = "git status | grep 'new' | awk \'{printf\" %s \",\$3 }\' |";
open(GIT, $gitcommand);
$newfiles = <GIT>;
close(GIT);
# Strip the "src/" (add to this list if needed)
if ($newfiles) {
$newfiles =~ s/  / /g;
$newfiles =~ s/src\///g;
$newfiles =~ s/interfaces\///g;
print "NEW files: $newfiles\n\n";
}
#
# ... deleted 
#
$gitcommand = "git status | grep 'deleted' | awk \'{printf\" %s \",\$2 }\' |";
open(GIT, $gitcommand);
$delfiles = <GIT>;
close(GIT);
# Strip the "src/" (add to this list if needed)
if ($delfiles) {
$delfiles =~ s/  / /g;
$delfiles =~ s/src\///g;
$delfiles =~ s/interfaces\///g;
print "DELETED files: $delfiles\n\n";
};
#
# ... renamed
#
$gitcommand = "git status | grep 'renamed' | awk \'{printf\" %s \",\$2\" \"\$3\" \"\$4 }\' |";
open(GIT, $gitcommand);
$rinamefiles = <GIT>;
close(GIT);
# Strip the "src/" (add to this list if needed)
if ($rinamefiles) {
print "$rinamefiles\n\n";
$rinamefiles =~ s/  / /g;
$rinamefiles =~ s/src\///g;
$rinamefiles =~ s/interfaces\///g;
print "RENAMED files: $rinamefiles\n\n";
};
#
# Versions
#
open(VER,"<","include/version.inc");
while($line = <VER>) {
  chomp $line;
  $ID  = substr $line, 13, 1;
  if ( "$ID" =~ "1" ) {$SV = substr $line, 16, 1};
  if ( "$ID" =~ "2" ) {$SS = substr $line, 16, 1};
  if ( "$ID" =~ "3" ) {$SP = substr $line, 16, 1; last;};
}
close(VER);
$Revision=`git rev-list  --count HEAD`;
$Revision++;
$Hash    =`git rev-parse --short HEAD`;
$Revision+=10000 ;
#
# Write the commit message
#
open(MSGFILE,">","commit.msg") or die "The file commit.msg could " . "not be opened.\n";
print MSGFILE "Version $SV.$SS.$SP,  Revision ${Revision},  Hash ${Hash} \n";
if ($changedfiles) {
  print MSGFILE "MODIFIED * $changedfiles\n\n";
};
if ($newfiles) {
  print MSGFILE "NEW * $newfiles\n\n";
};
if ($delfiles) {
 print MSGFILE "DELETED * $delfiles\n\n";
};
if ($rinamefiles) {
 print MSGFILE "RENAMED * $rinamefiles\n\n";
};
print MSGFILE "Bugs:\n";
print MSGFILE "- \n\n";
print MSGFILE "Additions:\n";
print MSGFILE "- \n\n";
print MSGFILE "Changes:\n";
print MSGFILE "- \n\n";
if($patchname) {print MSGFILE "Patch sent by: $patchname\n"};
close(MSGFILE);
