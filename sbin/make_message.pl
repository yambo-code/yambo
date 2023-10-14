#!/usr/bin/perl -w
#
# License-Identifier: GPL
#
# Copyright (C) 2006 The Yambo Team
#
# Authors (see AUTHORS file for details): CH
#
# Wonderful script to generate a commit message template
#
# Usage: In the YAMBO root directory
#
#  sbin/make_message.pl    or
#  sbin/make_message.pl -p="Myrta"   if a patch was supplied by e.g. Myrta
#
use Getopt::Long;
&GetOptions("p=s"          => \$patchname);
#
# Collect the files marked by git status
# 
# ... modified
#
$gitcommand = "git status | sed -n '/not staged/q;p' | grep 'modified' | awk \'{printf\" %s \",\$2 }\'|";
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
open(VER,"<","include/version/version.m4");
while($line = <VER>) {
  chomp $line;
  my @VERS = split /"/, $line;
  if ( $line =~ /SVERSION=/ ) {$SV = $VERS[1];};
  if ( $line =~ /SSUBVERSION=/ ) {$SS = $VERS[1]};
  if ( $line =~ /SPATCHLEVEL=/ ) {$SP = $VERS[1]};
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
