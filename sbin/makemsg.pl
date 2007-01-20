#!/usr/bin/perl -w
#
# Wonderful script to generate a commit message template (CDH)
# Usage: In the SELF root directory
# sbin/makemsg    or
# sbin/makemsg -p="Myrta"   if a patch was supplied by e.g. Myrta
#
use Getopt::Long;
&GetOptions("p=s"          => \$patchname);
#
# Collect the files marked by svn status
#
$svncommand = "svn status | grep -v ? | awk \'{printf\" %s \",\$2 }\' |";
open(SVN, $svncommand);
$changedfiles = <SVN>;
close(SVN);
# Strip the "src/" (add to this list if needed)
$changedfiles =~ s/  / /g;
$changedfiles =~ s/src\///g;
$changedfiles =~ s/interfaces\///g;
print "Identified modified files: $changedfiles\n";
#
# Grab the version number
#
$svncommand = "ls 3.0* |";
open(SVN, $svncommand);
$version = <SVN>;
close(SVN);
#
# Write the commit message
#
open(MSGFILE,">","commit.msg") or die "The file commit.msg could " .
   "not be opened.\n";
print MSGFILE "V $version \n";
print MSGFILE "* $changedfiles\n\n";
print MSGFILE "Bugs:\n";
print MSGFILE "- \n";
print MSGFILE "Additions:\n";
print MSGFILE "- \n";
print MSGFILE "Changes:\n";
print MSGFILE "- \n";
if($patchname) {print MSGFILE "Patch sent by: $patchname\n"};
close(MSGFILE);
print "Edit the commit.msg file and commit/patch with -F commit.msg \n";
