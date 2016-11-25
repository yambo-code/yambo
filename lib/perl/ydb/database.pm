#
#        Copyright (C) 2000-2016 the YAMBO team
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
sub add_a_database_line
#========================
{
 $local_id=@_[0];
 $local_id_p_1=@_[0]+1;
 $file= $DB_file;
 $filename = basename("@_[2]");
 $old = $file;
 $new = "$file.tmp.$$";
 $new_line = "$local_id @_[1] $filename";
 open(OLD, "< $old")         or die "can't open $old: $!";
 open(NEW, "> $new")         or die "can't open $new: $!";
 $added="0";
 while (<OLD>) {
  @line = split(' ',$_);
  if ( $line[0] =~ "$local_id" and "@line" =~ "$new_line"){
   print "NEW @line $new_line\n";
   print NEW "$new_line\n";
   $added++;
  } elsif ( $line[0] =~ "$local_id" and "$line[1]" =~ "description" and "@_[1]" =~ "description"){
   print NEW "$new_line\n";
   $added++;
  } elsif ( $line[0] =~ "$local_id" and "$line[1]" =~ "key" and "@_[1]" =~ "key"){
   print NEW "$new_line\n";
   $added++;
  } elsif ( $added == 0 and $line[0] =~ "$local_id_p_1") {
   print NEW "$new_line\n";
   print NEW $_;
   $added++;
  } else {
   print NEW $_;
  }
 }
 print "$new_line $added\n";
 if ( $added == 0) {print NEW "$new_line\n"};
 close(OLD)                  or die "can't close $old: $!";
 close(NEW)                  or die "can't close $new: $!";
 rename($new, $old)          or die "can't rename $new to $old: $!";
}
sub delete_database_entry
#========================
{
 $local_id=@_[0];
 $which = @_[1];
 $what  = @_[2];
 $file  = $DB_file;
 $old   = $file;
 $new   = "$file.tmp.$$";
 open(OLD, "< $old")         or die "can't open $old: $!";
 open(NEW, "> $new")         or die "can't open $new: $!";
 while (<OLD>) {
  @line = split(' ',$_);
  if ( $line[0] =~ "$local_id") 
  {
   if ($what =~ "all") { next };
   if ($which =~ "input" and $line[1] =~ "input") { next };
   if ($which =~ "output" and $line[1] =~ "output") { next };
   if ($which =~ "database" and $line[1] =~ "database") { next };
  }else{
   print NEW $_ ;
  } 
 }
 close(OLD)                  or die "can't close $old: $!";
 close(NEW)                  or die "can't close $new: $!";
 if (!$quiet) {rename($new, $old)          or die "can't rename $new to $old: $!"};
}
1;
