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
 open(OLD, "< $old")         or die "can't open $old: $!";
 open(NEW, "> $new")         or die "can't open $new: $!";
 $ID_save="0";
 while (<OLD>) {
   @line = split(' ',$_);
   if ( $ID_save =~ "$local_id" and $line[0] =~ "$local_id_p_1" ) {
    print NEW "$local_id @_[1] $filename\n";
   };
   print NEW $_;
   $ID_save=$line[0];
 }
 if ($local_id eq $runs) {
   print NEW "$local_id @_[1] $filename\n";
 }
 close(OLD)                  or die "can't close $old: $!";
 close(NEW)                  or die "can't close $new: $!";
 rename($new, $old)          or die "can't rename $new to $old: $!";
}
1;
