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
sub print_the_run
#=================
{
 my $IRUN_now=@_[0];
 my $ID_now=$ID[$IRUN_now];
 if ($list) {
  if ($material and $material ne $RUN_material[$IRUN_now]){return};
  if ($key_words and $keys[0] ne $RUN_key[$IRUN_now,1]){return};
  if ($ID_in and $ID_in ne $ID_now){return};
 }
 print "\n ID\t\t:$ID_now\n";
 print " Material\t:$RUN_material[$IRUN_now]\n";
 print " Description\t:$RUN_description[$IRUN_now]\n";
 print " Date\t\t:$RUN_date[$IRUN_now]\n";
 print " Keys\t\t:";
 $start=" ";
 for($ik = 1; $ik < 100; $ik++) {
   if ($ik >1 ) { $start="\n\t\t  " };
   print "$start$RUN_key[$IRUN_now][$ik]" if exists($RUN_key[$IRUN_now][$ik]);
 };
 print "\n";
 print " Input(s)\t:";
 $start=" ";
 for($ik = 1; $ik < 100; $ik++) {
   if ($ik >1 ) { $start="\n\t\t  " };
   print "$start$RUN_in[$IRUN_now][$ik]" if exists($RUN_in[$IRUN_now][$ik]);
 };
 print "\n";
 print " Output(s)\t:";
 $start=" ";
 for($ik = 1; $ik < 100; $ik++) {
   if ($ik >1 ) { $start="\n\t\t  " };
   print "$start$RUN_out[$IRUN_now][$ik]" if exists($RUN_out[$IRUN_now][$ik]);
 };
 print "\n";
 print " Database(s)\t:";
 $start=" ";
 for($ik = 1; $ik < 100; $ik++) {
   if ($ik >1 ) { $start="\n\t\t  " };
   print "$start$RUN_db[$IRUN_now][$ik]" if exists($RUN_db[$IRUN_now][$ik]);
 };
 print "\n";
}
1;
