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
  if ($ID_in and "$ID[$irun]" ne "$ID_in") {return};
  if ($IF_in and "$RUN_father[$irun]" ne "$IF_in") {return};
  if ($material and index($RUN_material[$irun],$material) eq -1 ) {return};
  if ($ID_in and $ID_in ne $ID_now){return};
  # 
  # Key section
  #
  if ($key) {
    $list_it="no";
    for($ik = 1; $ik < 100; $ik++) {
      if ($RUN_tag[$IRUN_now][$ik] and index($RUN_tag[$IRUN_now][$ik],$key) >= 0 ){$list_it="yes"};
      if ($RUN_in_tag[$IRUN_now][$ik] and index($RUN_in_tag[$IRUN_now][$ik],$key) >= 0 ){$list_it="yes"};
      if ($RUN_out_tag[$IRUN_now][$ik] and index($RUN_out_tag[$IRUN_now][$ik],$key) >= 0 ){$list_it="yes"};
      if ($RUN_db_tag[$IRUN_now][$ik] and index($RUN_db_tag[$IRUN_now][$ik],$key) >= 0 ){$list_it="yes"};
    }
    if ( $list_it =~ "no" ) {return};
  }
 }
 print "\n ID\t\t:$ID_now\n";
 print " Father\t\t:$RUN_father[$IRUN_now]\n";
 print " Material\t:$RUN_material[$IRUN_now]\n";
 print " Description\t:$RUN_description[$IRUN_now]\n";
 print " Date\t\t:$RUN_date[$IRUN_now]\n";
 print " Tags\t\t:";
 $start=" ";
 for($ik = 1; $ik < 100; $ik++) {
   if ($ik >1 ) { $start="\n\t\t  " };
   print "$start$RUN_tag[$IRUN_now][$ik]" if exists($RUN_tag[$IRUN_now][$ik]);
 };
 print "\n";
 print " Input(s)\t:";
 $start=" ";
 for($ik = 1; $ik < 100; $ik++) {
   if ($ik >1 ) { $start="\n\t\t  " };
   if ($RUN_in_tag[$IRUN_now][$ik]){
    print "$start$RUN_in[$IRUN_now][$ik] (Tag(s): $RUN_in_tag[$IRUN_now][$ik])" if exists($RUN_in[$IRUN_now][$ik]);
   }else{
    print "$start$RUN_in[$IRUN_now][$ik]" if exists($RUN_in[$IRUN_now][$ik]);
   }
 };
 print "\n";
 print " Output(s)\t:";
 $start=" ";
 for($ik = 1; $ik < 100; $ik++) {
   if ($ik >1 ) { $start="\n\t\t  " };
   if ($RUN_out_tag[$IRUN_now][$ik]){
    print "$start$RUN_out[$IRUN_now][$ik] (Tag(s): $RUN_out_tag[$IRUN_now][$ik])" if exists($RUN_out[$IRUN_now][$ik]);
   }else{
    print "$start$RUN_out[$IRUN_now][$ik]" if exists($RUN_out[$IRUN_now][$ik]);
   }
 };
 print "\n";
 print " Database(s)\t:";
 $start=" ";
 for($ik = 1; $ik < 100; $ik++) {
   if ($ik >1 ) { $start="\n\t\t  " };
   if ($RUN_db_tag[$IRUN_now][$ik]){
    print "$start$RUN_db[$IRUN_now][$ik] (Tag(s): $RUN_db_tag[$IRUN_now][$ik])" if exists($RUN_db[$IRUN_now][$ik]);
   }else{
    print "$start$RUN_db[$IRUN_now][$ik]" if exists($RUN_db[$IRUN_now][$ik]);
   }
 };
 print "\n";
}
1;
