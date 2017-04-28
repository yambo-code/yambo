#
#        Copyright (C) 2000-2017 the YAMBO team
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
sub list_all_runs
#=================
{
print   " DB elemets\t:$runs \n";
 my $irun=0;
 while ($irun<$runs) { 
  $irun++;
  $RUN_printed[$irun]="no";
 } 
 $i_fat=0;
 while ($i_fat<$N_fathers){
  $i_fat++;
  $irun=&have_run("ID",$father[$i_fat]);
  #print "FATHER is $irun\n";
  &print_the_run($irun,$father[$i_fat],0);
  while ($irun<$runs){
   $irun++;
   if ($RUN_father[$irun] eq $father[$i_fat]) {
    #print "CHILD(1) of $father[$i_fat] \n";
    &print_the_run($irun,$father[$i_fat],1);
    my $irun2=$irun;
    while ($irun2<$runs) {
     $irun2++;
     if ($RUN_father[$irun2] eq $irun) {
      #print "CHILD(2) of $irun \n";
      &print_the_run($irun2,$irun,2);
      my $irun3=$irun2;
      while ($irun3<$runs) {
       $irun3++;
       if ($RUN_father[$irun3] eq $irun2) {
        #print "CHILD(3) of $irun2 \n";
        &print_the_run($irun3,$irun2,3);
        my $irun4=$irun3;
        while ($irun4<$runs) {
         $irun4++;
         if ($RUN_father[$irun4] eq $irun3) {
          #print "CHILD(4) of $irun3 \n";
          &print_the_run($irun4,$irun3,4);
          my $irun5=$irun4;
          while ($irun5<$runs) {
           $irun5++;
           if ($RUN_father[$irun5] eq $irun4) {
            #print "CHILD(5) of $irun4 \n";
            &print_the_run($irun5,$irun4,5);
            my $irun6=$irun5;
            while ($irun6<$runs) {
             $irun6++;
             if ($RUN_father[$irun6] eq $irun5) {
              #print "CHILD(6) of $irun6 \n";
              &print_the_run($irun6,$irun5,6);
              my $irun7=$irun6;
              while ($irun7<$runs) {
               $irun7++;
               if ($RUN_father[$irun7] eq $irun6) {
                #print "CHILD(3) of $irun6 \n";
                &print_the_run($irun7,$irun6,7);
               }
              }
             }
            }
           }
          }
         }
        }
       }
      }
     }
    }
   }
  } 
 }
}
sub loop_the_runs_given_father
#=============================
{
 my $i_fat=@_[0];
 my $irun=0;
 while ($irun<$runs) {
  $irun++;
  &print_the_run($irun,@_[0]);
 }
}
sub print_the_run
#=================
{
 my $IRUN_now=@_[0];
 my $ID_now=$ID[$IRUN_now];
 #
 my $field_length=12;
 $right_free_space= (" " x ($field_length+1));
 #
 if ($list) {
  #
  my $IFATHER_now=@_[1];
  $tab_now=@_[2];
  #
  if ("$RUN_father[$IRUN_now]" ne "$IFATHER_now") {return};
  #
  if ($IF_in and "$RUN_father[$IRUN_now]" ne "$IF_in" and $ID_now ne $IF_in ) {return};
  if ($ID_in and $ID_in ne $ID_now){return};
  if ($material and index($RUN_material[$IRUN_now],$material) eq -1 ) {return};
  #
  if ("$RUN_printed[$IRUN_now]" eq "yes") {return};
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
  $RUN_printed[$IRUN_now]="yes";
  #
 }else{
  $tab_now=0;
 }
 #
 if ($tab_now eq 0 ){ 
  $space=" ";
 }else{
  $space= " ".(" " x (3*($tab_now-1))).".->";
 }
 #
 if ($tab_now eq 0) {print "\n"};
 print $space."ID($ID_now)\n";
 #
 if ($tab_now eq 0 ){ 
  $space=" |";
 }else{
  $space= " ".(" " x (3*$tab_now))."|";
 }
 #
 if ($tab_now eq 0) { 
  $right_space= (" " x ($field_length-length("Material")));
  print $space."Material".$right_space.":$RUN_material[$IRUN_now]\n";
 }
 #
 if ($RUN_description[$IRUN_now][1]) {
  $right_space= (" " x ($field_length-length("Description")));
  print $space."Description".$right_space.":";
  $start="";
  for($ik = 1; $ik < 100; $ik++) {
    if ($ik >1 ) { $start="\n".$space.$right_free_space };
    print "$start$RUN_description[$IRUN_now][$ik]" if exists($RUN_description[$IRUN_now][$ik]);
  };
  print "\n";
 }
 $right_space= (" " x ($field_length-length("Date")));
 print $space."Date".$right_space.":$RUN_date[$IRUN_now]\n";
 #
 $right_space= (" " x ($field_length-length("Running")));
 if ($RUN_run[$IRUN_now]) {print $space."Running".$right_space.":$RUN_run[$IRUN_now]\n"};
 #
 if ($RUN_tag[$IRUN_now][1]) {
  $right_space= (" " x ($field_length-length("Tags")));
  print $space."Tags".$right_space.":";
  $start="";
  for($ik = 1; $ik < 100; $ik++) {
    if ($ik >1 ) { $start="\n".$space.$right_free_space };
    print "$start$RUN_tag[$IRUN_now][$ik]" if exists($RUN_tag[$IRUN_now][$ik]);
  };
  print "\n";
 }
 if ($RUN_in[$IRUN_now][1] and $verb eq 1) {
  $right_space= (" " x ($field_length-length("Input(s)")));
  print $space."Input(s)".$right_space.":";
  $start="";
  for($ik = 1; $ik < 100; $ik++) {
    if ($ik >1 ) { $start="\n".$space.$right_free_space };
    if ($RUN_in_tag[$IRUN_now][$ik]){
     print "$start$RUN_in[$IRUN_now][$ik] (Tag(s): $RUN_in_tag[$IRUN_now][$ik])" if exists($RUN_in[$IRUN_now][$ik]);
    }else{
     print "$start$RUN_in[$IRUN_now][$ik]" if exists($RUN_in[$IRUN_now][$ik]);
    }
  };
  print "\n";
 }
 if ($RUN_out[$IRUN_now][1] and $verb eq 1) {
  $right_space= (" " x ($field_length-length("Output(s)")));
  print $space."Output(s)".$right_space.":";
  $start="";
  for($ik = 1; $ik < 100; $ik++) {
    if ($ik >1 ) { $start="\n".$space.$right_free_space };
    if ($RUN_out_tag[$IRUN_now][$ik]){
     print "$start$RUN_out[$IRUN_now][$ik] (Tag(s): $RUN_out_tag[$IRUN_now][$ik])" if exists($RUN_out[$IRUN_now][$ik]);
    }else{
     print "$start$RUN_out[$IRUN_now][$ik]" if exists($RUN_out[$IRUN_now][$ik]);
    }
  };
 print "\n";
 }
 if ($RUN_db[$IRUN_now][1] and $verb eq 1) {
  $right_space= (" " x ($field_length-length("Database(s)")));
  print $space."Database(s)".$right_space.":";
  $start=" ";
  for($ik = 1; $ik < 100; $ik++) {
    if ($ik >1 ) { $start="\n".$space.$right_free_space };
    if ($RUN_db_tag[$IRUN_now][$ik]){
     print "$start$RUN_db[$IRUN_now][$ik] (Tag(s): $RUN_db_tag[$IRUN_now][$ik])" if exists($RUN_db[$IRUN_now][$ik]);
    }else{
     print "$start$RUN_db[$IRUN_now][$ik]" if exists($RUN_db[$IRUN_now][$ik]);
    }
  };
  print "\n";
 }
}
1;
