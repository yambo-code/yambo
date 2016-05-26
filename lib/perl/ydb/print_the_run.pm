sub print_the_run
#=================
{
 if ($material and $material ne $RUN_material[@_]){exit};
 if ($key_words and $keys[0] ne $RUN_key[@_,1]){exit};
 if ($ID_in and $ID_in ne @_){exit};
 print "\n ID\t\t:@_\n";
 print " Material\t:$RUN_material[@_]\n";
 print " Description\t:$RUN_description[@_]\n";
 print " Date\t\t:$RUN_date[@_]\n";
 print " Keys\t\t:";
 for($ik = 1; $ik < 100; $ik++) {
   print " $RUN_key[@_,$ik]" if exists($RUN_key[@_,$ik]);
 };
 print "\n";
 print " Input\t\t:";
 for($ik = 1; $ik < 100; $ik++) {
   print " $RUN_in[@_,$ik]" if exists($RUN_in[@_,$ik]);
 };
 print "\n";
 print " Outputs\t:";
 for($ik = 1; $ik < 100; $ik++) {
   print " $RUN_out[@_,$ik]" if exists($RUN_out[@_,$ik]);
 };
 print "\n";
 print " Databases\t:";
 for($ik = 1; $ik < 100; $ik++) {
   print " $RUN_db[@_,$ik]" if exists($RUN_db[@_,$ik]);
 };
 print "\n";
}
1;
