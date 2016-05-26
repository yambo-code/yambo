#
sub have_material
#=================
{
 $irun=0;
 while ($irun<$runs) {
  $irun++;
  if ("$RUN_material[$irun]" =~ "$material") {
   return 1;
   exit; 
  }
 } 
 return 0;
}
#sub have_keys{
# $irun=0;
# while ($irun<$runs) {
#  $irun++;
#  for($ik = 1; $ik < 100; $ik++) {
#    if exists($RUN_key[$irun,$ik]){
#      print " $RUN_key[$irun,$ik]" ;
#      my @matches = grep { /"$RUN_key[$irun,$ik]"/ } @list_of_strings;
#    }
#  };
#  #if ("$RUN_[$irun]" =~ "$material") {
#  # return 1;
#  # exit; 
# }
# return 0;
#}
1;
