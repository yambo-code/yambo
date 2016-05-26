sub create_new_run{
 #
 # New ID
 $ID = $runs+1;
 #
 #if (&have_keys() eq 1) {exit;};
 #
 print "\n\n Creating RUN...";
 #
 #############################
 print "\n ID\t\t:$ID\n";
 if (&have_material() eq 0) {&remote_cmd("mkdir $path/$material")};
 &remote_cmd("mkdir $path/$material/$ID");
 &remote_cmd("mkdir $path/$material/$ID/input");
 &remote_cmd("mkdir $path/$material/$ID/output");
 &remote_cmd("mkdir $path/$material/$ID/databases");
 $RUN_dir="$path/$material/$ID";
 $database_line[0]="$ID material $material";
 $database_line[1]="$ID date $date";
 $database_line[2]="$ID description $create";
 $ic=2;
 foreach $key (@keys) {
  $ic++;
  $database_line[$ic]="$ID key $key";
 };
 $local_description_file="$HOME/.ydb/${ID}_description";
 open(LOCAL_DESC,'>',$local_description_file) or die;
 foreach $line (@database_line) {
  print DB "$line\n";
 }
 print LOCAL_DESC "$create";
 close(LOCAL_DESC);
 &remote_cmd("put $local_description_file $RUN_dir/description");
 $n_to_remove++;
 $FILE_to_remove[$n_to_remove]="$local_description_file";
}
1;
