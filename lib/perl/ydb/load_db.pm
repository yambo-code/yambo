sub load_db
{
$runs=0;
open(DB,"<","$DB_file");
while(<DB>) { 
 @element = split(' ',$_);
 if ($element[0] ne $runs) {
  $runs++;
  $n_keys=0;
  $n_outs=0;
  $n_dbs=0;
 }
 if ($element[1] eq "material")    { $RUN_material[$runs]=$element[2] } ;
 if ($element[1] eq "date")        { $RUN_date[$runs]=$element[2] } ;
 if ($element[1] eq "description") { $RUN_description[$runs]=$element[2] } ;
 if ($element[1] eq "key")         { 
   $n_keys++;
   $RUN_key[$runs,$n_keys]=$element[2];
 } ;
 if ($element[1] eq "output")         { 
   $n_outs++;
   $RUN_out[$runs,$n_outs]=$element[2];
 } ;
 if ($element[1] eq "input")         { 
   $n_ins++;
   $RUN_in[$runs,$n_ins]=$element[2];
 } ;
 if ($element[1] eq "database")         { 
   $n_dbs++;
   $RUN_db[$runs,$n_dbs]=$element[2];
 } ;
};
close(DB);
}
1;
