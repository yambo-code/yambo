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
