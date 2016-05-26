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
sub remove_run{
 #
 print "\n\n Removing RUN $ID_in...";
 #
 for($if = 1; $if < 100; $if++) {
   if (exists($RUN_out[$ID_in][$if])){
    &remote_cmd("rm $path/$RUN_material[$ID_in]/$ID_in/output/$RUN_out[$ID_in][$if].gz");
   }
   if (exists($RUN_in[$ID_in][$if])){
    &remote_cmd("rm $path/$RUN_material[$ID_in]/$ID_in/input/$RUN_in[$ID_in][$if]");
   }
   if (exists($RUN_db[$ID_in][$if])){
    &remote_cmd("rm $path/$RUN_material[$ID_in]/$ID_in/database/$RUN_db[$ID_in][$if].nc.gz");
   }
 };
 &remote_cmd("rm $path/$RUN_material[$ID_in]/$ID_in/description");
 &remote_cmd("rmdir $path/$RUN_material[$ID_in]/$ID_in/input");
 &remote_cmd("rmdir $path/$RUN_material[$ID_in]/$ID_in/output");
 &remote_cmd("rmdir $path/$RUN_material[$ID_in]/$ID_in/databases");
 &remote_cmd("rmdir $path/$RUN_material[$ID_in]/$ID_in");
}
1;
