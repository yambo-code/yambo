#
#        Copyright (C) 2000-2018 the YAMBO team
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
AC_DEFUN([ACX_GET_MPI_KIND],
[
cat > conftest.c << EOF_
#include <stdio.h>
#include <stdlib.h>
#include "mpi.h"
int main()
{
 int len;
 char *version=NULL;
 version = malloc(MPI_MAX_LIBRARY_VERSION_STRING+1);
 MPI_Get_library_version(version, &len);
 printf("%s",version);
 return 0;
}
EOF_
(eval $MPICC -o conftest.x conftest.c >& conftest.err )
if test -e "conftest.x"; then
  MPIKIND=`./conftest.x|head -n 1`
else
  MPIKIND="undefined"
fi
AC_MSG_CHECKING([for MPI version])
AC_MSG_RESULT([$MPIKIND])
AC_SUBST(MPIKIND)
])
