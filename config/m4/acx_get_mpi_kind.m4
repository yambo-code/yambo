#
# License-Identifier: GPL
#
# Copyright (C) 2016 The Yambo Team
#
# Authors (see AUTHORS file for details): AM
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
