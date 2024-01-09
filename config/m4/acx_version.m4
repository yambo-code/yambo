#
# License-Identifier: GPL
#
# Copyright (C) 2016 The Yambo Team
#
# Authors (see AUTHORS file for details): AM
#
AC_DEFUN([ACX_VERSION],
[
cat << EOF > include/version/version.h
#define YAMBO_VERSION $SVERSION
#define YAMBO_SUBVERSION $SSUBVERSION
#define YAMBO_PATCHLEVEL $SPATCHLEVEL
#define YAMBO_REVISION $SREVISION
#define YAMBO_HASH "$SHASH" 
EOF
])
