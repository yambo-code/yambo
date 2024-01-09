#
# License-Identifier: GPL
#
# Copyright (C) 2016 The Yambo Team
#
# Authors (see AUTHORS file for details): AM
#
AC_DEFUN([ACX_STRIPE_SUBPATH],
[
TMP1=`echo $1 | sed 's/\//+/g'`
TMP2=`echo $extlibs_path/${FCKIND}/${FC} | sed 's/\//+/g'`
TMP3=`echo $TMP1 | sed "s/$TMP2/\(LIB\)/g"`
STRIPE=`echo $TMP3 | sed 's/+/\//g'`
if [[ -z "${1// }" ]]; then
 STRIPE="$STRIPE ($2)"
fi
])
