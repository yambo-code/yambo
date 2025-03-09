#
# License-Identifier: GPL
#
# Copyright (C) 2025 The Yambo Team
#
# Authors (see AUTHORS file for details): CA
#
AC_DEFUN([AC_HAVE_YIOHDF5],[

AC_ARG_ENABLE(yio_hdf5, AS_HELP_STRING([--enable-yio-hdf5],[Activate Yio with HDF5 without NetCDF. Default is no],[]))

yio_hdf5="no"

if test x"$enable_yio_hdf5" = "xyes"; then yio_hdf5=yes ; fi

if test "x$yio_hdf5" = "xyes" ; then
  #
  YIO_DIR="YioHDF5"
  AC_MSG_CHECKING([for Yio HDF5 library])
  AC_MSG_RESULT([yes])
  def_yio="-D_YIO_HDF5";
else
  YIO_DIR="YioNetCDF"
  AC_MSG_CHECKING([for Yio HDF5 library])
  AC_MSG_RESULT([no])
  def_yio="-D_YIO_NETCDF";
fi

AC_SUBST(yio_hdf5)
AC_SUBST(def_yio)
AC_SUBST(YIO_DIR)
#
])
