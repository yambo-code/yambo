#

# from http://www.arsc.edu/support/news/HPCnews/HPCnews249.shtml
#
#        Copyright (C) 2000-2022 the YAMBO team
#              http://www.yambo-code.org
#
# Authors (see AUTHORS file for details): AM, AF, DS
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
AC_DEFUN([AC_HAVE_NETCDF_F90],[

AC_ARG_WITH(netcdf_libs,AC_HELP_STRING([--with-netcdf-libs=<libs>],
            [Use NetCDF libraries <libs>],[32]))
AC_ARG_WITH(netcdf_path, AC_HELP_STRING([--with-netcdf-path=<path>],
            [Path to the NetCDF install directory],[32]),[],[])
AC_ARG_WITH(netcdf_libdir,AC_HELP_STRING([--with-netcdf-libdir=<path>],
            [Path to the NetCDF lib directory],[32]))
AC_ARG_WITH(netcdf_includedir,AC_HELP_STRING([--with-netcdf-includedir=<path>],
            [Path to the NetCDF include directory],[32]))
#
AC_ARG_WITH(netcdff_libs,AC_HELP_STRING([--with-netcdff-libs=<libs>],
            [Use NetCDFF libraries <libs>],[32]))
AC_ARG_WITH(netcdff_path, AC_HELP_STRING([--with-netcdff-path=<path>],
            [Path to the NetCDFF install directory],[32]),[],[])
AC_ARG_WITH(netcdff_libdir,AC_HELP_STRING([--with-netcdff-libdir=<path>],
            [Path to the NetCDFF lib directory],[32]))
AC_ARG_WITH(netcdff_includedir,AC_HELP_STRING([--with-netcdff-includedir=<path>],
            [Path to the NetCDFF include directory],[32]))
#
# Large Databases Support (LFS)
#
AC_ARG_ENABLE(netcdf_classic, AC_HELP_STRING([--enable-netcdf-classic],
             [Switch to OLD NetCDF classic. Default is no.]))
#
# NETCDF PAR IO
#
AC_ARG_ENABLE(netcdf_par_io,AC_HELP_STRING([--enable-netcdf-par-io],
             [Activate the NETCDF parallel io. Default is no.]))
#
# HDF5 support
#
AC_ARG_ENABLE(netcdf_v3,AC_HELP_STRING([--enable-netcdf-v3],
             [Switch to OLD NETCD v3 format. Default is no.]))
#
# HDF5 data compression
#
AC_ARG_ENABLE(hdf5_compression,AC_HELP_STRING([--enable-hdf5-compression],
             [Activate the HDF5 data compression. Default is no.]))
#
# NETCDF SHODOW FOR OUTPUT FILES
#
AC_ARG_ENABLE(netcdf_output,AC_HELP_STRING([--enable-netcdf-output],
             [Activate the netcdf copy for some output files. Default is no.]))
#
enable_hdf5="yes" ;
enable_pnetcdf="no" ;
compile_netcdf="no"
internal_netcdf="no"
def_netcdf=""
NETCDF_OPT="--enable-netcdf-4"
NETCDF_VER="v4"
IO_LIB_VER="parallel";
#
save_fcflags="$FCFLAGS" ;
hdf5_libs="$HDF5_LIBS" ;
save_libs="$LIBS" ;
#
# global options
#
if test x"$enable_netcdf_classic" = "xyes" ; then  enable_hdf5=no      ; fi
if test x"$enable_netcdf_v3"      = "xyes" ; then  enable_hdf5=no      ; fi
if test x"$enable_netcdf_par_io"  = "xyes" ; then  enable_pnetcdf=yes ; enable_hdf5=no  ; fi
if test x"$enable_hdf5_par_io"    = "xyes" ; then  enable_hdf5=yes     ; fi
#
if test x"$enable_hdf5_par_io" = "xyes"  &&  test x"$enable_netcdf_par_io" = "xyes" ; then
  AC_MSG_ERROR([Select --disable-hdf5-par-io with --enable-netcdf-par-io]) ;
fi
#    
if test x"$enable_hdf5_par_io" = "xno"    ; then IO_LIB_VER="serial"; fi
if test x"$enable_netcdf_par_io" = "xyes" ; then IO_LIB_VER="parallel"; fi
#
# Set NETCDF LIBS and FLAGS from INPUT
#
if test -d "$with_netcdf_path" || test -d "$with_netcdf_libdir" ; then
  #
  # external netcdf
  #
  if test -d "$with_netcdf_libdir" ; then  AC_MSG_CHECKING([for NetCDF in $with_netcdf_libdir]) ;
  elif test -d "$with_netcdf_path" ; then  AC_MSG_CHECKING([for NetCDF in $with_netcdf_path]) ;
  fi
  #
  if test -d "$with_netcdf_path" ; then 
      try_netcdf_libdir="$with_netcdf_path/lib" ;
      try_netcdf_incdir="$with_netcdf_path/include" ;
      try_netcdff_libdir="$with_netcdf_path/lib" ;
      try_netcdff_incdir="$with_netcdf_path/include" ;
      try_pnetcdf_libdir="$with_netcdf_path/lib" ;
      try_pnetcdf_incdir="$with_netcdf_path/include" ;
  fi
  if test -d "$with_netcdff_path" ; then 
      try_netcdff_libdir="$with_netcdff_path/lib" ;
      try_netcdff_incdir="$with_netcdff_path/include" ;
  fi
  if test -d "$with_pnetcdf_path" ; then 
      try_pnetcdf_libdir="$with_pnetcdf_path/lib" ;
      try_pnetcdf_incdir="$with_pnetcdf_path/include" ;
  fi
  #
  if test -d "$with_pnetcdf_libdir"    ; then try_pnetcdf_libdir="$with_pnetcdf_libdir" ; fi
  if test -d "$with_netcdf_libdir"     ; then try_netcdf_libdir="$with_netcdf_libdir" ; fi
  if test -d "$with_netcdf_includedir" ; then try_netcdf_incdir="$with_netcdf_includedir" ; fi
  #
  if test -d "$with_pnetcdf_libdir"     ; then try_pnetcdf_libdir="$with_pnetcdf_libdir" ; fi
  if test -d "$with_netcdff_libdir"     ; then try_netcdff_libdir="$with_netcdff_libdir" ; fi
  if test -d "$with_netcdff_includedir" ; then try_netcdff_incdir="$with_netcdff_includedir" ; fi
  #
  if test -z "$try_netcdf_libdir" ; then AC_MSG_ERROR([No lib-dir specified]) ; fi
  if test -z "$try_netcdf_incdir" ; then AC_MSG_ERROR([No include-dir specified]) ; fi
  #
  AC_LANG([Fortran])
  #
  if test -d "$try_pnetcdf_incdir" ; then
    try_PNETCDF_INCS="$IFLAG$try_pnetcdf_incdir" ;
  fi
  try_NETCDF_INCS="$IFLAG$try_netcdf_incdir" ;
  if test -d "$try_netcdff_incdir" ; then
    try_NETCDFF_INCS="$IFLAG$try_netcdff_incdir" ;
  fi
  #
  if test x"$enable_pnetcdf" = "xyes"; then
    if test -r $try_pnetcdf_libdir/libpnetcdf.a ; then
      try_PNETCDF_LIBS="-L$try_pnetcdf_libdir -lpnetcdf" ;
    elif test -r $try_netcdf_libdir/libpnetcdf.a ; then
      try_PNETCDF_LIBS="-L$try_netcdf_libdir -lpnetcdf" ;
    fi
  fi
  try_NETCDF_LIBS="-L$try_netcdf_libdir -lnetcdf" ;
  if test -r $try_netcdff_libdir/libnetcdff.a ; then
    try_NETCDFF_LIBS="-L$try_netcdff_libdir -lnetcdff" ;
  elif test -r $try_netcdf_libdir/libnetcdff.a ; then
    try_NETCDFF_LIBS="-L$try_netcdf_libdir -lnetcdff" ;
  fi
  #
elif test x"$with_netcdf_libs" != "x" ; then
  #
  # directly provided lib
  #
  AC_MSG_CHECKING([for NetCDF Library using $with_netcdf_libs])
  if test -d "$with_netcdf_includedir" ; then  try_NETCDF_INCS="$IFLAG$with_netcdf_includedir" ; fi
  if test -d "$with_netcdff_includedir" ; then try_NETCDFF_INCS="$IFLAG$with_netcdff_includedir" ; fi
  if test -d "$with_pnetcdf_includedir" ; then try_PNETCDF_INCS="$IFLAG$with_pnetcdf_includedir" ; fi
  netcdf="yes";
  try_NETCDF_LIBS="$with_netcdf_libs" ;
  try_NETCDFF_LIBS="$with_netcdff_libs" ;
  try_PNETCDF_LIBS="$with_pnetcdf_libs" ;
  AC_MSG_RESULT(yes)
  #
fi
#
# TEST NETCDF LIBS and FLAGS
#
if test x"$enable_hdf5" = "xno"; then
  #
  netcdf=no;
  #
  if test -d "$with_netcdf_path" || test -d "$with_netcdf_libdir" || test x"$with_netcdf_libs" != "x"; then
    #
    FCFLAGS="$try_NETCDFF_INCS $try_NETCDF_INCS $try_PNETCDF_INCS $save_fcflags";
    LIBS="$try_NETCDFF_LIBS $try_NETCDF_LIBS $try_PNETCDF_LIBS $save_libs";
    #
    AC_COMPILE_IFELSE(AC_LANG_PROGRAM([], [
      use netcdf
      implicit none
      integer nf_err
      integer ID
      nf_err = nf90_create('netcdf_test',nf90_share,ID)]),
      [netcdf=yes], [netcdf=no]);
    #
    if test "x$netcdf" = "xyes"; then
      AC_MSG_RESULT([yes]) ;
      NETCDF_INCS="$try_NETCDF_INCS" ;
      NETCDF_LIBS="$try_NETCDF_LIBS" ;
      NETCDFF_INCS="$try_NETCDFF_INCS" ;
      NETCDFF_LIBS="$try_NETCDFF_LIBS" ;
      PNETCDF_INCS="$try_PNETCDF_INCS" ;
      PNETCDF_LIBS="$try_PNETCDF_LIBS" ;
    else
      AC_MSG_RESULT([no]) ;
    fi
    # 
    FCFLAGS="$save_fcflags" ;
    LIBS="$save_libs" ;
    #
  fi
  if test "x$netcdf" = "xno"; then
    #
    # internal netcdf
    #
    AC_MSG_CHECKING([for internal NetCDF library])
    #
    internal_netcdf="yes"
    #
    if test x"$enable_pnetcdf" = "xyes"; then
      NETCDF_OPT="--enable-pnetcdf --disable-netcdf-4 --enable-cdf5"
      NETCDF_VER="v3"
      IO_LIB_VER="parallel";
    else
      NETCDF_OPT="--disable-netcdf-4"
      NETCDF_VER="v3"
      IO_LIB_VER="serial";
    fi
    # 
    # the following may change if we use a different version
    # of the netcdf lib
    #
    #
    NETCDF_HDF5_PATH="${extlibs_path}/${FCKIND}/${FC}/${NETCDF_VER}/${IO_LIB_VER}"
    #
    NETCDF_LIBS="${NETCDF_HDF5_PATH}/lib/libnetcdf.a" ;
    NETCDF_INCS="${IFLAG}${NETCDF_HDF5_PATH}/include" ;
    NETCDFF_LIBS="${NETCDF_HDF5_PATH}/lib/libnetcdff.a" ;
    NETCDFF_INCS="${IFLAG}${NETCDF_HDF5_PATH}/include" ;
    PNETCDF_LIBS="${NETCDF_HDF5_PATH}/lib/libpnetcdf.a -L${NETCDF_HDF5_PATH}/lib" ;
    PNETCDF_INCS="${IFLAG}${NETCDF_HDF5_PATH}/include" ;
    #
    if test "$use_libm"    = "yes"; then NETCDF_LIBS="$NETCDF_LIBS -lm"   ; fi
    if test "$use_libcurl" = "yes"; then NETCDF_LIBS="$NETCDF_LIBS -lcurl"; fi
    #
    netcdf=yes
    if test x"$enable_pnecdf" = "xyes"; then
      if test -e "${NETCDF_HDF5_PATH}/lib/libnetcdf.a" && test -e "${NETCDF_HDF5_PATH}/lib/libnetcdff.a" && test -e "${NETCDF_HDF5_PATH}/lib/libpnetcdf.a"; then
        compile_netcdf="no" ;
        AC_MSG_RESULT([already compiled]) ;
      else 
        compile_netcdf="yes" ;
        AC_MSG_RESULT([to be compiled]) ;
      fi
    else
      if test -e "${NETCDF_HDF5_PATH}/lib/libnetcdf.a" && test -e "${NETCDF_HDF5_PATH}/lib/libnetcdff.a"; then
        compile_netcdf="no" ;
        AC_MSG_RESULT([already compiled]) ;
      else 
        compile_netcdf="yes" ;
        AC_MSG_RESULT([to be compiled]) ;
      fi
    fi
    #
  fi
  #
fi
#
#
# HDF5 support
#
if test x"$enable_hdf5" = "xyes"; then
  #
  AC_LANG([Fortran])       
  #
  # checking for NETDCF
  #
  FCFLAGS="$try_NETCDFF_INCS $try_NETCDF_INCS $save_fcflags" ;
  LIBS="$try_NETCDFF_LIBS $try_NETCDF_LIBS $hdf5_libs" ;
  #
  AC_LINK_IFELSE(AC_LANG_PROGRAM([], [
    use netcdf
    implicit none
    integer nf_err
    integer ID
    nf_err = nf90_create('netcdf_test',nf90_share,ID)
    ]),[netcdf=yes], [netcdf=no]);
  #
  if test "x$netcdf" = "xyes"; then
    NETCDF_LIBS="$try_NETCDF_LIBS" ;
    NETCDF_INCS="$try_NETCDF_INCS" ;
    NETCDFF_LIBS="$try_NETCDFF_LIBS" ;
    NETCDFF_INCS="$try_NETCDFF_INCS" ;
    AC_MSG_RESULT([yes]) ;
    #
  fi
  #
  FCFLAGS="$save_fcflags" ;
  LIBS="$save_libs" ;
  #
  if test "x$netcdf" = "xno"; then
    if test -d "$with_netcdf_libdir" || test -d "$with_netcdf_path" || test -d "$with_netcdff_libdir" || test -d "$with_netcdff_path" ; then AC_MSG_RESULT([no]) ; fi
    #
    AC_MSG_CHECKING([for internal NETCDF library]);
    internal_netcdf="yes" ;
    #
    NETCDF_OPT="--enable-netcdf-4";
    NETCDF_VER="v4";
    #
    NETCDF_HDF5_PATH="${extlibs_path}/${FCKIND}/${FC}/${NETCDF_VER}/${IO_LIB_VER}" ;
    NETCDF_HDF5_PAR_PATH="${extlibs_path}/${FCKIND}/${FC}/${NETCDF_VER}/parallel" ;
    #
    NETCDF_LIBS="${NETCDF_HDF5_PATH}/lib/libnetcdf.a" ;
    NETCDF_INCS="${IFLAG}${NETCDF_HDF5_PATH}/include" ;
    NETCDFF_LIBS="${NETCDF_HDF5_PATH}/lib/libnetcdff.a" ;
    NETCDFF_INCS="${IFLAG}${NETCDF_HDF5_PATH}/include" ;
    #
    netcdf=yes ;
    #
    if test -e "${NETCDF_HDF5_PATH}/lib/libnetcdf.a" && test -e "${NETCDF_HDF5_PATH}/lib/libnetcdff.a" ; then
      #
      compile_netcdf="no" ;
      AC_MSG_RESULT([already compiled]) ;
      #
    elif test "$IO_LIB_VER" = "serial" && test -e "${NETCDF_HDF5_PAR_PATH}/lib/libnetcdf.a" && test -e "${NETCDF_HDF5_PAR_PATH}/lib/libnetcdff.a" && test -e "${NETCDF_HDF5_PAR_PATH}/lib/libhdf5.a"; then
      #
      compile_netcdf="no" ;
      IO_LIB_VER="parallel";
      NETCDF_LIBS="${NETCDF_HDF5_PAR_PATH}/lib/libnetcdf.a" ;
      NETCDF_INCS="${IFLAG}${NETCDF_HDF5_PAR_PATH}/include" ;
      NETCDFF_LIBS="${NETCDF_HDF5_PAR_PATH}/lib/libnetcdff.a" ;
      NETCDFF_INCS="${IFLAG}${NETCDF_HDF5_PAR_PATH}/include" ;
      AC_MSG_RESULT([already compiled (using parallel version)]) ;
      #
    else
      #  
      compile_netcdf="yes";
      #
      if test "$IO_LIB_VER" = "serial";   then HDF5_OPT="--disable-parallel" ; fi
      if test "$IO_LIB_VER" = "parallel"; then HDF5_OPT="--enable-parallel"  ; fi
      #
      AC_MSG_RESULT([to be compiled]) ;
      #
    fi
    #
  fi
fi
#
# NETCDF-HDF5 LIBS
#
if test x"$hdf5" = "xyes"; then
  def_netcdf="-D_HDF5_LIB";
  #if test "$IO_LIB_VER" = "serial" || test "$IO_LIB_VER" = "unknown" ; then
  #  def_netcdf="-D_HDF5_LIB";
  #elif test "$IO_LIB_VER" = "parallel"; then
  #  def_netcdf="-D_HDF5_PARLIB";
  #fi
fi
#
# Enable netcdf for output files support
#
if test x"$enable_netcdf_output" = "xyes"; then
  def_netcdf="${def_netcdf} -D_NC_OUTPUT";
fi
#
if test x"$enable_netcdf_classic" = "xyes" ; then
  #
  # Disable large File Support
  #
  def_netcdf="${def_netcdf} -D_NC_CLASSIC";
  #
elif test x"$netcdf" = "xyes" && test x"$hdf5" = "xyes" ; then
  #
  # NETCDF-HDF5 IO
  #
  def_netcdf="${def_netcdf} -D_HDF5_IO";
  #
fi
#
AC_SUBST(NETCDF_LIBS)
AC_SUBST(NETCDF_INCS)
AC_SUBST(NETCDF_OPT)
AC_SUBST(NETCDF_VER)
AC_SUBST(NETCDFF_LIBS)
AC_SUBST(NETCDFF_INCS)
AC_SUBST(PNETCDF_LIBS)
AC_SUBST(PNETCDF_INCS)
AC_SUBST(IO_LIB_VER)
AC_SUBST(netcdf)
AC_SUBST(def_netcdf)
AC_SUBST(compile_netcdf)
AC_SUBST(internal_netcdf)
AC_SUBST(enable_netcdf_classic)
AC_SUBST(enable_netcdf_v3)
AC_SUBST(enable_netcdf_par_io)

])
