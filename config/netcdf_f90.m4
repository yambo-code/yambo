#

# from http://www.arsc.edu/support/news/HPCnews/HPCnews249.shtml
#
#        Copyright (C) 2000-2019 the YAMBO team
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
AC_ARG_WITH(hdf5_libs,AC_HELP_STRING([--with-hdf5-libs=<libs>],
            [Use HDF5 libraries <libs>],[32]))
AC_ARG_WITH(hdf5_path, AC_HELP_STRING([--with-hdf5-path=<path>],
            [Path to the HDF5 install directory],[32]),[],[])
AC_ARG_WITH(hdf5_libdir,AC_HELP_STRING([--with-hdf5-libdir=<path>],
            [Path to the HDF5 lib directory],[32]))
AC_ARG_WITH(hdf5_includedir,AC_HELP_STRING([--with-hdf5-includedir=<path>],
            [Path to the HDF5 include directory],[32]))
#
# Large Databases Support (LFS)
#
AC_ARG_ENABLE(netcdf-classic, AC_HELP_STRING([--enable-netcdf-classic],
             [Switch to OLD NetCDF classic. Default is no.]))
#
# HDF5 support
#
AC_ARG_ENABLE(netcdf_hdf5,AC_HELP_STRING([--enable-netcdf-hdf5],
             [Activate the HDF5 support. Default is no.]))
#
#
# HDF5 data compression
#
AC_ARG_ENABLE(hdf5_compression,AC_HELP_STRING([--enable-hdf5-compression],
             [Activate the HDF5 data compression. Default is no.]))
#
#
# HDF5 PAR IO
#
AC_ARG_ENABLE(hdf5_par_io,AC_HELP_STRING([--enable-hdf5-par-io],
             [Activate the HDF5 parallel io. Default is no.]))
#
# HDF5 FOR P2Y (also requires parallel HDF5)
#
AC_ARG_ENABLE(hdf5_p2y_support, AC_HELP_STRING([--enable-hdf5-p2y-support],
 [Activate HDF5 support in p2y. Default is no unless parallel HDF5 libs are linked.]))
#
enable_netcdf="no"
enable_hdf5="no"
compile_netcdf="no"
internal_netcdf="no"
compile_hdf5="no"
internal_hdf5="no"
def_netcdf=""
NETCDF_OPT="--disable-netcdf-4"
NETCDF_VER="v3"
HDF5_OPT="--disable-parallel";
HDF5_VER="serial";
#
# Other libs
#
AC_LANG_PUSH(C)
AC_CHECK_LIB(z ,   deflate,      [use_libz="yes";   ],[use_libz="no";   ],[])
AC_CHECK_LIB(sz,   deflate,      [use_libsz="yes";  ],[use_libsz="no";  ],[])
AC_CHECK_LIB(dl,   dlopen,       [use_libdl="yes";  ],[use_libdl="no";  ],[])
AC_CHECK_LIB(curl, curl_version, [use_libcurl="yes";],[use_libcurl="no";],[])
AC_CHECK_LIB(m,    cos,          [use_libm="yes";   ],[use_libm="no";   ],[])
AC_LANG_POP(C)
#
# global options
#
if test -d "$with_hdf5_libdir"          ; then enable_hdf5=yes ; fi
if test -d "$with_hdf5_path"            ; then enable_hdf5=yes ; fi
if test x"$with_hdf5_libs" != "x"       ; then enable_hdf5=yes ; fi
if test x"$enable_netcdf_hdf5" = "xyes" ; then enable_hdf5=yes ; fi
#
if test x"$enable_hdf5_p2y_support" = "xyes" ; then enable_hdf5=yes ; fi
#
if test x"$enable_hdf5_par_io" = "xyes" ; then
  enable_netcdf_hdf5=yes ;
  enable_hdf5=yes ;
fi
#    
if test x"$enable_hdf5_par_io" = "xyes" || test x"$enable_hdf5_p2y_support" = "xyes" ; then HDF5_VER="parallel"; fi
#
#
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
  fi
  if test -d "$with_netcdff_path" ; then 
      try_netcdff_libdir="$with_netcdff_path/lib" ;
      try_netcdff_incdir="$with_netcdff_path/include" ;
  fi
  #
  if test -d "$with_netcdf_libdir"     ; then try_netcdf_libdir="$with_netcdf_libdir" ; fi
  if test -d "$with_netcdf_includedir" ; then try_netcdf_incdir="$with_netcdf_includedir" ; fi
  #
  if test -d "$with_netcdff_libdir"     ; then try_netcdff_libdir="$with_netcdff_libdir" ; fi
  if test -d "$with_netcdff_includedir" ; then try_netcdff_incdir="$with_netcdff_includedir" ; fi
  #
  if test -z "$try_netcdf_libdir" ; then AC_MSG_ERROR([No lib-dir specified]) ; fi
  if test -z "$try_netcdf_incdir" ; then AC_MSG_ERROR([No include-dir specified]) ; fi
  #
  AC_LANG([Fortran])
  #
  try_NETCDF_INCS="$IFLAG$try_netcdf_incdir" ;
  if test -d "$try_netcdff_incdir" ; then
    try_NETCDFF_INCS="$IFLAG$try_netcdff_incdir" ;
  fi
  #
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
  netcdf="yes";
  try_NETCDF_LIBS="$with_netcdf_libs" ;
  try_NETCDFF_LIBS="$with_netcdff_libs" ;
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
    save_fcflags="$FCFLAGS" ;
    save_libs="$LIBS" ;
    #
    FCFLAGS="$try_NETCDFF_INCS $try_NETCDF_INCS $save_fcflags";
    LIBS="$try_NETCDFF_LIBS $try_NETCDF_LIBS $save_libs";
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
    # the following may change if we use a different version
    # of the netcdf lib
    #
    #
    NETCDF_HDF5_PATH="${extlibs_path}/${FCKIND}/${FC}/${NETCDF_VER}/${HDF5_VER}"
    #
    NETCDF_LIBS="${NETCDF_HDF5_PATH}/lib/libnetcdf.a" ;
    NETCDF_INCS="${IFLAG}${NETCDF_HDF5_PATH}/include" ;
    NETCDFF_LIBS="${NETCDF_HDF5_PATH}/lib/libnetcdff.a" ;
    NETCDFF_INCS="${IFLAG}${NETCDF_HDF5_PATH}/include" ;
    #
    if test "$use_libm"    = "yes"; then NETCDF_LIBS="$NETCDF_LIBS -lm"   ; fi
    if test "$use_libcurl" = "yes"; then NETCDF_LIBS="$NETCDF_LIBS -lcurl"; fi
    #
    netcdf=yes
    if test -e "${NETCDF_HDF5_PATH}/lib/libnetcdf.a" && test -e "${NETCDF_HDF5_PATH}/lib/libnetcdff.a"; then
      compile_netcdf="no" ;
      AC_MSG_RESULT([already compiled]) ;
    else 
      compile_netcdf="yes" ;
      AC_MSG_RESULT([to be compiled]) ;
    fi
    #
  fi
  #
fi
#
#
# HDF5 support
#
hdf5="no"
#
if test x"$enable_hdf5" = "xyes"; then
  #
  if   test -d "$with_hdf5_libdir"    ; then AC_MSG_CHECKING([for HDF5 in $with_hdf5_libdir]) ;
  elif test -d "$with_hdf5_path"    ;   then AC_MSG_CHECKING([for HDF5 in $with_hdf5_path]) ;
  elif test x"$with_hdf5_libs" != "x" ; then AC_MSG_CHECKING([for HDF5 using $with_hdf5_libs]) ;
  fi
  #
  AC_LANG([Fortran])       
  #
  # re-define lib and inc dirs
  #
  if test -d "$with_hdf5_path" ; then 
      try_hdf5_libdir=$with_hdf5_path/lib
      try_hdf5_incdir=$with_hdf5_path/include
  fi
  if test -d "$with_hdf5_libdir"     ; then try_hdf5_libdir=$with_hdf5_libdir ; fi
  if test -d "$with_hdf5_includedir" ; then try_hdf5_incdir=$with_hdf5_includedir ; fi
  #
  if test x"$with_hdf5_libs" != "x" ; then try_HDF5_LIBS="$with_hdf5_libs" ; fi
  #
  if test -d "$try_hdf5_libdir" ; then try_HDF5_LIBS="-L$try_hdf5_libdir -lhdf5hl_fortran -lhdf5_fortran -lhdf5_hl -lhdf5" ; fi
  #
  if test -d "$try_hdf5_incdir" ; then try_HDF5_INCS="$IFLAG$try_hdf5_incdir" ; fi
  #
  save_libs="$LIBS" ;
  save_fcflags="$FCFLAGS" ;
  #
  FCFLAGS="$try_NETCDFF_INCS $try_NETCDF_INCS $try_HDF5_INCS $save_fcflags" ;
  #
  if test "$use_libz"    = "yes"; then try_HDF5_LIBS="$try_HDF5_LIBS -lz"   ; fi
  if test "$use_libsz"   = "yes"; then try_HDF5_LIBS="$try_HDF5_LIBS -lsz"  ; fi
  if test "$use_libm"    = "yes"; then try_HDF5_LIBS="$try_HDF5_LIBS -lm"   ; fi
  if test "$use_libdl"   = "yes"; then try_HDF5_LIBS="$try_HDF5_LIBS -ldl"  ; fi
  if test "$use_libcurl" = "yes"; then try_HDF5_LIBS="$try_HDF5_LIBS -lcurl"; fi
  #
  LIBS="$try_HDF5_LIBS"
  #
  #if test "$HDF5_VER" = "serial" ; then
    AC_LINK_IFELSE(AC_LANG_PROGRAM([], [
       use hdf5
       use netcdf
       implicit none
       integer  error
       error = NF90_HDF5
       call h5open_f(error)
       call h5close_f(error)
       ]),[hdf5=yes], [hdf5=no]);
  #fi;
  ##
  #AC_LINK_IFELSE(AC_LANG_PROGRAM([], [
  #   use hdf5
  #   use netcdf
  #   implicit none
  #   integer  error
  #   error = NF90_HDF5
  #   call h5open_f(error)
  #   call h5close_f(error)
  #   ]),[hdf5_par=yes], [hdf5_par=no]);
  ##
  #if test "$HDF5_VER" = "parallel" ; then hdf5="$hdf5_par" ; fi
  #if test "$HDF5_VER" = "serial" ; then
  #  if test "x$hdf5_par" = "xyes" ; then HDF5_VER="parallel" ; fi
  #fi;
  #
  netcdf=$hdf5;
  if test "x$hdf5" = xyes; then
    HDF5_LIBS="$try_HDF5_LIBS" ;
    HDF5_INCS="$try_HDF5_INCS" ;
    NETCDF_LIBS="$try_NETCDF_LIBS" ;
    NETCDF_INCS="$try_NETCDF_INCS" ;
    NETCDFF_LIBS="$try_NETCDFF_LIBS" ;
    NETCDFF_INCS="$try_NETCDFF_INCS" ;
    #if test $HDF5_VER = "parallel"; then AC_MSG_RESULT([yes - parallel lib found]) ; fi
    #if test $HDF5_VER = "serial";   then AC_MSG_RESULT([yes - serial lib found]) ; fi
    AC_MSG_RESULT([yes]) ;
    HDF5_VER="unknown"
  fi
  #
  FCFLAGS="$save_fcflags" ;
  LIBS="$save_libs" ;
  #
  if test "x$hdf5" = xno; then
    if   test -d "$with_hdf5_libdir" || test -d "$with_hdf5_path"; then AC_MSG_RESULT([no]) ; fi
    #
    AC_MSG_CHECKING([for internal NETCDF+HDF5 library]);
    internal_hdf5="yes" ;
    internal_netcdf="yes" ;
    #
    NETCDF_OPT="--enable-netcdf-4";
    NETCDF_VER="v4";
    #
    NETCDF_HDF5_PATH="${extlibs_path}/${FCKIND}/${FC}/${NETCDF_VER}/${HDF5_VER}" ;
    NETCDF_HDF5_PAR_PATH="${extlibs_path}/${FCKIND}/${FC}/${NETCDF_VER}/parallel" ;
    #
    HDF5_LIBS="${NETCDF_HDF5_PATH}/lib/libhdf5hl_fortran.a ${NETCDF_HDF5_PATH}/lib/libhdf5_fortran.a ${NETCDF_HDF5_PATH}/lib/libhdf5_hl.a ${NETCDF_HDF5_PATH}/lib/libhdf5.a" ;
    HDF5_INCS="${IFLAG}${NETCDF_HDF5_PATH}/include" ;
    NETCDF_LIBS="${NETCDF_HDF5_PATH}/lib/libnetcdf.a" ;
    NETCDF_INCS="${IFLAG}${NETCDF_HDF5_PATH}/include" ;
    NETCDFF_LIBS="${NETCDF_HDF5_PATH}/lib/libnetcdff.a" ;
    NETCDFF_INCS="${IFLAG}${NETCDF_HDF5_PATH}/include" ;
    #
    netcdf=yes ;
    hdf5=yes ;
    #
    if test -e "${NETCDF_HDF5_PATH}/lib/libnetcdf.a" && test -e "${NETCDF_HDF5_PATH}/lib/libnetcdff.a" && test -e "${NETCDF_HDF5_PATH}/lib/libhdf5.a"; then
      #
      compile_netcdf="no" ;
      compile_hdf5="no" ;
      AC_MSG_RESULT([already compiled]) ;
      #
    elif test "$HDF5_VER" = "serial" && test -e "${NETCDF_HDF5_PAR_PATH}/lib/libnetcdf.a" && test -e "${NETCDF_HDF5_PAR_PATH}/lib/libnetcdff.a" && test -e "${NETCDF_HDF5_PAR_PATH}/lib/libhdf5.a"; then
      #
      HDF5_VER="parallel";
      HDF5_LIBS="${NETCDF_HDF5_PAR_PATH}/lib/libhdf5hl_fortran.a ${NETCDF_HDF5_PAR_PATH}/lib/libhdf5_fortran.a ${NETCDF_HDF5_PAR_PATH}/lib/libhdf5_hl.a ${NETCDF_HDF5_PAR_PATH}/lib/libhdf5.a" ;
      HDF5_INCS="${IFLAG}${NETCDF_HDF5_PAR_PATH}/include" ;
      NETCDF_LIBS="${NETCDF_HDF5_PAR_PATH}/lib/libnetcdf.a" ;
      NETCDF_INCS="${IFLAG}${NETCDF_HDF5_PAR_PATH}/include" ;
      NETCDFF_LIBS="${NETCDF_HDF5_PAR_PATH}/lib/libnetcdff.a" ;
      NETCDFF_INCS="${IFLAG}${NETCDF_HDF5_PAR_PATH}/include" ;
      AC_MSG_RESULT([already compiled (using parallel version)]) ;
      #
    else
      #  
      compile_netcdf="yes";
      compile_hdf5="yes" ;
      #
      if test "$HDF5_VER" = "serial";   then HDF5_OPT="--disable-parallel" ; fi
      if test "$HDF5_VER" = "parallel"; then HDF5_OPT="--enable-parallel"  ; fi
      #
      AC_MSG_RESULT([to be compiled]) ;
      #
    fi
    #
    #
    if test "$use_libz"    = "yes"; then HDF5_LIBS="$HDF5_LIBS -lz"   ; fi
    if test "$use_libsz"   = "yes"; then HDF5_LIBS="$HDF5_LIBS -lsz"  ; fi
    if test "$use_libm"    = "yes"; then HDF5_LIBS="$HDF5_LIBS -lm"   ; fi
    if test "$use_libdl"   = "yes"; then HDF5_LIBS="$HDF5_LIBS -ldl"  ; fi
    if test "$use_libcurl" = "yes"; then HDF5_LIBS="$HDF5_LIBS -lcurl"; fi
    #
  fi
fi
#
#
# NETCDF-HDF5 LIBS
#
if test x"$hdf5" = "xyes"; then
  def_netcdf="-D_HDF5_LIB";
  #if test "$HDF5_VER" = "serial" || test "$HDF5_VER" = "unknown" ; then
  #  def_netcdf="-D_HDF5_LIB";
  #elif test "$HDF5_VER" = "parallel"; then
  #  def_netcdf="-D_HDF5_PARLIB";
  #fi
fi
#
# Disable large File Support
#
if test x"$enable_netcdf_classic" = "xyes"; then
  def_netcdf="${def_netcdf} -D_NC_CLASSIC";
#
# NETCDF-HDF5 IO
#
elif test x"$netcdf" = "xyes" && test x"$hdf5" = "xyes" && test x"$enable_netcdf_hdf5" = "xyes" ; then
  def_netcdf="${def_netcdf} -D_HDF5_IO";
fi
#
# HDF5-DATA COMPRESSION
#
if test x"$netcdf" = "xyes" && test x"$hdf5" = "xyes" && test x"$enable_netcdf_hdf5" = "xyes" && test x"$enable_hdf5_compression" = "xyes" ; then
    def_netcdf="${def_netcdf} -D_HDF5_COMPRESSION";
fi
#
# NETCDF-HDF5 PAR IO
#
if test x"$netcdf" = "xyes" && test x"$hdf5" = "xyes" && test x"$enable_netcdf_hdf5" = "xyes" && test x"$enable_hdf5_par_io" = "xyes" ; then
    def_netcdf="${def_netcdf} -D_PAR_IO";
fi
#
AC_SUBST(NETCDF_LIBS)
AC_SUBST(NETCDF_INCS)
AC_SUBST(NETCDF_OPT)
AC_SUBST(NETCDF_VER)
AC_SUBST(NETCDFF_LIBS)
AC_SUBST(NETCDFF_INCS)
AC_SUBST(HDF5_LIBS)
AC_SUBST(HDF5_INCS)
AC_SUBST(HDF5_OPT)
AC_SUBST(HDF5_VER)
AC_SUBST(netcdf)
AC_SUBST(hdf5)
AC_SUBST(def_netcdf)
AC_SUBST(compile_netcdf)
AC_SUBST(compile_hdf5)
AC_SUBST(internal_netcdf)
AC_SUBST(internal_hdf5)
AC_SUBST(enable_hdf5_p2y_support)

])
