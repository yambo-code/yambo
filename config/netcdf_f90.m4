#
# autoconf macro for detecting NetCDF module file
# from http://www.arsc.edu/support/news/HPCnews/HPCnews249.shtml
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
AC_ARG_ENABLE(netcdf-LFS, AC_HELP_STRING([--enable-netcdf-LFS],
             [Activate NetCDF Large File Support. Default is no.]))
#
# HDF5 support
#
AC_ARG_ENABLE(netcdf_hdf5,AC_HELP_STRING([--enable-netcdf-hdf5],
                                  [Activate the HDF5 support. Default is no.]))
#
netcdf="no"
netcdf_idir=""
netcdff_idir=""
dnetcdf=""
NCFLAGS=""
NCLIBS=""
NFLIBS=""
IFLAG=""
compile_netcdf="no"
enable_hdf5=""
#
# global options
#
#
if test -d "$with_hdf5_libdir"   ; then enable_hdf5=yes ; fi
if test -d "$with_hdf5_path"     ; then enable_hdf5=yes ; fi
if test  x"$with_hdf5_libs"   != "x"    ; then enable_hdf5=yes ; fi
#
if test x"$enable_netcdf_hdf5" = "xyes" ; then 
   enable_hdf5=yes 
fi
#
# F90 module flag
#
IFLAG=$ax_cv_f90_modflag
if test -z "$IFLAG" ; then IFLAG="-I" ; fi
#
# main search
#
#
if test -d "$with_netcdf_path" || test -d "$with_netcdf_libdir" ; then
  #
  # external netcdf
  #
  if test -d "$with_netcdf_libdir" ; then AC_MSG_CHECKING([for NetCDF in $with_netcdf_libdir]) 
  elif test -d "$with_netcdf_path" ; then AC_MSG_CHECKING([for NetCDF in $with_netcdf_path]) 
  fi
  #
  if test -d "$with_netcdf_path" ; then 
      try_libdir=$with_netcdf_path/lib
      try_incdir=$with_netcdf_path/include
  fi
  if test -d "$with_netcdff_path" ; then 
      tryf_libdir=$with_netcdff_path/lib
      tryf_incdir=$with_netcdff_path/include
  fi
  #
  if test -d "$with_netcdf_libdir"     ; then try_libdir=$with_netcdf_libdir ; fi
  if test -d "$with_netcdf_includedir" ; then try_incdir=$with_netcdf_includedir ; fi
  #
  if test -d "$with_netcdff_libdir"     ; then tryf_libdir=$with_netcdff_libdir ; fi
  if test -d "$with_netcdff_includedir" ; then tryf_incdir=$with_netcdff_includedir ; fi
  #
  if test -z "$try_libdir" ; then AC_MSG_ERROR([No lib-dir specified]) ; fi
  if test -z "$try_incdir" ; then AC_MSG_ERROR([No include-dir specified]) ; fi
  #
  AC_LANG([Fortran])
  #
  save_fcflags="$FCFLAGS"
  #
  FCFLAGS="$IFLAG$try_incdir $save_fcflags"
  if test -d "$tryf_incdir" ; then
    FCFLAGS="$IFLAG$try_incdir $IFLAG$tryf_incdir  $save_fcflags"
  fi
  AC_COMPILE_IFELSE(AC_LANG_PROGRAM([], [use netcdf]),
     [netcdf=yes
      if test ! -d include ; then mkdir include ; fi
      for file in `find "$try_incdir" \( -name '*netcdf*' -o -name '*typesizes*' \) `; do
        cp $file include/ 
      done
      for file in `find "$try_incdir" \( -name '*NETCDF*' -o -name '*TYPESIZES*' \) `; do
        cp $file include/ 
      done
      if test -d "$tryf_incdir"; then
        for file in `find "$tryf_incdir" \( -name '*netcdf*' -o -name '*typesizes*' \) `; do
          cp $file include/ 
        done
        for file in `find "$tryf_incdir" \( -name '*NETCDF*' -o -name '*TYPESIZES*' \) `; do
          cp $file include/ 
        done
      fi
      if test ! -d lib ; then mkdir lib ; fi
      for file in `find $try_libdir -name '*netcdf*.a'`; do
        cp $file lib/ 
      done
      if test -d "$tryf_libdir" ; then 
        for file in `find $tryf_libdir -name '*netcdf*.a'`; do
          cp $file lib/ 
        done
      fi
     ], [netcdf=no])
  FCFLAGS="$save_fcflags"
  #
  NCLIBS="-lnetcdf"
  if test -r $tryf_libdir/libnetcdff.a ; then
    NFLIBS="-lnetcdff"
  elif test -r $try_libdir/libnetcdff.a ; then
    NCLIBS="-lnetcdff -lnetcdf"
  fi
  #
  if test "x$netcdf" = xyes; then
    AC_MSG_RESULT([yes])
  fi
  if test "x$netcdf" = xno; then
    AC_MSG_RESULT([no])
  fi
  #
  NETCDF_LIBS="-L$try_libdir $NCLIBS"
  if test -d "$tryf_libdir" ;  then
    NETCDF_LIBS="-L$tryf_libdir $NFLIBS -L$try_libdir $NCLIBS"
  fi
  NCLIBS="$NFLIBS $NCLIBS"
  #
elif test x"$with_netcdf_libs" != "x" ; then
  #
  # directly provided lib
  #
  AC_MSG_CHECKING([for NetCDF Library using $with_netcdf_libs])
  compile_netcdf="no"
  if test -d "$with_netcdf_includedir" ; then netcdf_idir="$IFLAG$with_netcdf_includedir" ; fi
  if test -d "$with_netcdff_includedir" ; then netcdff_idir="$IFLAG$with_netcdff_includedir" ; fi
  netcdf=yes
  NCLIBS="$with_netcdff_libs $with_netcdf_libs" 
  AC_MSG_RESULT(yes)
  #
  NETCDF_LIBS="$NCLIBS"
  #
fi
if test "x$netcdf" = xno; then
  #
  # internal netcdf
  #
  AC_MSG_CHECKING([for NetCDF library])
  # internal netcdf
  compile_netcdf="yes"
  if test "x$enable_bluegene" = "xyes" ; then NCFLAGS=-DIBMR2Fortran ; fi
  # 
  # the following may change if we use a different version
  # of the netcdf lib
  #
  #NCLIBS="-lnetcdf"
  NCLIBS="-lnetcdff -lnetcdf"
  #
  netcdf=yes
  AC_MSG_RESULT(Internal)
  #
  NETCDF_LIBS="$NCLIBS"
  # 
fi
#
# Large File Support
#
if test x"$enable_netcdf_LFS" = "xyes"; then dnetcdf="$dnetcdf -D_64BIT_OFFSET"; fi
#
#
# HDF5 support
#
hdf5="no"
#
if test x"$enable_hdf5" = "xyes"; then
  #
  if test x"$compile_netcdf" = "xyes" ; then 
     AC_MSG_ERROR([HDF5 support and Internal NetCDF not compatible])
  fi
  #
  if   test -d "$with_hdf5_libdir" ; then AC_MSG_CHECKING([for HDF5 in $with_hdf5_libdir]) 
  elif test -d "$with_hdf5_path" ;   then AC_MSG_CHECKING([for HDF5 in $with_hdf5_path]) 
  elif test x"$with_hdf5_libs" != "x" ; then AC_MSG_CHECKING([for HDF5 using $with_hdf5_libs])
  fi
  #
  AC_LANG([Fortran])       
  save_libs="$LIBS"
  save_fcflags="$FCFLAGS"
  #
  # re-define lib and inc dirs
  #
  if test -d "$with_hdf5_path" ; then 
      try_libdir=$with_hdf5_path/lib
      try_incdir=$with_hdf5_path/include
  fi
  if test -d "$with_hdf5_libdir"     ; then try_libdir=$with_hdf5_libdir ; fi
  if test -d "$with_hdf5_includedir" ; then try_incdir=$with_hdf5_includedir ; fi
  #
  try_hdf5_flags="-lhdf5_fortran -lhdf5_hl -lhdf5"
  if test x"$with_hdf5_libs" != "x" ; then try_hdf5_flags="$with_hdf5_libs" ; fi
  #
  if test -d "$try_libdir" ; then try_hdf5_flags="-L$try_libdir $try_hdf5_flags" ; fi
  #
  HDF5_FLAGS="$try_hdf5_flags"
  #
  FCFLAGS_="$netcdf_idir $netcdff_idir"
  if test -d "$try_incdir" ; then FCFLAGS_="$FCFLAGS_ $IFLAG$try_incdir" ; fi
  FCFLAGS="${IFLAG}./include $FCFLAGS_ $save_fcflags"
  #
  for ldflag in "-lcurl -lz" "-lcurl -lsz -lz" "-lsz -lz" "-lz" " "; do
    LIBS="-L./lib $NCLIBS $HDF5_FLAGS $ldflag"
    AC_LINK_IFELSE(AC_LANG_PROGRAM([], [
       use hdf5
       use netcdf
       implicit none
       integer cmode
       cmode = NF90_HDF5
       cmode = nf90_abort(1)
       call h5open_f(cmode)]),
       [hdf5=yes], [hdf5=no])
    if test "x$hdf5" = xyes; then
      NCLIBS="$NCLIBS $HDF5_FLAGS $ldflag"
      netcdf_idir="$FCFLAGS_"
      AC_MSG_RESULT([yes])
      #
      if test x"$with_hdf5_libs" != "x" ; then
        HDF5_LIBS="$HDF5_FLAGS"
      else
        HDF5_LIBS="-L$try_libdir $HDF5_FLAGS"
      fi
      #
      break
    fi
  done
  FCFLAGS="$save_fcflags"    
  LIBS="$save_libs"
  #
  if test "x$hdf5" = xno; then AC_MSG_RESULT([no]) ; fi
fi
#
# NETCDF-HDF5 IO
#
if test x"$netcdf" = "xyes" && test x"$hdf5" = "xyes" && test x"$enable_netcdf_hdf5" = "xyes" ; then
    dnetcdf="${dnetcdf} -D_HDF5_IO";
fi

AC_SUBST(NETCDF_LIBS)
AC_SUBST(HDF5_LIBS)
AC_SUBST(NCLIBS)
AC_SUBST(NCFLAGS)
AC_SUBST(netcdf)
AC_SUBST(netcdf_idir)
AC_SUBST(dnetcdf)
AC_SUBST(compile_netcdf)

])
