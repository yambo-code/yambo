#
# autoconf macro for detecting NetCDF module file
# from http://www.arsc.edu/support/news/HPCnews/HPCnews249.shtml
#
#        Copyright (C) 2000-2014 the YAMBO team
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

AC_ARG_ENABLE(netcdf, AC_HELP_STRING([--enable-netcdf],
            [Activate the NetCDF support]),[],[enable_netcdf="yes"])
AC_ARG_WITH(netcdf_libs,AC_HELP_STRING([--with-netcdf-libs=<libs>],
            [Use NetCDF libraries <libs>],[32]))
AC_ARG_WITH(netcdf_path, AC_HELP_STRING([--with-netcdf-path=<path>],
            [Path to the NetCDF install directory],[32]),[],[])
AC_ARG_WITH(netcdf_libdir,AC_HELP_STRING([--with-netcdf-libdir=<path>],
            [Path to the NetCDF lib directory],[32]))
AC_ARG_WITH(netcdf_includedir,AC_HELP_STRING([--with-netcdf-includedir=<path>],
            [Path to the NetCDF include directory],[32]))
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
dnetcdf=""
NCFLAGS=""
NCLIBS=""
IFLAG=""
compile_netcdf="no"
NETCDF_FLAGS=""


#
# global options
#
if test -d "$with_netcdf_libdir" ; then enable_netcdf=yes ; fi
if test -d "$with_netcdf_path"   ; then enable_netcdf=yes ; fi
if test x"$with_netcdf_libs" != "x"  ; then enable_netcdf=yes ; fi
#
if test -d "$with_hdf5_libdir"   ; then enable_netcdf_hdf5=yes ; fi
if test -d "$with_hdf5_path"     ; then enable_netcdf_hdf5=yes ; fi
if test  x"$with_hdf5_libs" != "x"   ; then enable_netcdf_hdf5=yes ; fi
#
if test x"$enable_netcdf" != "xyes" ; then enable_netcdf_hdf5=no ; fi
#
# F90 module flag
#
IFLAG=$ax_cv_f90_modflag
if test -z "$IFLAG" ; then IFLAG="-I" ; fi

#
# main search
#
if test "x$enable_netcdf" = "xyes" ; then
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
    if test -d "$with_netcdf_libdir"     ; then try_libdir=$with_netcdf_libdir ; fi
    if test -d "$with_netcdf_includedir" ; then try_incdir=$with_netcdf_includedir ; fi
    #
    if test -z "$try_libdir" ; then AC_MSG_ERROR([No lib-dir specified]) ; fi
    if test -z "$try_incdir" ; then AC_MSG_ERROR([No include-dir specified]) ; fi
    #
    AC_LANG([Fortran])
    #
    save_fcflags="$FCFLAGS"
    #
    FCFLAGS="$IFLAG$try_incdir $save_fcflags"
    AC_COMPILE_IFELSE(AC_LANG_PROGRAM([], [use netcdf]),
       [netcdf=yes 
        for file in `find "$try_incdir" \( -name '*netcdf*' -o -name '*typesizes*' \) `; do
          cp $file include/ 
        done
        for file in `find $try_libdir -name '*netcdf*.a'`; do
          cp $file lib/ 
        done
       ], [netcdf=no])
    FCFLAGS="$save_fcflags"
    #
    NCLIBS="-lnetcdf"
    if test -r $try_libdir/libnetcdff.a ; then
      NCLIBS="-lnetcdff ${NCLIBS}"
    fi
    #
    if test "x$netcdf" = xyes; then
      AC_MSG_RESULT([yes])
      dnetcdf="-D_NETCDF_IO"
      if test x"$enable_netcdf_LFS" = "xyes"; then dnetcdf="-D_NETCDF_IO -D_64BIT_OFFSET"; fi
    fi
    if test "x$netcdf" = xno; then
      AC_MSG_RESULT([no])
    fi
    #
  elif test x"$with_netcdf_libs" != "x" && test -d "$with_netcdf_includedir" ; then
    #
    # directly provided lib
    #
    AC_MSG_CHECKING([for NetCDF Library using $with_netcdf_libs])
    compile_netcdf="no"
    netcdf_idir="$IFLAG$with_netcdf_includedir"
    dnetcdf="-D_NETCDF_IO"
    if test x"$enable_netcdf_LFS" = "xyes"; then dnetcdf="-D_NETCDF_IO -D_64BIT_OFFSET"; fi
    netcdf=yes
    NCLIBS="$with_netcdf_libs"
    AC_MSG_RESULT(yes)
    #
  else
    #
    # internal netcdf
    #
    AC_MSG_CHECKING([for NetCDF library])
    # internal netcdf
    compile_netcdf="yes"
    if test "x$enable_bluegene" = "xyes" ; then 
        NCFLAGS=-DIBMR2Fortran
    fi
    # 
    # the following may change if we use a different version
    # of the netcdf lib
    #
    #NCLIBS="-lnetcdf"
    NCLIBS="-lnetcdff -lnetcdf"
    #
    dnetcdf="-D_NETCDF_IO"
    netcdf=yes
    AC_MSG_RESULT(Internal)
    #
  fi
else
 AC_MSG_CHECKING([for NetCDF library])
 AC_MSG_RESULT([no])
fi

#
# HDF5 support
#
hdf5="no"
if test "x$netcdf" = xyes; then  
  #
  if test x"$enable_netcdf_hdf5" = "xyes"; then
    #
    if test x"$compile_netcdf" = "xyes" ; then 
       AC_MSG_ERROR([HDFT5 support and Internal NetCDF not compatible])
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
    #
    if test -d "$try_libdir" ; then try_hdf5_flags="-L$try_libdir $try_hdf5_flags" ; fi
    if test x"$with_hdf5_libs" != "x" ; then HDF5_FLAGS="$with_hdf5_libs" ; fi
    #
    HDF5_FLAGS="$NCLIBS $try_hdf5_flags"
    #
    if test -d "$try_incdir" ; then FCFLAGS="$netcdf_idir $IFLAG$try_incdir" ; fi
    #
    for ldflag in "$HDF5_FLAGS" "$HDF5_FLAGS -lsz" "$HDF5_FLAGS -lz" ; do
      LIBS="$ldflag"
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
        dnetcdf="${dnetcdf} -D_HDF5_IO"
        NCLIBS="$ldflag"
        netcdf_idir="$netcdf_idir $FCFLAGS"
        AC_MSG_RESULT([yes])
        #
        break
      fi
    done
    FCFLAGS="$save_fcflags"    
    LIBS="$save_libs"
    #
    if test "x$hdf5" = xno; then AC_MSG_RESULT([no]) ; fi
  fi
fi

AC_SUBST(NCLIBS)
AC_SUBST(NCFLAGS)
AC_SUBST(netcdf)
AC_SUBST(netcdf_idir)
AC_SUBST(dnetcdf)
AC_SUBST(compile_netcdf)

])


