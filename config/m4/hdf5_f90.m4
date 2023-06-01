#
# from http://www.arsc.edu/support/news/HPCnews/HPCnews249.shtml
#
#        Copyright (C) 2000-2023 the YAMBO team
#              http://www.yambo-code.org
#
# Authors (see AUTHORS file for details): AM, AF, DS, CA
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
AC_DEFUN([AC_HAVE_HDF5_F90],[
#
AC_ARG_WITH(hdf5_libs,AS_HELP_STRING([--with-hdf5-libs=<libs>],[Use HDF5 libraries <libs>],[32]))
AC_ARG_WITH(hdf5_path, AS_HELP_STRING([--with-hdf5-path=<path>],[Path to the HDF5 install directory],[32]),[],[])
AC_ARG_WITH(hdf5_libdir,AS_HELP_STRING([--with-hdf5-libdir=<path>],[Path to the HDF5 lib directory],[32]))
AC_ARG_WITH(hdf5_includedir,AS_HELP_STRING([--with-hdf5-includedir=<path>],[Path to the HDF5 include directory],[32]))
#
# HDF5 data compression
#
AC_ARG_ENABLE(hdf5_compression,AS_HELP_STRING([--enable-hdf5-compression],[Activate the HDF5 data compression. Default is no.]))
#
# HDF5 PAR IO
#
AC_ARG_ENABLE(hdf5_par_io,AS_HELP_STRING([--enable-hdf5-par-io],[Enable the HDF5 parallel I/O. Default is yes]),,enable_hdf5_par_io="yes")
#
# HDF5 FOR P2Y (also requires parallel HDF5)
#
AC_ARG_ENABLE(hdf5_p2y_support, AS_HELP_STRING([--enable-hdf5-p2y-support],[Activate HDF5 support in p2y. Default is no unless parallel HDF5 libs are linked.]))
#
enable_hdf5="yes" ;
internal_hdf5="no"
NETCDF_VER="v4"
#
if test "$mpibuild" = "yes" ; then
  if test x"$enable_hdf5_par_io" = "xno" ; then 
   HDF5_OPT="--disable-parallel";
   IO_LIB_VER="serial";
  else
   HDF5_OPT="--enable-parallel";
   IO_LIB_VER="parallel";
  fi
else
  HDF5_OPT="--disable-parallel";
  IO_LIB_VER="serial";
  enable_hdf5_par_io="no";
fi
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
#
if test x"$enable_netcdf_classic" = "xyes" ; then  enable_hdf5=no      ; fi
if test x"$enable_netcdf_v3"      = "xyes" ; then  enable_hdf5=no      ; fi
if test x"$enable_netcdf_par_io"  = "xyes" ; then  enable_hdf5=no      ; fi
if test x"$enable_hdf5_par_io"    = "xyes" ; then  enable_hdf5=yes     ; fi
#    
if test x"$enable_hdf5_par_io" = "xno"   ; then IO_LIB_VER="serial"; fi
#
#
# HDF5 support
#
hdf5="no"
#
if test x"$enable_hdf5" = "xyes"; then
 #
 if ! test "$with_hdf5_libs" = "internal" ; then
  #
  AC_MSG_CHECKING([for HDF5]) ;
  if   test -d "$with_hdf5_libdir"    ; then AC_MSG_RESULT([in libdir $with_hdf5_libdir]) ;
  elif test -d "$with_hdf5_path"    ;   then AC_MSG_RESULT([in path $with_hdf5_path]) ;
  elif test x"$with_hdf5_libs" != "x" ; then AC_MSG_RESULT([using $with_hdf5_libs]) ;
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
  FCFLAGS="$try_HDF5_INCS $save_fcflags" ;
  LIBS="$try_HDF5_LIBS" ;
  #
  #if test "$HDF5_VER" = "serial" ; then
    AC_LINK_IFELSE(AC_LANG_PROGRAM([], [
       use hdf5
       implicit none
       integer  error
       call h5open_f(error)
       call h5close_f(error)
       ]),[hdf5=yes], [hdf5=no]);
  #fi;
  ##
  #AC_LINK_IFELSE(AC_LANG_PROGRAM([], [
  #   use hdf5
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
  #
  FCFLAGS="$save_fcflags" ;
  LIBS="$save_libs" ;
  #
  if test "x$hdf5" = "xno" ; then
    AC_MSG_RESULT([no]) ;
    #
    # Automatic detection of hdf5 libs copied from QE
    #
    AC_MSG_CHECKING([for HDF5 using h5pfc/h5fc system compilers]) ;
    #
    h5pfc="none"
    if test -e $with_hdf5_path/bin/h5pfc; then 
       h5pfc=$with_hdf5_path/bin/h5pfc; 
     elif command -v h5pfc > /dev/null; then 
       h5pfc=$(command -v h5pfc)  
    fi
    # 
    h5fc="none"
    if test -e $with_hdf5_path/bin/h5fc ; then 
       h5fc=$with_hdf5_path/bin/h5fc; 
    elif command -v h5fc > /dev/null; then 
       h5fc=$(command -v h5fc) 
    fi
    #
    # This should be fixed
    # Serial / Parallel libraries should be accepted only if they correspond to what needed by yambo
    #
    if test -e $h5pfc; then
       try_HDF5_LIBS=`$h5pfc -show | awk -F'-L' '{@S|@1=""; for (i=2; i<=NF;i++) @S|@i="-L"@S|@i; print @S|@0}'`
       try_hdf5_incdir=`$h5pfc -show | awk -F'-I' '{print @S|@2}' | awk '{print @S|@1}'`
       IO_LIB_VER="parallel";
    elif test -e $h5fc; then 
       try_HDF5_LIBS=`$h5fc -show | awk -F'-L' '{@S|@1=""; for (i=2; i<=NF;i++) @S|@i="-L"@S|@i; print @S|@0}'`
       try_hdf5_incdir=`$h5fc -show | awk -F'-I' '{print @S|@2}' | awk '{print @S|@1}'`
       IO_LIB_VER="serial";
       enable_hdf5_par_io="no";
    fi
    #
    try_HDF5_INCS="$IFLAG$try_hdf5_incdir" ;
    #
    save_libs="$LIBS" ;
    save_fcflags="$FCFLAGS" ;
    #
    FCFLAGS="$try_HDF5_INCS $save_fcflags" ;
    LIBS="$try_HDF5_LIBS" ;
    #
    AC_LINK_IFELSE(AC_LANG_PROGRAM([], [
       use hdf5
       implicit none
       integer  error
       call h5open_f(error)
       call h5close_f(error)
       ]),[hdf5=yes], [hdf5=no]);
    #
    FCFLAGS="$save_fcflags" ;
    LIBS="$save_libs" ;
    #
   fi
   #
   if test "x$hdf5" = xno; then
      AC_MSG_RESULT([no]) ;
      AC_MSG_CHECKING([for HDF5 using automatic library list]) ;
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
      if test -d "$try_hdf5_libdir" ; then try_HDF5_LIBS="-L$try_hdf5_libdir -lhdf5_hl_fortran -lhdf5_fortran -lhdf5_hl -lhdf5" ; fi
      if test "$use_libz"    = "yes"; then try_HDF5_LIBS="$try_HDF5_LIBS -lz"   ; fi
      if test "$use_libsz"   = "yes"; then try_HDF5_LIBS="$try_HDF5_LIBS -lsz"  ; fi
      if test "$use_libm"    = "yes"; then try_HDF5_LIBS="$try_HDF5_LIBS -lm"   ; fi
      if test "$use_libdl"   = "yes"; then try_HDF5_LIBS="$try_HDF5_LIBS -ldl"  ; fi
      if test "$use_libcurl" = "yes"; then try_HDF5_LIBS="$try_HDF5_LIBS -lcurl"; fi
      if test x"$try_hdf5_incdir" == "x" ; then  
        if test -d "$with_hdf5_path/include/" ; then $try_hdf5_incdir="$with_hdf5_path/include/"; fi
      fi
      #
      try_HDF5_INCS="$IFLAG$try_hdf5_incdir" ; 
      #
      save_libs="$LIBS" ;
      save_fcflags="$FCFLAGS" ;
      #
      FCFLAGS="$try_HDF5_INCS $save_fcflags" ;
      LIBS="$try_HDF5_LIBS" ;
      #
      AC_LINK_IFELSE(AC_LANG_PROGRAM([], [
        use hdf5
        implicit none
        integer  error
        call h5open_f(error)
        call h5close_f(error)
      ]),[hdf5=yes], [hdf5=no]);
      #
      FCFLAGS="$save_fcflags" ;
      LIBS="$save_libs" ;
    fi
  fi
  #
  # To be fixed
  #
  if test "x$hdf5" = xyes; then
    HDF5_LIBS="$try_HDF5_LIBS" ;
    HDF5_INCS="$try_HDF5_INCS" ;
    if test $IO_LIB_VER = "parallel"; then 
       AC_MSG_RESULT([yes - parallel lib found]) ; 
       HDF5_OPT="--enable-parallel"  ; 
    fi
    if test $IO_LIB_VER = "serial";   then
       AC_MSG_RESULT([yes - serial lib found]) ;
       HDF5_OPT="--disable-parallel"  ; 
    fi
    # AC_MSG_RESULT([yes]) ;
    IO_LIB_VER="unknown"
  fi
  #
  if test "x$hdf5" = xno; then
    if ! test "$with_hdf5_libs" = "internal" ; then
      AC_MSG_RESULT([no]) ;
    fi
    #
    AC_MSG_CHECKING([for internal HDF5 library]);
    internal_hdf5="yes" ;
    #
    NETCDF_VER="v4";
    #
    NETCDF_HDF5_PATH="${extlibs_path}/${FCKIND}/${FC}/${NETCDF_VER}/${IO_LIB_VER}" ;
    NETCDF_HDF5_PAR_PATH="${extlibs_path}/${FCKIND}/${FC}/${NETCDF_VER}/parallel" ;
    #
    HDF5_LIBS_F="${NETCDF_HDF5_PATH}/lib/libhdf5_hl_fortran.a ${NETCDF_HDF5_PATH}/lib/libhdf5_fortran.a";
    HDF5_LIBS_C="${NETCDF_HDF5_PATH}/lib/libhdf5_hl.a ${NETCDF_HDF5_PATH}/lib/libhdf5.a";
    HDF5_LIBS="$HDF5_LIBS_F $HDF5_LIBS_C";
    HDF5_INCS="${IFLAG}${NETCDF_HDF5_PATH}/include" ;
    #
    hdf5=yes ;
    #
    if test -e "${NETCDF_HDF5_PATH}/lib/libhdf5.a"; then
      #
      compile_hdf5="no" ;
      AC_MSG_RESULT([already compiled]) ;
      #
    elif test "$IO_LIB_VER" = "serial" && test -e "${NETCDF_HDF5_PAR_PATH}/lib/libhdf5.a"; then
      #
      compile_hdf5="no" ;
      IO_LIB_VER="parallel";
      HDF5_LIBS="${NETCDF_HDF5_PAR_PATH}/lib/libhdf5_hl_fortran.a ${NETCDF_HDF5_PAR_PATH}/lib/libhdf5_fortran.a ${NETCDF_HDF5_PAR_PATH}/lib/libhdf5_hl.a ${NETCDF_HDF5_PAR_PATH}/lib/libhdf5.a" ;
      HDF5_INCS="${IFLAG}${NETCDF_HDF5_PAR_PATH}/include" ;
      AC_MSG_RESULT([already compiled (using parallel version)]) ;
      #
    else
      #  
      compile_hdf5="yes" ;
      #
      if test "$IO_LIB_VER" = "serial";   then HDF5_OPT="--disable-parallel" ; fi
      if test "$IO_LIB_VER" = "parallel"; then HDF5_OPT="--enable-parallel"  ; fi
      #
      AC_MSG_RESULT([to be compiled]) ;
      #
    fi
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
AC_SUBST(HDF5_LIBS)
AC_SUBST(HDF5_INCS)
AC_SUBST(HDF5_OPT)
AC_SUBST(IO_LIB_VER)
AC_SUBST(netcdf)
AC_SUBST(hdf5)
AC_SUBST(def_netcdf)
AC_SUBST(compile_hdf5)
AC_SUBST(internal_netcdf)
AC_SUBST(internal_hdf5)
AC_SUBST(enable_hdf5_p2y_support)

])
