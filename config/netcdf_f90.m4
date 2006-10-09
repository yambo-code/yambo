#
# autoconf macro for detecting NetCDF module file
# from http://www.arsc.edu/support/news/HPCnews/HPCnews249.shtml
#
AC_DEFUN([KH_PATH_NETCDF_F90],[
AC_ARG_WITH(netcdf,AC_HELP_STRING([--with-netcdf=<path>],
                                  [Path of the NetCDF installation directory]),
  [NC_PREFIX="$withval"], [NC_PREFIX="/usr/local/"] )
netcdf="no"
dnetcdf=""
NCLIBS=""
AC_MSG_CHECKING([whether the NetCDF library is installed])
if ! test -z "$NC_PREFIX"; then
 AC_LANG([Fortran])
 save_fcflags="$FCFLAGS"
 for flag in "-I" "-M" "-p"; do
    FCFLAGS="$flag$NC_PREFIX/include $save_fcflags"
    AC_COMPILE_IFELSE(AC_LANG_PROGRAM([], [use netcdf]),
     [netcdf=yes 
      for file in `find $NC_PREFIX/include/ \( -name '*netcdf*' -o -name '*typesizes*' \) `; do
       cp $file include/ 
      done
      for file in `find $NC_PREFIX/lib/ -name '*netcdf*.a'`; do
       cp $file lib/ 
      done
     ], [netcdf=no])
    FCFLAGS="$save_fcflags"
    if test "x$netcdf" = xyes; then
     AC_MSG_RESULT([yes])
     NCLIBS="-lnetcdf"
     dnetcdf="-DNC"
     break
    fi
 done
fi
if test "x$netcdf" = xno; then
 AC_MSG_RESULT([no])
fi
AC_SUBST(NCLIBS)
AC_SUBST(netcdf)
AC_SUBST(dnetcdf)
])
