#
# License-Identifier: GPL
#
# Copyright (C) 2022 The Yambo Team
#
# Authors (see AUTHORS file for details): AM
#
AC_DEFUN([ACX_IO_WRAP_UP],
[
#
# NETCDF-HDF5 PAR IO or HDF5-DATA COMPRESSION (the two are exclusive)
#
compile_pnetcdf="no"
IO_MODE="serial"
PARIO_info=" "
#
if test x"$netcdf" = "xyes" && test x"$hdf5" = "xyes" && test x"$enable_hdf5" = "xyes" && test x"$enable_hdf5_par_io" = "xyes" ; then
    def_netcdf="${def_netcdf} -D_PAR_IO";
    enable_hdf5_compression="no";
    IO_MODE="parallel";
    PARIO_info="(via HDF5)";    
elif test x"$netcdf" = "xyes" && test x"$enable_pnetcdf" = "xyes" ; then
    def_netcdf="${def_netcdf} -D_PAR_IO";
    compile_pnetcdf=${compile_netcdf};
    enable_hdf5_compression="no";
    IO_MODE="parallel";
    PARIO_info="(via PNETCDF)";    
elif test x"$netcdf" = "xyes" && test x"$hdf5" = "xyes" && test x"$enable_hdf5" = "xyes" && test x"$enable_hdf5_compression" = "xyes" ; then
    def_netcdf="${def_netcdf} -D_HDF5_COMPRESSION";
    IO_MODE="parallel";
    PARIO_info="(via COMPRESS-HDF5)";    
fi
#
AC_SUBST(compile_pnetcdf)
AC_SUBST(PARIO_info)
AC_SUBST(IO_MODE)
])
