#
#        Copyright (C) 2000-2020 the YAMBO team
#              http://www.yambo-code.org
#
# Authors (see AUTHORS file for details): DS AM
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
chmod u+x sbin/*
cp config/Makefile .
#
if ! test -d bin      ; then mkdir bin      ; fi
if ! test -d include  ; then mkdir include  ; fi
if ! test -d lib      ; then mkdir lib      ; fi
if ! test -d lib/bin  ; then mkdir lib/bin  ; fi
#
if   test -d include/system ; then rm -r include/system ; fi
if ! test -d include/system ; then mkdir include/system ; fi
#
# Copy system headers
#
#
YAML_INCDIRS=`echo $YAML_INCS       | sed "s/$IFLAG/ /g"` ;                       #  
for includedir in $YAML_INCDIRS;   do                                             #  This part
  count=`ls -1 $includedir/*yaml*.h   2>/dev/null | wc -l`                        #  need to be
  if test $count != 0; then ln -fs $includedir/*yaml*.h   include/system/  ; fi   #  fixed
  count=`ls -1 $includedir/*yaml*.mod 2>/dev/null | wc -l`                        #
  if test $count != 0; then ln -fs $includedir/*yaml*.mod include/system/  ; fi   #
done
#
FUTILE_INCDIRS=`echo $FUTILE_INCS       | sed "s/$IFLAG/ /g"` ;                     #  
for includedir in $FUTILE_INCDIRS;   do                                             #  This part
  count=`ls -1 $includedir/*futile*.h   2>/dev/null | wc -l`                        #  need to be
  if test $count != 0; then ln -fs $includedir/*futile*.h   include/system/  ; fi   #  fixed
  count=`ls -1 $includedir/*futile*.mod 2>/dev/null | wc -l`                        #
  if test $count != 0; then ln -fs $includedir/*futile*.mod include/system/  ; fi   #
done
#
IOTK_INCDIRS=`echo $IOTK_INCS       | sed "s/$IFLAG/ /g"` ; 
for includedir in $IOTK_INCDIRS;   do
  count=`ls -1 $includedir/*iotk*.h   2>/dev/null | wc -l`
  if test $count != 0; then ln -fs $includedir/*iotk*.h   include/system/  ; fi
  count=`ls -1 $includedir/*iotk*.mod 2>/dev/null | wc -l`
  if test $count != 0; then ln -fs $includedir/*iotk*.mod include/system/  ; fi
done
#
ETSF_INCDIRS=`echo "$ETSF_INCS"     | sed "s/$IFLAG/ /g"`
for includedir in $ETSF_INCDIRS;   do
  count=`ls -1 $includedir/*etsf*.h   2>/dev/null | wc -l`
  if test $count != 0; then ln -fs $includedir/*etsf*.h   include/system/  ; fi
  count=`ls -1 $includedir/*etsf*.mod 2>/dev/null | wc -l`
  if test $count != 0; then ln -fs $includedir/*etsf*.mod include/system/  ; fi
done
#
NETCDF_INCDIRS=`echo "$NETCDF_INCS" | sed "s/$IFLAG/ /g"`
NETCDFF_INCDIRS=`echo "$NETCDFF_INCS" | sed "s/$IFLAG/ /g"`
for includedir in $NETCDF_INCDIRS $NETCDFF_INCDIRS; do
  count=`ls -1 $includedir/*netcdf*.h   2>/dev/null | wc -l`
  if test $count != 0; then ln -fs $includedir/*netcdf*.h   include/system/  ; fi
  count=`ls -1 $includedir/*netcdf*.mod   2>/dev/null | wc -l`
  if test $count != 0; then ln -fs $includedir/*netcdf*.mod include/system/  ; fi
  if test -e $includedir/typesizes.h; then ln -fs $includedir/typesizes.h  include/system/  ; fi
done
#
HDF5_INCDIRS=`echo "$HDF5_INCS"     | sed "s/$IFLAG/ /g"`
for includedir in $HDF5_INCDIRS;   do
  count=`ls -1 $includedir/*H5*.h   2>/dev/null | wc -l`
  if test $count != 0; then ln -fs $includedir/*H5*.h       include/system/  ; fi
  count=`ls -1 $includedir/*H5*.mod 2>/dev/null | wc -l`
  if test $count != 0; then ln -fs $includedir/*H5*.mod     include/system/  ; fi
done
#
FFT_INCDIRS=`echo "$FFT_INCS"       | sed "s/$IFLAG/ /g"`
for includedir in $FFT_INCDIRS;    do
  count=`ls -1 $includedir/*fft*.h   2>/dev/null | wc -l`
  if test $count != 0; then ln -fs $includedir/*fft*.h      include/system/  ; fi
  count=`ls -1 $includedir/*fft*.mod 2>/dev/null | wc -l`
  if test $count != 0; then ln -fs $includedir/*fft*.mod    include/system/  ; fi
done
#
PETSC_INCDIRS=`echo "$PETSC_INCS"   | sed "s/$IFLAG/ /g"`
for includedir in $PETSC_INCDIRS;  do
  count=`ls -1 $includedir/*petsc*.h   2>/dev/null | wc -l`
  if test $count != 0; then ln -fs $includedir/*petsc*.h    include/system/  ; fi
  count=`ls -1 $includedir/*petsc*.mod 2>/dev/null | wc -l`
  if test $count != 0; then ln -fs $includedir/*petsc*.mod  include/system/  ; fi
done
#
SLEPC_INCDIRS=`echo "$SLEPC_INCS"   | sed "s/$IFLAG/ /g"`
for includedir in $SLEPC_INCDIRS;  do
  count=`ls -1 $includedir/*slepc*.h   2>/dev/null | wc -l`
  if test $count != 0; then ln -fs $includedir/*slepc*.h    include/system/  ; fi
  count=`ls -1 $includedir/*slepc*.mod 2>/dev/null | wc -l`
  if test $count != 0; then ln -fs $includedir/*slepc*.mod  include/system/  ; fi
done
#
LIBXC_INCDIRS=`echo "$LIBXC_INCS"   | sed "s/$IFLAG/ /g"`
for includedir in $LIBXC_INCDIRS;  do
  count=`ls -1 $includedir/*xc*.h   2>/dev/null | wc -l`
  if test $count != 0; then ln -fs $includedir/*xc*.h       include/system/  ; fi
  count=`ls -1 $includedir/*xc*.mod 2>/dev/null | wc -l`
  if test $count != 0; then ln -fs $includedir/*xc*.mod     include/system/  ; fi
done
#
MPI_INCDIRS=`echo "$MPI_INCS"       | sed "s/$IFLAG/ /g"`
for includedir in $MPI_INCDIRS;    do
  count=`ls -1 $includedir/*mpi*.h   2>/dev/null | wc -l`
  if test $count != 0; then ln -fs $includedir/*mpi*.h     include/system/  ; fi
  count=`ls -1 $includedir/*mpi*.mod 2>/dev/null | wc -l`
  if test $count != 0; then ln -fs $includedir/*mpi*.mod   include/system/  ; fi
done
#
if ! test -d "$extlibs_path/${FCKIND}/${FC}";         then mkdir -p "$extlibs_path/${FCKIND}/${FC}";         fi
if ! test -d "$extlibs_path/${FCKIND}/${FC}/lib";     then mkdir    "$extlibs_path/${FCKIND}/${FC}/lib";     fi
if ! test -d "$extlibs_path/${FCKIND}/${FC}/include"; then mkdir    "$extlibs_path/${FCKIND}/${FC}/include"; fi
if ! test -d "$extlibs_path/${FCKIND}/${FC}/bin";     then mkdir    "$extlibs_path/${FCKIND}/${FC}/bin";     fi
#
if  test -d "$extlibs_path/${FCKIND}/${FC}" ; then
 BIN_LIBRARIES=$extlibs_path/${FCKIND}/${FC}/bin/* ;
 BIN_NETCDF=$extlibs_path/${FCKIND}/${FC}/${NETCDF_VER}/${HDF5_VER}/bin/* ;
 BIN_SLEPC=$extlibs_path/${FCKIND}/${FC}/${build_precision}/bin/* ;
 for file in BIN_LIBRARIES $BIN_NETCDF $BIN_SLEPC; do
  if test -f $file; then 
   cp $file $exec_prefix/lib/bin ;
  fi
 done;
fi
#
if [[ "$prefix" != "$srcdir" ]] && [[ "$srcdir" != "." ]] ; then
 if test ! -d "$prefix/driver"     ; then mkdir "$prefix/driver"     ; fi
 if test ! -d "$prefix/ypp"        ; then mkdir "$prefix/ypp"        ; fi
 if test ! -d "$prefix/interfaces" ; then mkdir "$prefix/interfaces" ; fi
 if test ! -d "$prefix/lib"        ; then mkdir "$prefix/lib"        ; fi
 if test ! -d "$prefix/lib/local"  ; then mkdir "$prefix/lib/local"  ; fi
 if test ! -d "$prefix/lib/slatec" ; then mkdir "$prefix/lib/slatec" ; fi
 if test ! -d "$prefix/lib/math77" ; then mkdir "$prefix/lib/math77" ; fi
 cd "$srcdir/driver/" ;
 for file in `ls *.h` ; do
  cp "$file" "$prefix/driver" ;
 done ;
 for file in `ls *.c` ; do
  cp "$file" "$prefix/driver" ;
 done ;
 cd "../lib/local" ;
 for file in `ls *.f` ; do
  cp "$file" "$prefix/lib/local" ;
 done ;
 cd "../slatec" ;
 for file in `ls *.f` ; do
  cp "$file" "$prefix/lib/slatec" ;
 done ;
 cd "../math77" ;
 for file in `ls *.f` ; do
  cp "$file" "$prefix/lib/math77" ;
 done ;
 cd "$prefix" ;
fi
