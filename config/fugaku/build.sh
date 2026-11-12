#!/bin/bash
# Cross-compile Yambo for Fugaku (A64FX) on an x86 login node with Fujitsu
# Fortran, SSL2, Fujitsu MPI and the site Spack libraries.
# Usage: YAMBO_WORK=<new directory> bash config/fugaku/build.sh
set -euo pipefail
PROFILE=$(cd "$(dirname "$0")" && pwd)
SOURCE=$(cd "$PROFILE/../.." && pwd)
WORK=${YAMBO_WORK:?Set YAMBO_WORK to a new directory on project storage}
OPT=${YAMBO_FCFLAGS:--Kfast -Kocl -g -Kopenmp}

TCS=/opt/FJSVxtclanga/tcsds-1.2.41
SPACK=/vol0004/apps/oss/spack-v1.0.1/opt/spack/linux-a64fx
NC=$SPACK/netcdf-c-4.9.2-vk7s5qmah7pmecpujgbzjkwn2qmc7hkc
NF=$SPACK/netcdf-fortran-4.6.1-dvqvtiwdr4vug2e253qxrwxwmfgvli4b
H5=$SPACK/hdf5-1.14.6-6jmbeftqcya7nolxvbcn3i7ru7lxmlxc
FFT=$SPACK/fujitsu-fftw-1.1.0-4uqby7bqrn7t2tr5si4naudfz7okivxx
XC=$SPACK/libxc-6.2.2-wwaw5lc6nusclomeqmtwikoypiemhyvg
# HDF5 with Fortran bindings comes before NetCDF's C-only HDF5 dependency.
LIBDIRS="$H5/lib $NF/lib $NC/lib $FFT/lib $XC/lib $TCS/lib64
  $SPACK/libaec-1.0.6-e64wyyosfg3jaeat7a5uercmdgbqnui4/lib64
  $SPACK/c-blosc-1.21.6-otajzivhzp3vdqwgmko2t7okz6p2tnfm/lib64
  $SPACK/snappy-1.2.1-adxzrswvg73ltdbfvnnrx3uucnp2dfzr/lib
  $SPACK/zstd-1.5.7-6kjmzm3tclvbxkkb5vomygtn4nkbwcfu/lib"
RPATH=$(for d in $LIBDIRS; do printf ' -Wl,-rpath,%s -Wl,-rpath-link,%s' "$d" "$d"; done)
export PATH=$TCS/bin:$PATH OMP_NUM_THREADS=1

test ! -e "$WORK/yambo" || { echo "$WORK/yambo exists: use a new YAMBO_WORK" >&2; exit 1; }
mkdir -p "$WORK/yambo"
cd "$WORK"
exec > >(tee build.log) 2>&1
echo "Fortran flags: $OPT"
git -C "$SOURCE" describe --always --dirty > SOURCE_COMMIT 2>/dev/null || echo unknown > SOURCE_COMMIT
tar -C "$SOURCE" --exclude=.git -cf - . | tar -xf - -C yambo
cat > env.sh <<EOF
export YAMBO_WORK=$WORK
export PATH=$WORK/yambo/bin:$TCS/bin:\$PATH
export LD_LIBRARY_PATH=$(echo $LIBDIRS | tr ' ' :):\${LD_LIBRARY_PATH:-}
EOF
# MPI, OpenMP, SSL2 and NaN check; run it in a job (see README).
mpifrtpx -O2 -Kopenmp -Nalloc_assign -SSL2BLAMP "$PROFILE/verify.f90" -o verify

cd yambo
# configure runs a pointer-size probe and file checks that cannot work when
# cross-compiling; A64FX pointers are 8 bytes and the MPI files exist.
export POINTER_SIZE=8 ac_pointersize=8 ac_cv_file_mpifrtpx=yes ac_cv_file_mpifccpx=yes
export "ac_cv_file_$(printf %s "$TCS/include/mpi/fujitsu/mpif.h" | tr -c '[:alnum:]_' _)=yes"
# Automatic detection garbles this list (ld's banner lands inside -latomic).
export FLIBS="-L$TCS/lib64 -lfjomphk -lfjomp -lfj90i -lfj90fmt_sve -lfj90f -lfjsrcinfo -lfjompcrt -lfjprofcore -lfjprofomp -lmpg -latomic -lpthread -lrt -lelf -lz -ldl"
./configure --build=x86_64-pc-linux-gnu --host=aarch64-unknown-linux-gnu \
  FC=frtpx F77=frtpx CC=fccpx MPIFC=mpifrtpx MPIF77=mpifrtpx MPICC=mpifccpx \
  FCFLAGS="$OPT -Cpp -Nalloc_assign" FFLAGS="$OPT -Cpp -Nalloc_assign" \
  UFLAGS='-O0 -g -Kopenmp -Cpp -Nalloc_assign' CFLAGS='-O2 -Kopenmp' OMPFLAGS=-Kopenmp \
  CPP='cpp -E -P -D_FUJITSU' FPP='cpp -E -P -traditional-cpp -D_FUJITSU' LDFLAGS="$RPATH" \
  --enable-dp --enable-open-mp --enable-par-linalg --enable-hdf5-par-io \
  --with-extlibs-path="$WORK/deps" --with-mpi-includedir="$TCS/include/mpi/fujitsu" \
  --with-blas-libs=-SSL2BLAMP --with-lapack-libs=-SSL2BLAMP \
  --with-scalapack-libs=-SCALAPACK --with-blacs-libs=-SCALAPACK \
  --with-fft-path="$FFT" --with-hdf5-path="$H5" \
  --with-netcdf-path="$NC" --with-netcdff-path="$NF" \
  --with-libxc-path="$XC" --with-libxc-libs="-L$XC/lib -lxcf03 -lxc" --with-editor=none

# Bundled libraries: unpack, patch for cross-compilation (prepare.py), build.
D=$WORK/deps/unknown/mpifrtpx
mkdir -p log "$D/lib" "$D/bin" "$D/include"
(cd lib/archive && make -f Makefile.loc iotk ydiago devxlib)
for d in iotk ydiago devxlib; do
  (cd lib/$d && make -f Makefile.loc uncompress && cp -n Makefile.loc Makefile)
done
python3 "$PROFILE/prepare.py"
for d in iotk ydiago devxlib; do (cd lib/$d && make -j8); done
ls "$D/lib/libiotk.a" "$D/lib/libydiago.a" "$D/no_gpu/lib/libdevXlib.a"

# Yambo links with fcflags, not LDFLAGS. Its main is in C, so add -mlcmain
# here, after the bundled libraries (their Fortran-main tests break with it).
sed -i "s|^\(fcflags *=.*\)$|\1 -mlcmain=main$RPATH|" config/setup
make -j8 core
ls -l bin/yambo bin/ypp bin/p2y bin/a2y
echo "Done. Run $WORK/verify and the validation in a job (see README)."
