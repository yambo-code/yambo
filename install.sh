PETSC_DIR=/Users/henrique.pereira/software/petsc-3.6.4/
SLEPC_DIR=/Users/henrique.pereira/software/slepc-3.6.3/
./configure \
 --enable-keep-src \
 --enable-debug \
 --enable-msgs-comps \
 --without-editor \
 --with-fft-libs="-L/opt/local/lib -lfftw3 -lfftw3f"\
 --with-netcdf-libs="-L/opt/local/lib -lnetcdff -L/opt/local/lib -Wl,-headerpad_max_install_names -lnetcdf -lnetcdf -I/opt/local/include -pipe -Os -L/opt/local/lib /opt/local/lib/libhdf5_hl.a /opt/local/lib/libhdf5.a -L/opt/local/lib -Wl,-headerpad_max_install_names -lz -ldl -lm -lcurl"\
 --with-netcdf-includedir="/opt/local/include"\
 --with-blas-libs="-L/opt/local/lib -lopenblas"\
 --with-lapack-libs="-L/opt/local/lib -lopenblas -L/Users/henrique.pereira/software/petsc-3.6.4/arch-darwin-c-debug/lib -L/Users/henrique.pereira/software/slepc-3.6.3/arch-darwin-c-debug/lib -lpetsc -lslepc"\
 --with-iotk-path="/Users/henrique.pereira/software/espresso-5.2.1/iotk"\
 --with-libxc-libs="-L/usr/local/lib -lxc"\
 --with-libxc-includedir="/usr/local/include"\
 CPP="cpp-mp-4.7 -P -traditional -D_apple  -D_MPI -D_FFTW -I$SLEPC_DIR/include -I$PETSC_DIR/include -I$PETSC_DIR/arch-darwin-c-debug/include/ -I$SLEPC_DIR/arch-darwin-c-debug/include -I/opt/local/include/mpich-gcc47/"\
 FCCPP="cpp-mp-4.7"\
 CC="gcc-mp-4.7"\
 FC="gfortran-mp-4.7"\
 F77="gfortran-mp-4.7"\
 PFC="mpif90"\
 MPICC="mpicc"
