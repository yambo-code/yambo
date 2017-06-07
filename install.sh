./configure \
 --enable-keep-src \
 --enable-debug \
 --enable-msgs-comps \
 --without-editor \
 --with-fft-libs="-L/opt/local/lib -lfftw3 -lfftw3f"\
 --with-netcdf-libs="-L/opt/local/lib -lnetcdff -L/opt/local/lib -Wl,-headerpad_max_install_names -lnetcdf -lnetcdf"\
 --with-netcdf-includedir="/opt/local/include"\
 --with-blas-libs="-L/opt/local/lib -lopenblas"\
 --with-lapack-libs="-L/opt/local/lib -lopenblas"\
 --with-iotk-path="/Users/fulvio.paleari/software/q-e/iotk"\
 --with-libxc-libs="-L/opt/local/lib -lxc -lxcf90"\
 --with-libxc-includedir="/opt/local/include"\
 CPP="cpp"\
 FCCPP="cpp"\
 CC="gcc"\
 FC="gfortran"\
 F77="gfortran"\
 PFC="mpif90"\
 MPICC="mpicc"
