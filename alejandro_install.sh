./configure \
 --enable-keep-src \
 --with-iotk-path="/Users/fulvio.paleari/software/q-e/iotk" \
 --enable-internal-fftsg \
 --with-extlibs-path="$PWD/external_libs"\
 --with-netcdf-libs="-L/opt/local/lib -lnetcdff -L/opt/local/lib -Wl,-headerpad_max_install_names -lnetcdf -lnetcdf"\
 --with-netcdf-includedir="/opt/local/include"\
 --with-blas-libs="-L/opt/local/lib -lopenblas"\
 --with-lapack-libs="-L/opt/local/lib -lopenblas"\
 --enable-debug \
 --without-editor \
 --enable-msgs-comps \
 CC="gcc" \
 FC="gfortran" \
 F77="gfortran" \
 MPIFC="mpif90" \
 CPP="cpp" \
 MPICC="mpicc"
