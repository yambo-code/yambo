#./configure \
#--enable-iotk \
#--enable-par-linalg \
#--with-iotk-path=/usr/local/src/codes/quantumespresso/qe-6.5-gcc-8.3.1/iotk \
#--with-blas-libs=/s/server/intel/compilers_and_libraries_2017.4.196/linux/mkl/lib/intel64 \
#--with-lapack-libs=/s/server/intel/compilers_and_libraries_2017.4.196/linux/mkl/lib/intel64 \
#--with-fft-libs=/s/server/intel/compilers_and_libraries_2017.4.196/linux/mkl/lib/intel64 \
#--with-blacs-libs=/s/server/intel/compilers_and_libraries_2017.4.196/linux/mkl/lib/intel64 \
#--with-scalapack-libs=/s/server/intel/compilers_and_libraries_2017.4.196/linux/mkl/lib/intel64 \

./configure \
--enable-iotk \
--with-iotk-path=/usr/local/src/codes/quantumespresso/qe-6.5-gcc-8.3.1/iotk \
--with-blas-libs="-L/s/server/intel/compilers_and_libraries_2017.4.196/linux/mkl/lib/intel64 -Wl,--no-as-needed -lmkl_scalapack_lp64 -lmkl_gf_lp64 -lmkl_sequential -lmkl_core -lmkl_blacs_openmpi_lp64 -lpthread -lm -ldl" \
--with-lapack-libs="-L/s/server/intel/compilers_and_libraries_2017.4.196/linux/mkl/lib/intel64 -Wl,--no-as-needed -lmkl_scalapack_lp64 -lmkl_gf_lp64 -lmkl_sequential -lmkl_core -lmkl_blacs_openmpi_lp64 -lpthread -lm -ldl" \
--with-fft-libs="-L/s/server/intel/compilers_and_libraries_2017.4.196/linux/mkl/lib/intel64 -Wl,--no-as-needed -lmkl_scalapack_lp64 -lmkl_gf_lp64 -lmkl_sequential -lmkl_core -lmkl_blacs_openmpi_lp64 -lpthread -lm -ldl" \
--with-blacs-libs="-L/s/server/intel/compilers_and_libraries_2017.4.196/linux/mkl/lib/intel64 -Wl,--no-as-needed -lmkl_scalapack_lp64 -lmkl_gf_lp64 -lmkl_sequential -lmkl_core -lmkl_blacs_openmpi_lp64 -lpthread -lm -ldl" \
--with-scalapack-libs="-L/s/server/intel/compilers_and_libraries_2017.4.196/linux/mkl/lib/intel64 -Wl,--no-as-needed -lmkl_scalapack_lp64 -lmkl_gf_lp64 -lmkl_sequential -lmkl_core -lmkl_blacs_openmpi_lp64 -lpthread -lm -ldl"
