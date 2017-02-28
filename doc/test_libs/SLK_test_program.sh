
cat << EOF > SLK_prog.f90
  program test_SLK
    call pcheev
  end program
EOF

# COMPILER #
FC="gfortran"

local_libs="${HOME}/libs/compiled/gnu_etsfmi_4.8.5"

####################
# Blas & SCALAPACK #
####################
# put here what you would use in --with-blas-libs=""
BLAS_LIBS="-L$local_libs/blas/ -lblas"
# put here what you would use in --with-lapack-libs=""
LAPACK_LIBS="-L$local_libs/lapack/ -llapack"

########
#  MPI #
########
# put here what you would use in --with-mpi-root=""
MPI_ROOT="$local_libs/openmpi-2.0"
# put here what you would use in --with-mpi-libs=""
MPI_LIBS="-L$local_libs/openmpi-2.0/lib/ -lmpi"

##########
# BLACS #
##########
# put here what you would use in --with-blacs-libs=""
BLACS_LIBS="-L$local_libs/blacs/ -lblacs -lblacs_C_init -lblacs_init"

###############
#  SCALAPACK  #
##############
# put here what you would use in --with-scalapack-libs=""
SLK_LIBS="-L$local_libs/scalapack/ -lscalapack"


$FC -o SLK_prog.x SLK_prog.f90 $BLACS_LIBS $MPI_LIBS $SLK_LIBS $LAPACK_LIBS $BLAS_LIBS -I"$MPI_ROOT/include"
