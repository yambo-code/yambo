program verify_native
  use omp_lib
  use ieee_arithmetic
  implicit none
  include 'mpif.h'
  integer :: ierr, rank, ranks, hostname_length, threads, local_ok, global_ok
  character(MPI_MAX_PROCESSOR_NAME) :: hostname
  complex(8) :: a(3), b(3), dotc_value, dotu_value, mat(3,3), product(3)
  complex(8), external :: zdotc, zdotu
  real(8) :: nan_value
  logical :: dotc_ok, dotu_ok, gemv_ok, nan_ok
  call MPI_Init(ierr)
  call MPI_Comm_rank(MPI_COMM_WORLD,rank,ierr)
  call MPI_Comm_size(MPI_COMM_WORLD,ranks,ierr)
  call MPI_Get_processor_name(hostname,hostname_length,ierr)
  a = [(1d0,2d0),(-3d0,4d0),(5d0,-6d0)]
  b = [(2d0,-1d0),(4d0,3d0),(-6d0,5d0)]
  dotc_value = zdotc(3,a,1,b,1)
  dotu_value = zdotu(3,a,1,b,1)
  dotc_ok = abs(dotc_value-dot_product(a,b)) < 1d-12
  dotu_ok = abs(dotu_value-sum(a*b)) < 1d-12
  mat(:,1)=a
  mat(:,2)=b
  mat(:,3)=a+b
  product=(0d0,0d0)
  call zgemv('N',3,3,(1d0,0d0),mat,3,b,1,(0d0,0d0),product,1)
  gemv_ok = maxval(abs(product-matmul(mat,b))) < 1d-12
  nan_value=ieee_value(0d0,ieee_quiet_nan)
  nan_ok = nan_value /= nan_value
  threads=0
!$omp parallel reduction(+:threads)
  threads=threads+1
!$omp end parallel
  local_ok=0
  if (dotc_ok.and.dotu_ok.and.gemv_ok.and.nan_ok.and.threads==omp_get_max_threads()) local_ok=1
  call MPI_Allreduce(local_ok,global_ok,1,MPI_INTEGER,MPI_SUM,MPI_COMM_WORLD,ierr)
  print *, 'rank=',rank,'host=',trim(hostname),'threads=',threads
  print *, 'ZDOTC=',dotc_value,'expected=',dot_product(a,b),'pass=',dotc_ok
  print *, 'ZDOTU=',dotu_value,'expected=',sum(a*b),'pass=',dotu_ok
  print *, 'ZGEMV pass=',gemv_ok,'NaN pass=',nan_ok
  if (rank==0) print *, 'NATIVE_PROBE_PASS=',global_ok==ranks,'ranks=',ranks
  call MPI_Finalize(ierr)
  if (global_ok/=ranks) stop 1
end program
