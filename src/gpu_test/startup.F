
!======================
subroutine startup()
  !======================
  use gpu_m
  implicit none
  !
  if (.not. cuda_linalg_init) then
    call cuda_linalg_setup()
  endif

  write(*,*)
#ifdef __PGI
  write(*,*) "     compiler : PGI"
#endif
#ifdef __GNU
  write(*,*) "     compiler : GNU"
#endif
#ifdef __NV
  write(*,*) "     compiler : NV-SDK"
#endif
  !
  write(*,*) "  Have_device :", have_device
  write(*,*) "   Have_CUDAF :", have_cudaf
  write(*,*) " Have_OpenACC :", have_openacc
  write(*,*) " Have_OpenMP5 :", have_openmp5

end subroutine startup
