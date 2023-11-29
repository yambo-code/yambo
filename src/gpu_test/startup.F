
!======================
subroutine startup()
  !======================
  use gpu_loc_m
  implicit none
  !
  if (.not. gpu_linalg_init) then
    call gpu_linalg_setup()
  endif

  write(*,*)
#ifdef _PGI
  write(*,*) "     compiler : PGI"
#endif
#ifdef _GNU
  write(*,*) "     compiler : GNU"
#endif
#ifdef _NV
  write(*,*) "     compiler : NV-SDK"
#endif
  !
  write(*,*) "  Have_device    :", have_device
  write(*,*) "   Have_CUDAF    :", have_cudaf
  write(*,*) " Have_OpenACC    :", have_openacc
  write(*,*) " Have_OpenMP_GPU :", have_ompgpu

end subroutine startup
