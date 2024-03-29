!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM DS
!
subroutine X_AVERAGE_do_it(what,X_par)
 !
 ! The procedures to copy from Host to Device, that are included here, maybe not needded.
 ! Indeed X_par is computed on the Device, but later transferred to Host and the Host version only is used
 ! So there is probably no need to have data also on the Device
 !
 use pars,          ONLY:cZERO,SP
 use cuda_m,        ONLY:have_cuda
 use X_m,           ONLY:X_par_average,n_OPTICAL_dir_to_eval
 use deviceXlib_m,  ONLY:dev_memcpy
 use matrix,        ONLY:PAR_matrix,MATRIX_reset
 !
 implicit none
 !
 character(*)     :: what
 type(PAR_matrix) :: X_par
 !
 if (what=="ACCUMULATE") then
   X_par_average%blc=X_par_average%blc+X_par%blc*1._SP/real(n_OPTICAL_dir_to_eval,SP)
   X_par%blc=cZERO
   if (have_cuda) then
     ! DS+AF: This call assumes that Host and Device are syncronized
     !        It should be the case here, since blc_d is computed on the Device
     !        in X_redux, and then transferred to the Host before exiting 
     call dev_memcpy(X_par_average%blc_d,X_par_average%blc)
     X_par%blc_d=cZERO
   endif
   return
 endif
 !
 if (what=="DUMP") then
   X_par%blc=X_par_average%blc
   if (have_cuda) X_par%blc_d=X_par_average%blc_d
   call MATRIX_reset(X_par_average)
   return
 endif
 !
end subroutine X_AVERAGE_do_it
