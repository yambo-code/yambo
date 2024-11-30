  !
! License-Identifier: GPL
!
! Copyright (C) 2013 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
! headers
!
#include<dev_defs.h>
#include<y_memory.h>
!
subroutine WF_alloc(WF,k_extrema_only)
 !
 use pars,          ONLY:cZERO
 use wave_func,     ONLY:WAVEs,states_to_load,wf_ng,wf_ncx
 use FFT_m,         ONLY:fft_size
 use electrons,     ONLY:n_spinor
 use parallel_int,  ONLY:PARALLEL_live_message
 use gpu_m,         ONLY:have_gpu
 use devxlib,       ONLY:devxlib_memset_d
 !
 USE_MEMORY
 !
 implicit none
 !
 type(WAVEs)           :: WF
 logical, intent(in)   :: k_extrema_only
 !
 ! Work Space
 !
 integer :: ik,ib,i_sp_pol,NK,N_loaded,N_total,wf_grid_size
 !
 ! Distributed allocation
 !
 if(allocated(states_to_load)) then
   !
   WF%N=0
   N_loaded=0
   N_total=0
   do i_sp_pol=WF%sp_pol(1),WF%sp_pol(2)
     do ik=WF%k(1),WF%k(2)
       do ib=WF%b(1),WF%b(2)
         N_total=N_total+1
         if (states_to_load(ib,ik,i_sp_pol)) then
           N_loaded=N_loaded+1
           if (.not.k_extrema_only) WF%N=WF%N+1
           if (     k_extrema_only.and.(ik==WF%k(1).or.ik==WF%k(2))) WF%N=WF%N+1
	 endif
       enddo
     enddo
   enddo
   !
 else
   !
   NK=(WF%k(2)-WF%k(1)+1)
   if (k_extrema_only) NK=min(NK,2)
   WF%N=(WF%b(2)-WF%b(1)+1)*NK*(WF%sp_pol(2)-WF%sp_pol(1)+1)
   N_loaded=WF%N
   N_total=WF%N
   !
 endif
 !
 call PARALLEL_live_message("Wave-Function states",LOADED=N_loaded,TOTAL=N_total)
 !
 if (WF%space=='R')  wf_grid_size=fft_size
 if (WF%space=='G')  wf_grid_size=wf_ng
 if (WF%space=='C')  wf_grid_size=wf_ncx
 if (WF%space=='B')  wf_grid_size=wf_ncx
 !
 YAMBO_ALLOC(WF%c,(wf_grid_size,n_spinor,WF%N))
 if (have_gpu) then
   YAMBO_ALLOC_GPU(DEV_VAR(WF%c),(wf_grid_size,n_spinor,WF%N))
 endif
 !
 YAMBO_ALLOC(WF%index,(WF%b(2),WF%k(2),WF%sp_pol(2)))
 !
 WF%c=cZERO
 if (have_gpu) call devxlib_memset_d(DEV_VAR(WF%c),cZERO)
 !
 WF%index=0
 !
end subroutine
