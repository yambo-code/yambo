!
! License-Identifier: GPL
!
! Copyright (C) 2013 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine WF_alloc(WF)
 !
 use pars,          ONLY:cZERO
 use wave_func,     ONLY:WAVEs,states_to_load,wf_ng,wf_ncx
 use FFT_m,         ONLY:fft_size
 use electrons,     ONLY:n_spinor
 use parallel_int,  ONLY:PARALLEL_live_message
 use cuda_m,        ONLY:have_cuda
 !
#include<memory.h>
 !
 type(WAVEs)           :: WF
 !
 ! Work Space
 !
 integer :: ik,ib,i_sp_pol,N_total,wf_grid_size
 !
 ! Distributed allocation
 !
 if(allocated(states_to_load)) then
   !
   WF%N=0
   N_total=0
   do i_sp_pol=WF%sp_pol(1),WF%sp_pol(2)
     do ik=WF%k(1),WF%k(2)
       do ib=WF%b(1),WF%b(2)
         N_total=N_total+1
         if (states_to_load(ib,ik,i_sp_pol)) WF%N=WF%N+1
       enddo
     enddo
   enddo
   !
 else
   !
   WF%N=(WF%b(2)-WF%b(1)+1)*(WF%k(2)-WF%k(1)+1)*(WF%sp_pol(2)-WF%sp_pol(1)+1)
   N_total=WF%N
   !
 endif
 !
 if (WF%space=='R')  wf_grid_size=fft_size
 if (WF%space=='G')  wf_grid_size=wf_ng
 if (WF%space=='C')  wf_grid_size=wf_ncx
 if (WF%space=='B')  wf_grid_size=wf_ncx
 !
 YAMBO_ALLOC(WF%c,(wf_grid_size,n_spinor,WF%N))
 if (have_cuda) then
   YAMBO_ALLOC_MOLD(WF%c_d,WF%c)
 endif
 !
 call PARALLEL_live_message("Wave-Function states",LOADED=WF%N,TOTAL=N_total)
 !
 YAMBO_ALLOC(WF%index,(WF%b(2),WF%k(2),WF%sp_pol(2)))
 !
 WF%c=cZERO
 if (have_cuda) WF%c_d=cZERO
 !
 WF%index=0
 !
end subroutine
