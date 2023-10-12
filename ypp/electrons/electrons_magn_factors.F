!
! License-Identifier: GPL
!
! Copyright (C) 2016 The Yambo Team
!
! Authors (see AUTHORS file for details):
!
subroutine electrons_magn_factors(kpts,bands,magn_factors)
 !
 use pars,           ONLY:SP
 use wave_func,      ONLY:WF
 use FFT_m,          ONLY:fft_size
 use LIVE_t,         ONLY:live_timing
 use interfaces,     ONLY:WF_load,WF_free
 use parallel_m,     ONLY:PP_indexes,myid,PP_indexes_reset
 use parallel_int,   ONLY:PP_redux_wait,PARALLEL_index 
 use wrapper_omp,    ONLY:Vstar_dot_V_omp
 !
 implicit none
 !
 integer,       intent(in)  :: kpts(2),bands(2)
 real(SP),      intent(out) :: magn_factors(bands(1):bands(2),kpts(1):kpts(2),3)
 !
 ! Work Space
 ! 
 integer               :: ik,ib,i_wf
 type(PP_indexes)      :: px
 !
 call PP_indexes_reset(px)
 call PARALLEL_index(px,(/kpts(2)-kpts(1)+1,bands(2)-bands(1)+1/))
 !
 magn_factors=0._SP
 !
 do ik=kpts(1),kpts(2)
   !
   if( .not.any(px%element_2D(ik-kpts(1)+1,:)) ) then
     if (ik==kpts(1)) call live_timing('Magn-factor',px%n_of_elements(myid+1))
     cycle
   endif
   !
   call WF_load(WF,0,1,bands,(/ik,ik/),title='-Magn-factor',impose_free_and_alloc=.TRUE.)
   !
   if (ik==1) call live_timing('Magn-factor',px%n_of_elements(myid+1))
   !
   do ib=bands(1),bands(2)
     if( .not. px%element_2D(ik-kpts(1)+1,ib-bands(1)+1) ) cycle
     i_wf=WF%index(ib,ik,1)
     ! x
     magn_factors(ib,ik,1)=real(Vstar_dot_V_omp( fft_size, WF%c(:, 1,i_wf), WF%c(:,2,i_wf) ),SP) &
&                         +real(Vstar_dot_V_omp( fft_size, WF%c(:, 2,i_wf), WF%c(:,1,i_wf) ),SP)
     ! y
     magn_factors(ib,ik,2)=aimag(Vstar_dot_V_omp( fft_size, WF%c(:,1,i_wf), WF%c(:,2,i_wf) )) &
&                         -aimag(Vstar_dot_V_omp( fft_size, WF%c(:,2,i_wf), WF%c(:,1,i_wf) ))
     ! z
     magn_factors(ib,ik,3)=real(Vstar_dot_V_omp( fft_size, WF%c(:,1,i_wf), WF%c(:,1,i_wf) ),SP) &
&                         -real(Vstar_dot_V_omp( fft_size, WF%c(:,2,i_wf), WF%c(:,2,i_wf) ),SP)
     !
     call live_timing(steps=1)
   enddo
   call WF_free(WF)
 enddo
 call live_timing()
 !
 call PP_redux_wait(magn_factors)
 !
end subroutine
