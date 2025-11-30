! 
! License-Identifier: GPL
!
! Copyright (C) 2016 The Yambo Team
!
! Authors (see AUTHORS file for details): DS
!
subroutine electrons_spin_factors(kpts,bands,spin_factors)
 !
 use pars,           ONLY:SP
 use wave_func,      ONLY:WF
 use electrons,      ONLY:n_spinor
 use YPPm,           ONLY:WF_ref
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
 real(SP),      intent(out) :: spin_factors(bands(1):bands(2),kpts(1):kpts(2),n_spinor)
 !
 ! Work Space
 ! 
 integer               :: ik,ib,i_wf,ib_ref,ik_ref
 real(SP)              :: ref_dir(2,2),v(2)
 type(PP_indexes)      :: px
 !
 ref_dir(:,1)=(/1._SP,0._SP/)
 ref_dir(:,2)=(/0._SP,1._SP/)
 !
 ib_ref=WF_ref(1)
 ik_ref=WF_ref(2)
 !
 if(ib_ref>0.and.ik_ref>0) then
   !
   call WF_load(WF,0,1,(/ib_ref,ib_ref/),(/ik_ref,ik_ref/),title='-Spinor Factor',impose_free_and_alloc=.TRUE.)
   !
   i_wf=WF%index(ib_ref,ik_ref,1)
   v(1)=real(Vstar_dot_V_omp( fft_size, WF%r(:,1,i_wf ), WF%r(:,1,i_wf) ),SP)
   v(2)=real(Vstar_dot_V_omp( fft_size, WF%r(:,2,i_wf ), WF%r(:,2,i_wf) ),SP)
   !
   call WF_free(WF)
   !
   ref_dir(:,1)=v(:)/sqrt(dot_product(v,v))
   !
   v=ref_dir(:,1)*dot_product(ref_dir(:,1),ref_dir(:,2))
   !
   if(sqrt(dot_product(v,v))>1.E-5) then
     v(:)=-v(:)+ref_dir(:,2)
     ref_dir(:,2)=v(:)/sqrt(dot_product(v,v))
   endif
   !
 endif
 !
 call PP_indexes_reset(px)
 call PARALLEL_index(px,(/kpts(2)-kpts(1)+1,bands(2)-bands(1)+1/))
 !
 spin_factors=0._SP
 !
 do ik=kpts(1),kpts(2)
   if( .not.any(px%element_2D(ik-kpts(1)+1,:)) ) then
     if (ik==kpts(1)) call live_timing('Spinor Components',px%n_of_elements(myid+1))
     cycle
   endif
   call WF_load(WF,0,1,bands,(/ik,ik/),title='-Spinor-factor',impose_free_and_alloc=.TRUE.)
   if (ik==kpts(1)) call live_timing('Spinor Components',px%n_of_elements(myid+1))
   do ib=bands(1),bands(2)
     if( .not. px%element_2D(ik-kpts(1)+1,ib-bands(1)+1) ) cycle
     i_wf=WF%index(ib,ik,1)
     v(1)=real(Vstar_dot_V_omp( fft_size, WF%r(:,1,i_wf), WF%r(:,1,i_wf) ),SP)
     v(2)=real(Vstar_dot_V_omp( fft_size, WF%r(:,2,i_wf), WF%r(:,2,i_wf) ),SP)
     spin_factors(ib,ik,1)=abs(dot_product(v,ref_dir(:,1)))
     spin_factors(ib,ik,2)=abs(dot_product(v,ref_dir(:,2)))
     call live_timing(steps=1)
   enddo
   call WF_free(WF)
 enddo
 call live_timing()
 !
 call PP_redux_wait(spin_factors)
 !
end subroutine
