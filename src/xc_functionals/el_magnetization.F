!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details):
!
subroutine el_magnetization(en,Xk,magn,bands)
 !
 ! Electronic magnetization for electronic Spinors
 !
 !  SD = Sum_I (PSI^I)* Sigma_Pauli* PSI^I  I=occupied states
 !
 !  PSI^I = spinor
 !  Sigma_Pauli=the 3 Pauli matrices
 !
 use pars,           ONLY:SP
 use R_lattice,      ONLY:bz_samp
 use electrons,      ONLY:levels,n_spin,n_spinor,n_sp_pol
 use D_lattice,      ONLY:nsym,dl_sop,i_time_rev
 use FFT_m,          ONLY:fft_size,fft_rot_r
 use wave_func,      ONLY:WF
 use interfaces,     ONLY:WF_load,WF_free
 use hamiltonian,    ONLY:WF_G_max,WF_Go_indx
 use electrons,      ONLY:Spin_magn
 use matrix_operate, ONLY:m3det
 use parallel_m,     ONLY:PAR_IND_WF_linear,PAR_COM_density
 use parallel_int,   ONLY:PP_redux_wait
#if defined _RT
 use drivers,       ONLY:l_real_time
 use timing_m,       ONLY:timing
#endif
 !
 implicit none
 !
#include<yambo_wfs.h>
 !
 type(levels), intent(in ) ::en
 type(bz_samp),intent(in ) ::Xk
 real(SP),     intent(out) ::magn(fft_size,3)
 integer,  optional, intent(in) ::bands(2)
 !
 ! Work Space
 !
 logical :: l_WFs_on_the_fly
 integer :: isym,ifft,ib,ik,i_sp_pol,i_wf,bands_range(2)
 real(SP):: cv(fft_size,3),tmp_sop(3,3),f_occ
 !
#if defined _RT
 call timing('el_magnetization',OPR='start')
#endif
 !
 magn=0._SP
 cv=0._SP
 !
 Spin_magn=0._SP
 !
 if (n_spin==1) return
 !
 l_WFs_on_the_fly=.not.allocated(WF%r)
 !
 do i_sp_pol=1,n_sp_pol
   bands_range=(/1,en%nbm(i_sp_pol)/)
   if(present(bands)) bands_range=bands
   do ik=1,Xk%nibz
     !
     if (l_WFs_on_the_fly) then
       call WF_load(WF,WF_G_max,WF_Go_indx,bands_range,(/ik,ik/),(/i_sp_pol,i_sp_pol/),&
&                   space='R',title='-mang',keep_states_to_load=.true.)
     endif
     !
     do ib=bands_range(1),bands_range(2)
       !
       if (allocated(PAR_IND_WF_linear%element_2D)) then
         if (.not.PAR_IND_WF_linear%element_2D(ib,ik)) cycle
       endif
       !
       if (size(WF%index)<ib)        cycle
       if (size(WF%index)<ik)        cycle
       if (size(WF%index)<i_sp_pol)  cycle
       !
       i_wf =WF%index(ib,ik,i_sp_pol)
       !
       if (i_wf==0) cycle
       !
       f_occ=en%f(ib,ik,i_sp_pol)
#if defined _RT
       if(l_real_time.and.allocated(en%fo)) f_occ=en%fo(ib,ik,i_sp_pol)
#endif
       !
       if (n_sp_pol==2) then
         !
         ! mz
         !
         cv(:,3)=cv(:,3)+Xk%weights(ik)*(-1)**(i_sp_pol-1)*f_occ*real(wfconjg(WF%r(:,1,i_wf))*WF%r(:,1,i_wf))
         !
       else if(n_spinor==2) then
         !
         !
         ! mx
         !
         cv(:,1)=cv(:,1)+Xk%weights(ik)*f_occ*(&
&                   +real(wfconjg(WF%r(:,1,i_wf))*WF%r(:,2,i_wf))&
&                   +real(wfconjg(WF%r(:,2,i_wf))*WF%r(:,1,i_wf)) )
         !
         ! my
         !
         cv(:,2)=cv(:,2)+Xk%weights(ik)*f_occ*(&
&                   +wfaimag(wfconjg(WF%r(:,1,i_wf))*WF%r(:,2,i_wf))&
&                   -wfaimag(wfconjg(WF%r(:,2,i_wf))*WF%r(:,1,i_wf)) )
         !
         ! mz
         !
         cv(:,3)=cv(:,3)+Xk%weights(ik)*f_occ*(&
&                   +real(wfconjg(WF%r(:,1,i_wf))*WF%r(:,1,i_wf))&
&                   -real(wfconjg(WF%r(:,2,i_wf))*WF%r(:,2,i_wf)) )
         !
       endif
       !
     enddo
     !
     if (l_WFs_on_the_fly) call WF_free(WF,keep_fft=.true.)
     !
   enddo
 enddo
 !
 if (allocated(PAR_IND_WF_linear%element_2D)) then
   call PP_redux_wait(cv,COMM=PAR_COM_density%COMM)
 endif
 ! 
 ! Symmetrization
 !
 do isym=1,nsym
   ! The magnetization, like the spin, is a pseudo-vector:
   ! i.e. is invariant under spatial inversion but changes under T-rev
   if (n_spinor==2) then
     tmp_sop(:,:)=dl_sop(:,:,isym)*m3det(dl_sop(:,:,isym))
     if( isym> nsym/(1+i_time_rev) ) tmp_sop(:,:)=-tmp_sop(:,:)
     forall(ifft=1:fft_size) magn(ifft,:)=magn(ifft,:)+ &
&                            matmul(tmp_sop, cv(fft_rot_r(ifft,isym),:)/real(nsym,SP))
   else
     magn(:,3)=magn(:,3)+cv(fft_rot_r(:,isym),3)/real(nsym,SP)
   endif
   !
 enddo
 !
 do ifft=1,fft_size
   Spin_magn(:)=Spin_magn(:)+magn(ifft,:)
 enddo
 !
#if defined _RT
 call timing('el_magnetization',OPR='stop')
#endif
 !
end subroutine
