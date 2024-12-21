!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details):
!
subroutine el_magnetization(en,Xk,magn,occ_mode,bands)
 !
 ! Electronic magnetization for electronic Spinors
 !
 !  SD = Sum_I (PSI^I)* Sigma_Pauli* PSI^I  I=occupied states
 !
 !  PSI^I = spinor
 !  Sigma_Pauli=the 3 Pauli matrices
 !
 use pars,           ONLY:SP,schlen
 use R_lattice,      ONLY:bz_samp
 use electrons,      ONLY:levels,n_spin,n_spinor,n_sp_pol
 use D_lattice,      ONLY:nsym,dl_sop,i_time_rev
 use FFT_m,          ONLY:fft_size,fft_rot_r
 use wave_func,      ONLY:WF
 use electrons,      ONLY:Spin_magn
 use matrix_operate, ONLY:m3det
 use parallel_m,     ONLY:PAR_IND_WF_linear,PAR_COM_density
 use parallel_int,   ONLY:PP_redux_wait
 use timing_m,       ONLY:timing
 !
 implicit none
 !
 type(levels), intent(in ) ::en
 type(bz_samp),intent(in ) ::Xk
 real(SP),     intent(out) ::magn(fft_size,3)
 character(*), optional, intent(in) ::occ_mode
 integer,      optional, intent(in) ::bands(2)
 !
 ! Work Space
 !
 character(schlen) :: occ_mode_
 integer :: isym,ifft,ib,ik,i_sp_pol,i_wf,bands_range(2)
 real(SP):: cv(fft_size,3),tmp_sop(3,3),f_occ
 !
 call timing('el_magnetization',OPR='start')
 !
 magn=0._SP
 cv=0._SP
 !
 Spin_magn=0._SP
 !
 if (n_spin==1) return
 !
 occ_mode_="def"
 if (present(occ_mode)) occ_mode_=occ_mode
 !
 if (trim(occ_mode_)=="fo".and..not.allocated(en%fo)) &
 & call error("el_density: occ mode fo, but fo occupations not allocated")
 !
 do i_sp_pol=1,n_sp_pol
   bands_range=(/1,en%nbm(i_sp_pol)/)
   if(present(bands)) bands_range=bands
   do ik=1,Xk%nibz
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
       if(occ_mode_=="def") f_occ=en%f(ib,ik,i_sp_pol)
       if(occ_mode_=="fo")  f_occ=en%fo(ib,ik,i_sp_pol)
       !
       if (n_sp_pol==2) then
         !
         ! mz
         !
         cv(:,3)=cv(:,3)+Xk%weights(ik)*(-1)**(i_sp_pol-1)*f_occ*real(conjg(WF%c(:,1,i_wf))*WF%c(:,1,i_wf))
         !
       else if(n_spinor==2) then
         !
         !
         ! mx
         !
         cv(:,1)=cv(:,1)+Xk%weights(ik)*f_occ*(&
&                   +real(conjg(WF%c(:,1,i_wf))*WF%c(:,2,i_wf))&
&                   +real(conjg(WF%c(:,2,i_wf))*WF%c(:,1,i_wf)) )
         !
         ! my
         !
         cv(:,2)=cv(:,2)+Xk%weights(ik)*f_occ*(&
&                   +aimag(conjg(WF%c(:,1,i_wf))*WF%c(:,2,i_wf))&
&                   -aimag(conjg(WF%c(:,2,i_wf))*WF%c(:,1,i_wf)) )
         !
         ! mz
         !
         cv(:,3)=cv(:,3)+Xk%weights(ik)*f_occ*(&
&                   +real(conjg(WF%c(:,1,i_wf))*WF%c(:,1,i_wf))&
&                   -real(conjg(WF%c(:,2,i_wf))*WF%c(:,2,i_wf)) )
         !
       endif
       !
     enddo
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
 call timing('el_magnetization',OPR='stop')
 !
end subroutine
