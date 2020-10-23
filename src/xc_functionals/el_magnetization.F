!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details):
!
! This file is distributed under the terms of the GNU
! General Public License. You can redistribute it and/or
! modify it under the terms of the GNU General Public
! License as published by the Free Software Foundation;
! either version 2, or (at your option) any later version.
!
! This program is distributed in the hope that it will
! be useful, but WITHOUT ANY WARRANTY; without even the
! implied warranty of MERCHANTABILITY or FITNESS FOR A
! PARTICULAR PURPOSE.  See the GNU General Public License
! for more details.
!
! You should have received a copy of the GNU General Public
! License along with this program; if not, write to the Free
! Software Foundation, Inc., 59 Temple Place - Suite 330,Boston,
! MA 02111-1307, USA or visit http://www.gnu.org/copyleft/gpl.txt.
!
subroutine el_magnetization(en,Xk,magn)
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
 type(levels), intent(in ) ::en
 type(bz_samp),intent(in ) ::Xk
 real(SP),     intent(out) ::magn(fft_size,3)
 !
 ! Work Space
 !
 integer :: isym,ifft,ib,ik,i_sp_pol,i_wf
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
 do i_sp_pol=1,n_sp_pol
   do ik=1,Xk%nibz
     do ib=1,en%nbm
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
#if defined _RT
 call timing('el_magnetization',OPR='stop')
#endif
 !
end subroutine
