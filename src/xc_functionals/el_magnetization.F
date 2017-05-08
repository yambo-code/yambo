!
!        Copyright (C) 2000-2017 the YAMBO team
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
 use electrons,      ONLY:Total_magn
 use matrix_operate, ONLY:m3det
 use parallel_m,     ONLY:PP_redux_wait,PAR_IND_WF_linear,PAR_COM_density
#if defined _RT
 use drivers,       ONLY:l_real_time
#endif
#if defined _TIMING && defined _RT
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
 integer :: isym,ifft,ib,ik,i_wf_up,i_wf_dn
 real(SP):: cv(fft_size,3),tmp_sop(3,3),f_occ(n_sp_pol)
 !
#if defined _TIMING && defined _RT
 call timing('el_magnetization',OPR='start')
#endif
 !
 magn=0._SP
 cv=0._SP
 !
 Total_magn=0._SP
 !
 if (n_spin==1) return
 !
 do ik=1,Xk%nibz
   do ib=1,en%nbm
     !
     if (allocated(PAR_IND_WF_linear%element_2D)) then
       if (.not.PAR_IND_WF_linear%element_2D(ib,ik)) cycle
     endif
     !
     if (size(WF%index,1)<ib)        cycle
     if (size(WF%index,2)<ik)        cycle
     !
     i_wf_up=WF%index(ib,ik,1)
     i_wf_dn=WF%index(ib,ik,2)
     !
     if (i_wf_up==0 .or. i_wf_dn==0) cycle
     !
     f_occ=en%f(ib,ik,:)
#if defined _RT
     if(l_real_time.and.allocated(en%fo)) f_occ=en%fo(ib,ik,:)
#endif
     !
     if (n_spinor==2) then
       !
       ! mx
       !
       cv(:,1)=cv(:,1)+Xk%weights(ik)*(&
&              f_occ(1)*real(conjg(WF%c(:,i_wf_up))*WF%c(:,i_wf_dn))&
&             +f_occ(1)*real(conjg(WF%c(:,i_wf_dn))*WF%c(:,i_wf_up)) )
       !
       ! my
       !
       cv(:,2)=cv(:,2)+Xk%weights(ik)*(&
&              f_occ(1)*aimag(conjg(WF%c(:,i_wf_up))*WF%c(:,i_wf_dn))&
&             -f_occ(1)*aimag(conjg(WF%c(:,i_wf_dn))*WF%c(:,i_wf_up)) )
     endif
     !
     ! mz
     !
     cv(:,3)=cv(:,3)+Xk%weights(ik)*(&
&            f_occ(1)       *real(conjg(WF%c(:,i_wf_up))*WF%c(:,i_wf_up))&
&           -f_occ(n_sp_pol)*real(conjg(WF%c(:,i_wf_dn))*WF%c(:,i_wf_dn)) )
     !
   enddo
   !
   !
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
   Total_magn(:)=Total_magn(:)+magn(ifft,:)
 enddo
 !
#if defined _TIMING && defined _RT
 call timing('el_magnetization',OPR='stop')
#endif
 !
end subroutine
