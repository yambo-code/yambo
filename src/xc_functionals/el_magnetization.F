!
! Copyright (C) 2000-2016 D. De Fausti and the YAMBO team
!              http://www.yambo-code.org
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
subroutine el_magnetization(en,Xk)
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
 use D_lattice,      ONLY:nsym,DL_vol,dl_sop,i_time_rev,inv_index
 use FFT_m,          ONLY:fft_size,fft_rot_r
 use wave_func,      ONLY:wf_state,wf
 use xc_functionals, ONLY:magn
 !
 use electrons,      ONLY:Total_magn
 use D_lattice,      ONLY:DL_vol
 use matrix_operate, ONLY:m3det
 !
 implicit none
 type(levels) ::en
 type(bz_samp)::Xk
 !
 ! Work Space
 !
 integer :: i1,i2,ifft_up,ifft_dn
 real(SP):: cv(fft_size,3),tmp_sop(3,3)
 !
 magn=0.
 cv=0.
 !
 Total_magn=0.
 !
 if (n_spin==1) return
 !
 do i1=1,en%nbm
   do i2=1,Xk%nibz
     !
     ifft_up=wf_state(i1,i2,1)
     ifft_dn=wf_state(i1,i2,2)
     !
     if (ifft_up==0 .or. ifft_dn==0) cycle
     if (n_spinor==2) then
       !
       ! mx
       !
       cv(:,1)=cv(:,1)+Xk%weights(i2)*(&
&              en%f(i1,i2,1)*conjg(wf(:,ifft_up))*wf(:,ifft_dn)&
&             +en%f(i1,i2,1)*conjg(wf(:,ifft_dn))*wf(:,ifft_up) )
       !
       ! my
       !
       cv(:,2)=cv(:,2)+Xk%weights(i2)*(0._SP,-1._SP)*(&
&              en%f(i1,i2,1)*conjg(wf(:,ifft_up))*wf(:,ifft_dn)&
&             -en%f(i1,i2,1)*conjg(wf(:,ifft_dn))*wf(:,ifft_up) )
     endif
     !
     ! mz
     !
     cv(:,3)=cv(:,3)+Xk%weights(i2)*(&
&            en%f(i1,i2,1)*conjg(wf(:,ifft_up))*wf(:,ifft_up)&
&           -en%f(i1,i2,n_sp_pol)*conjg(wf(:,ifft_dn))*wf(:,ifft_dn) )
   enddo
 enddo
 !
 do i1=1,nsym
   ! The magnetization, like the spin, is a pseudo-vector:
   ! i.e. is invariant under spatial invertion but changes under T-rev
   if (n_spinor==2) then
     tmp_sop(:,:)=dl_sop(:,:,i1)*m3det(dl_sop(:,:,i1))
     if( i1> nsym/(1+i_time_rev) ) tmp_sop(:,:)=-tmp_sop(:,:)
     forall(i2=1:fft_size) magn(i2,:)=magn(i2,:)+ &
&                            matmul(tmp_sop,real( cv(fft_rot_r(i1,i2),:)/real(nsym) ))
   else
     magn(:,3)=magn(:,3)+real(cv(fft_rot_r(i1,:),3)/real(nsym))
   endif
   !
 enddo
 !
 do i1=1,fft_size
   Total_magn(:)=Total_magn(:)+magn(i1,:)
 enddo
 !
end subroutine
