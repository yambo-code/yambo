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
subroutine el_magnetization_matrix(G_lesser,en,Xk,magn,lowest_band)
 !
 ! Electronic magnetization for electronic Spinors
 !
 !  SD = Sum_I (PSI^I)* Sigma_Pauli* PSI^I  I=occupied states
 !
 !  PSI^I = spinor
 !  Sigma_Pauli=the 3 Pauli matrices
 !
 use pars,           ONLY:SP,cI,cZERO
 use parallel_m,     ONLY:PAR_G_kpts
 use R_lattice,      ONLY:bz_samp
 use electrons,      ONLY:levels,n_spin,n_spinor,n_sp_pol
 use D_lattice,      ONLY:nsym,dl_sop,i_time_rev
 use FFT_m,          ONLY:fft_size,fft_rot_r
 use interfaces,     ONLY:WF_load,WF_free
 use hamiltonian,    ONLY:WF_G_max,WF_Go_indx
 use wave_func,      ONLY:WF,WAVEs
 use real_time,      ONLY:RT_ibz_coo,RT_Nk_ibz,RT_bands
 use parallel_m,     ONLY:PP_redux_wait,PAR_IND_WF_k,PAR_IND_B_mat_ordered,PAR_IND_WF_linear
 use matrix_operate, ONLY:UP_matrix_index,m3det
 use electrons,      ONLY:Spin_magn
 use timing_m,       ONLY:timing
 !
 implicit none
 !
 type(bz_samp), intent(in)  :: Xk
 complex(SP),   intent(in)  :: G_lesser(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),PAR_G_kpts(1):PAR_G_kpts(2),n_sp_pol)
 integer,       intent(in)  :: lowest_band
 type(levels),  intent(in)  :: en       
 real(SP),      intent(out) :: magn(fft_size,3)
 !
 ! Work Space
 !
 integer :: ib1,ib2,i_sp_pol,ik,ik_RT,isym,ifft,i_wf1,i_wf2
 real(SP):: cv(fft_size,3),tmp_sop(3,3)
 complex(SP) :: f_occ
 type(WAVEs) :: WF_tmp
 !
 call timing('el_magn_matrix',OPR='start')
 !
 magn=0._SP
 cv=  0._SP
 !
 if (n_spin==1) return
 !
 if (n_sp_pol==2) call error(' case n_sp_pol=2 not yet coded in yambo_rt')
 !
 if(lowest_band<RT_bands(1)-1) then
   !
   do i_sp_pol=1,n_sp_pol
     do ik=1,Xk%nibz
       if (.not.any(PAR_IND_WF_linear%element_2D(:,ik))) cycle
       call WF_load(WF_tmp,WF_G_max,WF_Go_indx,(/lowest_band,RT_bands(1)-1/),(/ik,ik/),(/i_sp_pol,i_sp_pol/),&
       &                                 space='R',title='-RT',keep_states_to_load=.true.)
       do ib1=lowest_band,RT_bands(1)-1
         !
         i_wf1=WF_tmp%index(ib1,ik,i_sp_pol)
         !
         f_occ=cmplx(en%f(ib1,ik,i_sp_pol),0._SP)
         !
         if (i_wf1==0) cycle
         !
         if (n_sp_pol==2) then
           !
           ! mz
           !
           cv(:,3)=cv(:,3)+Xk%weights(ik)*(-1)**(i_sp_pol-1)*real(f_occ)*real(conjg(WF_tmp%c(:,1,i_wf1))*WF_tmp%c(:,1,i_wf1))
           !
         else if(n_spinor==2) then
           !
           ! mx
           !
           cv(:,1)=cv(:,1)+Xk%weights(ik)*real(f_occ)*(&
&                     +real(conjg(WF_tmp%c(:,1,i_wf1))*WF_tmp%c(:,2,i_wf1))&
&                     +real(conjg(WF_tmp%c(:,2,i_wf1))*WF_tmp%c(:,1,i_wf1)) )
           !
           ! my
           !
           cv(:,2)=cv(:,2)+Xk%weights(ik)*real(f_occ)*(&
&                     +aimag(conjg(WF_tmp%c(:,1,i_wf1))*WF_tmp%c(:,2,i_wf1))&
&                     -aimag(conjg(WF_tmp%c(:,2,i_wf1))*WF_tmp%c(:,1,i_wf1)) )
           !
           ! mz
           !
           cv(:,3)=cv(:,3)+Xk%weights(ik)*real(f_occ)*(&
&                     +real(conjg(WF_tmp%c(:,1,i_wf1))*WF_tmp%c(:,1,i_wf1))&
&                     -real(conjg(WF_tmp%c(:,2,i_wf1))*WF_tmp%c(:,2,i_wf1)) )
           !
         endif
         !
       enddo
     enddo
     call WF_free(WF_tmp,keep_fft=.true.)
   enddo
   !  
 endif
 !
 do ik=1,Xk%nibz
   !
#if !defined _YPP_RT
   if (.not.PAR_IND_WF_k%element_1D(ik)) cycle
#endif
   !      
   do ib1=RT_bands(1),RT_bands(2)
     !
#if defined _YPP_RT
     if (.not.PAR_IND_WF_linear%element_2D(ib1,ik)) cycle
#endif
     !
     do ib2=ib1,RT_bands(2)
       !
#if !defined _YPP_RT
       if (.not.PAR_IND_B_mat_ordered%element_1D(  UP_matrix_index(ib1-RT_bands(1)+1,ib2-RT_bands(1)+1)-1 )) cycle
#endif
       !
       f_occ=cZERO
       do ik_RT=RT_ibz_coo(ik,1),RT_ibz_coo(ik,2)
         if (ib1==ib2) f_occ=f_occ-cI*G_lesser(ib1,ib2,ik_RT,1)
         if (ib1/=ib2) f_occ=f_occ-cI*G_lesser(ib1,ib2,ik_RT,1)*2._SP
       enddo
       f_occ=f_occ/real(RT_Nk_ibz(ik),SP)
       !
       i_wf1=WF%index(ib1,ik,1)
       i_wf2=WF%index(ib2,ik,1)
       !
       if (n_spinor==2) then
         !
         ! mx
         !
         cv(:,1)=cv(:,1)+Xk%weights(ik)*(&
&                real(f_occ*conjg(WF%c(:,1,i_wf1))*WF%c(:,2,i_wf2))&
&               +real(f_occ*conjg(WF%c(:,2,i_wf1))*WF%c(:,1,i_wf2)) )
         !
         ! my
         !
         cv(:,2)=cv(:,2)+Xk%weights(ik)*(&
&                aimag(f_occ*conjg(WF%c(:,1,i_wf1))*WF%c(:,2,i_wf2))&
&               -aimag(f_occ*conjg(WF%c(:,2,i_wf1))*WF%c(:,1,i_wf2)) )
         !
         ! mz
         !
         cv(:,3)=cv(:,3)+Xk%weights(ik)*(&
&                real(f_occ*conjg(WF%c(:,1,i_wf1))*WF%c(:,1,i_wf2))&
&               -real(f_occ*conjg(WF%c(:,2,i_wf1))*WF%c(:,2,i_wf2)) )
       endif
       !
     enddo
   enddo
   !
 enddo
 !
 call PP_redux_wait(cv)
 !
 ! Symmetrization
 !
 do isym=1,nsym
   ! The magnetization, like the spin, is a pseudo-vector:
   ! i.e. is invariant under spatial invertion but changes under T-rev
   if (n_spinor==2) then
     tmp_sop(:,:)=dl_sop(:,:,isym)*m3det(dl_sop(:,:,isym))
     if( isym> nsym/(1+i_time_rev) ) tmp_sop(:,:)=-tmp_sop(:,:)
     forall(ifft=1:fft_size) magn(ifft,:)=magn(ifft,:)+ &
&                            matmul(tmp_sop, cv(fft_rot_r(ifft,isym),:)/real(nsym,SP) )
   else
     magn(:,3)=magn(:,3)+cv(fft_rot_r(:,isym),3)/real(nsym,SP)
   endif
   !
 enddo
 !
 if(lowest_band<RT_bands(1)-1) then
   do ifft=1,fft_size
     Spin_magn(:)=Spin_magn(:)+magn(ifft,:)
   enddo
 endif
 !
 call timing('el_magn_matrix',OPR='stop')
 !
end subroutine
