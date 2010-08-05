!
! Copyright (C) 2000-2010 A. Marini and the YAMBO team
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
subroutine SC_add_XC(X,Xw,Xk,E,k,q,iteration,V_xc_nm1)
 !
 ! This routine is common between _SC and _RT. It provides the XC part of 
 ! the hamiltonian 
 !
 ! The definition of the XC part is the same in the two approches. The only
 ! difference is that in _XC the operator is written in the SC basis, while
 ! _RT works in the LDa basis.
 !
 use pars,           ONLY:SP
 use drivers,        ONLY:l_sc_exx,l_sc_srpa,l_sc_cohsex,l_sc_contains_lda,&
&                         l_collisions_IO
 use R_lattice,      ONLY:bz_samp
 use QP_m,           ONLY:QP_t,QP_nk,QP_reset
 use electrons,      ONLY:levels,n_spin
 use frequency,      ONLY:w_samp
 use X_m,            ONLY:X_t
 use xc_functionals, ONLY:V_xc
 use SC,             ONLY:V_xc_sc,local_V,SC_kind,SC_xc_functional,SC_cycle_mixing,&
&                         G_lesser,SC_bands,SC_R,H_rotate
 use FFT_m,          ONLY:fft_size
 !
 implicit none
 !
 type(levels) ::E
 type(bz_samp)::k,q,Xk
 type(X_t)    ::X(2)  ! Xx Em1s
 type(w_samp) ::Xw(2)
 !
 integer      ::iteration
 real(SP)     ::V_xc_nm1(fft_size,n_spin)
 !
 ! Work Space
 !
 integer      :: ib,ik
 type(QP_t)   :: qp_dummy
 !
 if (.not.local_V.or.l_sc_exx) then
   !
   ! Hartee-Fock & COHSEX
   !======================
   !
   if(l_collisions_IO) then
     !     
     G_lesser=(0.,0.)
     forall(ib=1:SC_bands,ik=1:E%nk) G_lesser(ib,ib,ik)=(0._SP,1._SP)*E%f(ib,ik,1)
     !
     ! Now I build the G_lesser rotating the non interaction one
     ! G_lesser(ib,ib,ik)=(0.,1)*en%f(ib,ik)
     !  
     do ik=1,QP_nk
       call H_rotate(SC_R(:,:,ik,1),G_lesser(:,:,ik),SC_bands,-1)
     enddo
     call COLLISIONS_IO_compose(G_lesser)
     !    ==========
   else
     !
     call QP_reset(qp_dummy)
     !
     ! Stop all messagging
     !
     call IO_and_Messaging_switch("-report -io_in -io_out")
     !
     call XCo_driver(E,k,k,q)
     !
     call IO_and_Messaging_switch("+io_in")
     !
     if (l_sc_cohsex.or.l_sc_srpa) call QP_ppa_cohsex(X(2),Xk,E,k,q,qp_dummy,Xw(2),-1)
     !
     call IO_and_Messaging_switch("+report +io_out")
     !
     !
   endif
   !
 endif
 !
 if (local_V) then
   !
   V_xc_sc=(0.,0.)
   !
   if (l_sc_exx) call V_oep(X(1),E,k,q,V_xc_sc)
   !
   call IO_and_Messaging_switch("-report -log -io_in -io_out")
   !
   if (l_sc_contains_lda) call XC_potential_driver(E,Xk,SC_kind,SC_xc_functional,1)
   if (l_sc_contains_lda.and..not.l_sc_exx) V_xc_sc(:,:)=V_xc(:,:)
   if (l_sc_contains_lda.and.     l_sc_exx) V_xc_sc(:,:)=V_xc(:,:)+V_xc_sc(:,:)
   !
   call IO_and_Messaging_switch("+report +log +io_in +io_out")
   !
   ! Mixing 
   !========
   !
   ! WARNING:
   ! The mixing of the potential has been eliminated for HF and COHSEX, for
   ! V_xc_sc case tests are needed.
   ! 
   if (iteration>1) V_xc_sc = SC_cycle_mixing*V_xc_sc + (1.-SC_cycle_mixing)*V_xc_nm1 
   !
   V_xc_nm1=V_xc_sc
   !
 endif
 !
end subroutine SC_add_XC
