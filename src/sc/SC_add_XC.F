!
!        Copyright (C) 2000-2017 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM
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
 ! The definition of the XC part is the same in the two approches. The only
 ! difference is that in _XC the operator is written in the SC basis, while
 ! _RT works in the LDa basis.
 !
 use pars,           ONLY:SP,cZERO,cI
 use drivers,        ONLY:l_sc_exx,l_sc_srpa,l_sc_coh,l_sc_sex,l_sc_is_libDFT,&
&                         l_use_Hxc_collisions,l_sc_fock,l_use_COH_collisions
 use R_lattice,      ONLY:bz_samp
 use QP_m,           ONLY:QP_t,QP_reset
 use parallel_m,     ONLY:master_cpu
 use electrons,      ONLY:levels,n_spin
 use frequency,      ONLY:w_samp
 use X_m,            ONLY:X_t
 use xc_functionals, ONLY:V_xc,XC_potential_driver
 use hamiltonian,    ONLY:l_sc_V_is_local,l_sc_XC_is_local,V_xc_sc
 use SC,             ONLY:SC_kind,SC_xc_functional,SC_cycle_mixing,&
&                         SC_bands,SC_R,H_rotate,l_oep_iterative
 use FFT_m,          ONLY:fft_size
 use IO_m,           ONLY:IO_and_Messaging_switch
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
 complex(SP)  :: G_sex(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),E%nk)
 complex(SP)  :: G_coh(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),E%nk)
 !
 type(QP_t)   :: qp_dummy
 !
 ! Fock/SEX/Hartree + COH
 !=======================
 !
 if (l_use_Hxc_collisions.or.l_use_COH_collisions) then
   !     
   G_sex=cZERO
   forall(ib=SC_bands(1):SC_bands(2),ik=1:E%nk) G_sex(ib,ib,ik)=cI*E%f(ib,ik,1)
   G_coh=cZERO
   forall(ib=SC_bands(1):SC_bands(2),ik=1:E%nk) G_coh(ib,ib,ik)=cI
   !
   ! Now I build the G_collisions rotating the non interaction one
   ! G_collisions(ib,ib,ik)=(0.,1)*en%f(ib,ik)
   !
   do ik=1,E%nk
     call H_rotate(SC_R(:,:,ik,1),G_sex(:,:,ik),SC_bands(2),-1)
     call H_rotate(SC_R(:,:,ik,1),G_coh(:,:,ik),SC_bands(2),-1)
   enddo
   !
   call COLLISIONS_compose_sc(G_sex,G_coh)
   !
 else
   !
   if (.not.l_sc_V_is_local) then
     !
     call QP_reset(qp_dummy)
     !
     ! Stop all messagging
     !
     call IO_and_Messaging_switch("-report -io_in -io_out")
     !
     if (l_sc_sex.or.l_sc_fock) call XCo_driver(E,k,k,q)
     !
     call IO_and_Messaging_switch("+io_in")
     !
     if (l_sc_sex.or.l_sc_coh)  call QP_ppa_cohsex(X(2),Xk,E,k,q,qp_dummy,Xw(2),-1)
     !
     call IO_and_Messaging_switch("+report +io_out",CONDITION=master_cpu)
     !
   else if (l_sc_XC_is_local) then
     !
     if (.not.l_oep_iterative) V_xc_sc=0._SP
     !
     ! GPL_EXCLUDE_START
     !
     ! OEP's
     !=======
     !
     if (l_sc_exx.or.l_sc_srpa) call OEP_driver(X,Xw(2),E,k,q,V_xc_sc)
     !
     ! GPL_EXCLUDE_END
     !
     ! DFT-like 
     !==========
     !
     if (l_sc_is_libDFT) call XC_potential_driver(E,Xk,SC_kind,SC_xc_functional,1,&
&                                                    MUTE="+report +log +io_in +io_out",unMUTE=master_cpu)
     !
     if (l_sc_is_libDFT.and..not.(l_sc_exx.or.l_sc_srpa)) V_xc_sc(:,:)=V_xc(:,:)
     if (l_sc_is_libDFT.and.     (l_sc_exx.or.l_sc_srpa)) V_xc_sc(:,:)=V_xc(:,:)+V_xc_sc(:,:)
     !
     ! Mixing 
     !========
     !
     ! WARNING:
     ! The mixing of the potential has been eliminated for HF and COHSEX, for
     ! V_xc_sc case tests are needed.
     ! 
     if (iteration>1.and..not.(l_sc_exx.or.l_sc_srpa)) V_xc_sc = SC_cycle_mixing*V_xc_sc + (1._SP-SC_cycle_mixing)*V_xc_nm1
     !
     V_xc_nm1=V_xc_sc
     !
   endif
   !
 endif
 !
end subroutine SC_add_XC
