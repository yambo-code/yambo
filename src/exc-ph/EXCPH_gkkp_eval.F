!
!        Copyright (C) 2000-2021 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM FP DS
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
! ======================================================================
! GKKP formulation following the F. Paleari PhD thesis (Pg. 109, Eq : 4.25)
! The exciton phonon matrix elements are defined as <q=Q,S'|dV_{Q}|q=0,S>
! These is for phonon absorption
!
subroutine EXCPH_gkkp_eval(iq_s,iq_bz,il,k,q,ID_gkkp)
 !
 use pars,           ONLY:SP,cZERO,rZERO,cONE
 !use D_lattice,      ONLY:sop_inv,sop_tab
 use R_lattice,      ONLY:bz_samp!,qindx_C
 use BS_solvers,     ONLY:io_BSS_diago,BSS_eh_table_m1,BSS_eh_table,BSS_free
 use IO_m,           ONLY:NONE,OP_APP_CL,OP_RD_CL,DUMP,OP_RD,RD_CL_IF_END,OP_WR_CL, &
&                         deliver_IO_error_message,manage_action,RD
 use IO_int,         ONLY:io_control
 use BS,             ONLY:BS_H_dim,BS_bands
 use ELPH,           ONLY:ph_freqs_sq,elph_branches,FAN_deltaE_treshold,GKKP,Q_io_map,K_io_map
 use EXCPH,          ONLY:BS_mat_in,BS_mat_out,EXCPH_gkkp,EXCPH_gkkp_sq,Xi,BSS_eh_table_in,&
&                         EXCPH_states,EXCPH_sum,k_plus_q_table,EXCPH_kthresh,&
&                         l_const_elph,l_abs_elph,l_abs_exc,l_elec_only,l_hole_only
 !
#include<y_memory.h>
 !
 type(bz_samp) ::k,q
 integer, intent(in)    :: iq_s,iq_bz,il!,iq
 integer, intent(inout) :: ID_gkkp
 !
 ! Work Space
 !
 integer                 :: io_err,i_H,i_Hp,i_alpha,iq_db,ik_db,&
&                           iv,ic,ik_bz,ivp,icp,i_beta,ik_plus_iq_bz,IO_ACT, &
&                           iv_in,ic_in,ik_bz_in
 real(SP)                :: ph_E
 complex(SP)             :: elph_gkkp,EXC_out,EXC_in
 integer, external       :: io_ELPH
 !
 ! Loop scheme: 
 !  for each (iq_bz, il) from gkkp_driver:
 !  -> i_beta  ->i_H
 !             ->ivp [Calculate Xi(1), sum over ivp, hole contribution]
 !             ->icp [Calculate Xi(2), sum over icp, elec contribution]
 !             ->i_alpha ->i_H [Calculate Gkkp(iq,il;i_beta,i_alpha), sum over i_H]
 Xi=cZERO
 !
 elph_gkkp=cONE
 !
 !
 ! elph_gkkp(:,ib,ob,ik_bz) = < ib k | dV_ql | ob k - q >
 !
 ! with k and q in the BZ
 !
 ! El-ph rotated or nosym. The wfc gauge and rotation phases must match WF_phases. 
 !
 iq_db=Q_io_map(iq_bz)
 IO_ACT=RD
 if(il==elph_branches(2)) IO_ACT=manage_action(RD_CL_IF_END,iq_bz,1,q%nbz)
 !
 call io_control(ACTION=IO_ACT,SEC=(/iq_db+1/),ID=ID_gkkp)
 io_err=io_ELPH(ID_gkkp,'gkkp_expanded')
 if(io_err/=0) call error(" Error reading gkkp_expanded") 
 !
 ph_E=sqrt(abs(ph_freqs_sq(iq_db,il)))
 !
 if (ph_E<FAN_deltaE_treshold) return
 !
 do i_beta=EXCPH_sum(1),EXCPH_sum(2)
   !
   Xi=cZERO
   !
   !write(iq_bz*1000+i_beta,*) iq_bz,i_beta,iq_s
   !
   do i_H=1,BS_H_dim
     !
     ! K = (ik_bz,iv,ic)
     !
     ik_bz = BSS_eh_table(i_H,1)
     iv    = BSS_eh_table(i_H,2)
     ic    = BSS_eh_table(i_H,3)
     !
     ! only sum k-points up to the shell with radius EXCPH_kthresh (debug only)
     if (EXCPH_kthresh>0._SP) then
       if (norm2(k%ptbz(ik_bz,:)) > EXCPH_kthresh ) cycle
     endif
     !
     !ik_minus_iq_bz = qindx_C(ik_bz,iq_bz,1)      ! K_bz-Q_bz
     !
     ik_plus_iq_bz  = k_plus_q_table(ik_bz,iq_bz) ! K_bz+Q_bz
     !
     ! 1=DOWN (hole contribution)
     !========
     !
     do ivp=BS_bands(1),BS_bands(2)
       !
       if (l_elec_only) exit
       !
       i_Hp=BSS_eh_table_m1(ik_bz,ivp,ic,1,1)
       if (i_Hp==0) cycle
       !
       if (.not.l_const_elph) then
         ik_db=K_io_map(ik_bz)
         if(.not.l_abs_elph) elph_gkkp=    GKKP%dVc(il,iv,ivp,ik_db,1) /sqrt(2._SP*ph_E)
         if(     l_abs_elph) elph_gkkp=abs(GKKP%dVc(il,iv,ivp,ik_db,1))/sqrt(2._SP*ph_E)
       endif
       !
       if (     l_abs_exc) EXC_OUT=  abs(BS_mat_out(i_Hp,i_beta))
       if (.not.l_abs_exc) EXC_OUT=conjg(BS_mat_out(i_Hp,i_beta))
       !
       Xi(i_H,1)=Xi(i_H,1)+elph_gkkp*EXC_OUT
       !
     enddo
     !
     ! 2=UP (electron contribution)
     !======
     !
     do icp=BS_bands(1),BS_bands(2)
       !
       if (l_hole_only) exit
       !
       i_Hp=BSS_eh_table_m1(ik_plus_iq_bz,iv,icp,1,1)
       if (i_Hp==0) cycle
       if (.not.l_const_elph) then
         ik_db=K_io_map(ik_plus_iq_bz)
         if(.not.l_abs_elph) elph_gkkp=    GKKP%dVc(il,icp,ic,ik_db,1) /sqrt(2._SP*ph_E)
         if(     l_abs_elph) elph_gkkp=abs(GKKP%dVc(il,icp,ic,ik_db,1))/sqrt(2._SP*ph_E)
       endif
       !
       if (     l_abs_exc) EXC_OUT=  abs(BS_mat_out(i_Hp,i_beta))
       if (.not.l_abs_exc) EXC_OUT=conjg(BS_mat_out(i_Hp,i_beta)) 
       !
       Xi(i_H,2)=Xi(i_H,2)+elph_gkkp*exc_out 
       !
     enddo
     !
   enddo
   !
   ! Gkkp m.e.
   !=============
   !
   do i_alpha=EXCPH_states(1),EXCPH_states(2)
     !
     do i_H=1,BS_H_dim
       !
       ! K = (ik_bz,iv,ic)
       !
       ik_bz = BSS_eh_table(i_H,1)
       iv    = BSS_eh_table(i_H,2)
       ic    = BSS_eh_table(i_H,3)
       !
       ! K_in = (ik_bz,iv,ic)
       !
       ik_bz_in = BSS_eh_table_in(i_H,1)
       iv_in    = BSS_eh_table_in(i_H,2)
       ic_in    = BSS_eh_table_in(i_H,3)
       !
       if (ik_bz /= ik_bz_in) call error(" Wrong k correspondence")
       if (iv    /= iv_in   ) call error(" Wrong iv correspondence")
       if (ic    /= ic_in   ) call error(" Wrong ic correspondence")
       !
       ! EXC_IN: "Left" part of exciton-phonon coupling. The "right" part is its conjugate
       !
       if (     l_abs_exc) EXC_IN=  abs(BS_mat_in(i_H,i_alpha))
       if (.not.l_abs_exc) EXC_IN=      BS_mat_in(i_H,i_alpha)
       !
       ! FIX ME ! Any reason why we use hole-electron instead of the standard elec-hole?
       !
       EXCPH_Gkkp(il,i_beta,i_alpha)=EXCPH_Gkkp(il,i_beta,i_alpha)+EXC_IN*(Xi(i_H,1)-Xi(i_H,2))
       !
     enddo
   enddo
   !
 enddo !Loop i_beta
 !
 ! Compute the squared couplings (to be removed)
 !
 forall(i_alpha=EXCPH_states(1):EXCPH_states(2),i_beta=EXCPH_sum(1):EXCPH_sum(2))
   EXCPH_Gkkp_sq(il,i_beta,i_alpha)=REAL(EXCPH_Gkkp(il,i_beta,i_alpha))**2 + AIMAG(EXCPH_Gkkp(il,i_beta,i_alpha))**2
 end forall
 !
end subroutine
