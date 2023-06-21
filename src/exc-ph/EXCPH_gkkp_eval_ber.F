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
! --- Theory ---
! (5) Figure out rotations/symmetry of exciton eigenvectors and EPC m.e.
! - FP
!
! GKKP formulation following the PRL  125, 107401 (2020) 
!
subroutine EXCPH_gkkp_eval_ber(iq_s,iq_bz,il,k,q,ID_gkkp)
 !
 use pars,           ONLY:SP,cZERO,rZERO,cONE
 use D_lattice,      ONLY:i_time_rev,sop_inv,sop_tab,nsym,i_time_rev
 use R_lattice,      ONLY:bz_samp,qindx_C
 use BS_solvers,     ONLY:io_BSS_diago,BS_mat,BSS_eh_table,BSS_free
 use IO_m,           ONLY:NONE,OP_APP_CL,OP_RD_CL,DUMP,OP_RD,RD_CL_IF_END,OP_WR_CL, &
&                         deliver_IO_error_message,manage_action,RD
 use IO_int,         ONLY:io_control
 use BS,             ONLY:BS_H_dim,BS_bands
 use ELPH,           ONLY:ph_freqs_sq,elph_branches,FAN_deltaE_treshold,GKKP,Q_io_map,K_io_map
 use EXCPH,          ONLY:BS_mat_in,EXCPH_gkkp,EXCPH_gkkp_sq,&
&                         EXCPH_states,EXCPH_sum,BSS_eh_table_m1,k_plus_q_table,&
&                         l_const_elph,l_abs_elph,l_abs_exc,BSS_eh_table_in
 !
#include<memory.h>
 !
 type(bz_samp) ::k,q
 integer, intent(in)    :: iq_s,iq_bz,il!,iq
 integer, intent(inout) :: ID_gkkp
 !
 ! Work Space
 !
 logical                 :: t_rev
 integer                 :: io_err,i_H,i_Hp,i_alpha,iq_db,ik_db,&
&                           iv,ic,ik_bz,ivp,icp,i_beta,ip_bz,ik_plus_iq_bz,ik_minus_iq_bz,IO_ACT, &
&                           iv_in,ic_in,ik_bz_in
 real(SP)                :: ph_E
 complex(SP)             :: elph_gkkp,Xi_element
 integer, external       :: io_ELPH
 !
 ! Loop scheme:
 !  i_l ->i_beta ->i_star [load gkkp] ->i_H
 !                                    ->ivp [Calculate Xi(1)]
 !                                    ->icp [Calculate Xi(2)]
 !                                    ->i_alpha ->i_H [Calculate Gkkp]
 elph_gkkp=cONE
 !
 t_rev= (iq_s>nsym/(i_time_rev+1))
 !
 !
 ! elph_gkkp(:,ib,ob,ik_bz) = < ib k | dV_ql | ob k - q >
 !
 ! with k and q in the BZ
 !
 iq_db=Q_io_map(iq_bz)
 IO_ACT=RD
 if(il==elph_branches(2)) IO_ACT=manage_action(RD_CL_IF_END,iq_bz,1,q%nbz)
 !
 call io_control(ACTION=IO_ACT,SEC=(/iq_db+1/),ID=ID_gkkp)
 io_err=io_ELPH(ID_gkkp,'gkkp_expanded') ! For now we have to work in the full BZ by turning all symmetries off.
 if(io_err/=0) call error(" Error reading gkkp_expanded") 
 !
 ph_E=sqrt(abs(ph_freqs_sq(iq_db,il)))
 !
 if (ph_E<FAN_deltaE_treshold) return
 !
 EXCPH_gkkp=cZERO
 !
 ! Respect to the Bernardi notation
 !
 ! alpha = m
 ! beta  = n
 !
 do i_beta=EXCPH_sum(1),EXCPH_sum(2)
   !
   do i_H=1,BS_H_dim
     !
     ! K = (ik_bz,iv,ic)
     !
     ik_bz = BSS_eh_table(i_H,1)
     iv    = BSS_eh_table(i_H,2)
     ic    = BSS_eh_table(i_H,3)
     !
     ik_minus_iq_bz = qindx_C(ik_bz,iq_bz,1)      ! K_bz-Q_bz
     !
     ik_plus_iq_bz  = k_plus_q_table(ik_bz,iq_bz) ! K_bz+Q_bz
     !
     ! DS note: I think here the choice is not unique
     !          given a ik_bz there maybe multiple ik_s which send ik_ibz --> ik_bz
     !
     ! ip_bz = R_iq_s^-1 ik_bz
     !       = R_iq_s^-1 R_ik_s ik_ibz
     !
     ip_bz = elemental_scattering(iq_s,ik_minus_iq_bz)
     !
     ! Conduction scattering
     ! ======================
     !
     do icp=BS_bands(1),BS_bands(2)
       !
       i_Hp=BSS_eh_table_m1(ip_bz,iv,icp)
       !
       if (i_Hp==0) cycle
       !
!       ik_db=K_io_map(ik_plus_iq_bz)
       ik_db=K_io_map(ik_minus_iq_bz)
       elph_gkkp=GKKP%dVc(il,icp,ic,ik_db,1)/sqrt(2._SP*ph_E)
       !
       if(.not.t_rev) Xi_element=elph_gkkp*conjg(BS_mat(i_Hp,i_beta))
       if(     t_rev) Xi_element=elph_gkkp*      BS_mat(i_Hp,i_beta)
       !
       do i_alpha=EXCPH_states(1),EXCPH_states(2)
         EXCPH_gkkp(il,i_beta,i_alpha)=EXCPH_gkkp(il,i_beta,i_alpha)+BS_mat_in(i_H,i_alpha)*Xi_element
       enddo
       !
     enddo
     !
   enddo
   !
 enddo !Loop i_beta
 !
 do i_beta=EXCPH_sum(1),EXCPH_sum(2)
   !
   do i_H=1,BS_H_dim
     ! Valence Scattering
     ! =========================
     !
     ! ip_bz = R_iq_s^-1 R_ik_s ik_ibz
     ip_bz = elemental_scattering(iq_s,ik_minus_iq_bz)
     !
     do ivp=BS_bands(1),BS_bands(2)
       !
       i_Hp=BSS_eh_table_m1(ip_bz,ivp,ic)
       !
       if (i_Hp==0) cycle
       !
       ik_db=K_io_map(ik_minus_iq_bz)
       elph_gkkp=    GKKP%dVc(il,iv,ivp,ik_db,1) /sqrt(2._SP*ph_E)
       !
       if(.not.t_rev) Xi_element=elph_gkkp*conjg(BS_mat(i_Hp,i_alpha))
       if(     t_rev) Xi_element=elph_gkkp*      BS_mat(i_Hp,i_alpha)
       !
       do i_alpha=EXCPH_states(1),EXCPH_states(2)
         EXCPH_Gkkp(il,i_beta,i_alpha)=EXCPH_Gkkp(il,i_beta,i_alpha)-BS_mat_in(i_H,i_alpha)*Xi_element
       enddo
       !
     enddo
     !
   enddo
   !
 enddo !Loop i_beta
 !
 ! Compute the squared couplings
 !
 forall(i_alpha=EXCPH_states(1):EXCPH_states(2),i_beta=EXCPH_sum(1):EXCPH_sum(2))
   EXCPH_Gkkp_sq(il,i_beta,i_alpha)=REAL(EXCPH_Gkkp(il,i_beta,i_alpha))**2 + AIMAG(EXCPH_Gkkp(il,i_beta,i_alpha))**2
 end forall
 !
 contains
   !
   integer function elemental_scattering(i_q_SYM,i_k_BZ)
       !
       ! This is done because the BSE is solved for q in IBZ  and k in BZ.
       ! However the EXCPH matrix elements are for k in BZ and q in BZ.
       !
       ! Therefore, I have the exc. weight (q |cRk vRk-q)
       ! but I don't have the exc. weight (Sq |cRk vRk-Sq).
       !
       ! Yet, I know that (Sq |cRk vRk-Sq) = (q |c{S^-1R}k v{S^-1R}k-q).
       !
       ! Therefore, in this subroutine I look for p = {S^-1 R}k
       !
       integer i_q_SYM,i_k_BZ,i_k_IBZ,i_k_SYM
       !
       ! K_i_k_bz =R_i_k_SYM K_i_k_IBZ
       !
       i_k_IBZ = k%sstar(i_k_BZ,1)
       i_k_SYM = k%sstar(i_k_BZ,2)
       !
       ! ip_bz=R_iq_s^-1 R_ik_s ik_ibz
       !
       elemental_scattering = k%k_table(i_k_IBZ, sop_tab(sop_inv(i_q_SYM),i_k_SYM) )
       !
   end function
   !
end subroutine
