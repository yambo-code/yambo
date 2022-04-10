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
subroutine EXCPH_gkkp_eval(iq,iq_s,iq_bz,il,E,k,q,X,ID_gkkp)
 !
 use pars,           ONLY:SP,cZERO,rZERO
 use drivers,        ONLY:l_EXCPH_Sigma
 use electrons,      ONLY:levels,n_sp_pol
 use D_lattice,      ONLY:i_time_rev,sop_inv,sop_tab,nsym,i_time_rev
 use R_lattice,      ONLY:nqibz,bz_samp,qindx_C
 use BS_solvers,     ONLY:io_BSS_diago,BSS_n_eig,BS_mat,BSS_eh_table,BSS_free,BSS_write_eig_2_db
 use IO_m,           ONLY:NONE,OP_APP_CL,OP_RD_CL,DUMP,OP_RD,RD_CL_IF_END,OP_WR_CL, &
&                         deliver_IO_error_message,manage_action,RD
 use IO_int,         ONLY:io_control
 use X_m,            ONLY:X_t
 use BS,             ONLY:BS_H_dim,BS_bands,L_kind,BS_res_ares_n_mat
 use ELPH,           ONLY:ph_freqs_sq,elph_branches,FAN_deltaE_treshold,GKKP
 use EXCPH,          ONLY:BS_mat_in,BSS_eh_table_in,EXCPH_gkkp,EXCPH_gkkp_sq,Xi,DB_Q_map,&
&                         EXCPH_states,EXCPH_sum,EXCPH_kind,BSS_eh_table_m1!,k_plus_q_table
#include<memory.h>
 !
 type(levels)  ::E
 type(bz_samp) ::k,q
 type(X_t)     ::X
 integer, intent(in)    :: iq,iq_s,iq_bz,il
 integer, intent(inout) :: ID_gkkp
 !
 ! Work Space
 !
 logical                 :: t_rev
 integer                 :: ID_BS,ID_exc_Gkkp,io_err,i_H,i_Hp,i_alpha,iq_db,&
&                           iv,ic,ik_bz,ivp,icp,i_beta,ip_bz,ik_plus_iq_bz,IO_ACT
 real(SP)                :: ph_E,elph_gkkp_sq
 integer, external       :: io_ELPH
 !
 ! Loop scheme:
 !  i_l ->i_beta ->i_star [load gkkp] ->i_H
 !                                    ->ivp [Calculate Xi(1)]
 !                                    ->icp [Calculate Xi(2)]
 !                                    ->i_alpha ->i_H [Calculate Gkkp]
 Xi=cZERO
 !
   !
   t_rev= (iq_s>nsym/(i_time_rev+1))
   !
   ! elph_gkkp(:,ib,ob,ik_bz) = < ib k | dV_ql | ob k - q >
   !
   ! with k and q in the BZ
   !
   iq_db=DB_Q_map(iq_bz)
   IO_ACT=manage_action(RD,iq_db,1,q%nbz)
   if(il==elph_branches(2)) IO_ACT=manage_action(RD_CL_IF_END,iq_db,1,q%nbz)
   !
   call io_control(ACTION=IO_ACT,SEC=(/iq_db+1/),ID=ID_gkkp)
   io_err=io_ELPH(ID_gkkp,'gkkp_expanded') ! For now we have to work in the full BZ by turning all symmetries off.
   if(io_err/=0) call error(" Error reading gkkp_expanded") 
   !
   ph_E=sqrt(abs(ph_freqs_sq(iq_db,il)))
   !
   if (ph_E<FAN_deltaE_treshold) return
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
       ! ip_bz = R_iq_s^-1 R_ik_s ik_ibz
       !       = R_iq_s^-1 ik_bz
       !
       ip_bz = elemental_scattering(iq_s,ik_bz)
       !
       ! 1=DOWN
       !========
       !
       do ivp=BS_bands(1),BS_bands(2)
         !
         i_Hp=BSS_eh_table_m1(ip_bz,ivp,ic)
         !
         elph_gkkp_sq=GKKP%dVc(il,iv,ivp,ik_bz,1)/sqrt(2._SP*ph_E)
         !
         if (i_Hp==0) cycle
         !
         if(.not.t_rev) Xi(i_H,i_beta,1)=Xi(i_H,i_beta,1)+elph_gkkp_sq*conjg(BS_mat(i_Hp,i_beta))
         if(     t_rev) Xi(i_H,i_beta,1)=Xi(i_H,i_beta,1)+elph_gkkp_sq*      BS_mat(i_Hp,i_beta) 
         !
       enddo
       !
       ! ip_bz = R_iq_s^-1 R_ik_s ik_ibz
       !
       ik_plus_iq_bz = qindx_C(ik_bz,iq_bz,1) !  k_plus_q_table(ik_bz,iq_bz) ! K_bz+Q_bz
       !
       ip_bz = elemental_scattering(iq_s,ik_plus_iq_bz)
       !
       ! 2=UP
       !======
       !
       do icp=BS_bands(1),BS_bands(2)
         !
         i_Hp=BSS_eh_table_m1(ip_bz,iv,icp)
         !
         elph_gkkp_sq=GKKP%dVc(il,icp,ic,ip_bz,1)/sqrt(2._SP*ph_E)
         !
         if (i_Hp==0) cycle
         !
         if(.not.t_rev) Xi(i_H,i_beta,2)=Xi(i_H,i_beta,2)+elph_gkkp_sq*conjg(BS_mat(i_Hp,i_beta))
         if(     t_rev) Xi(i_H,i_beta,2)=Xi(i_H,i_beta,2)+elph_gkkp_sq*      BS_mat(i_Hp,i_beta) 
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
         ! "Left" part of exciton-phonon coupling. The "right" part is its conjugate
         !
         EXCPH_Gkkp(il,i_beta,i_alpha)=EXCPH_Gkkp(il,i_beta,i_alpha)+&
&                                      BS_mat_in(i_H,i_alpha)*(Xi(i_H,i_beta,1)-Xi(i_H,i_beta,2))
         !
       enddo
     enddo
     !
     Xi=cZERO
     !
   enddo !Loop i_beta
 !
 ! Compute the squared couplings
 !
 forall(i_alpha=EXCPH_states(1):EXCPH_states(2),i_beta=EXCPH_sum(1):EXCPH_sum(2))
   EXCPH_Gkkp_sq(il,i_beta,i_alpha)=EXCPH_Gkkp(il,i_beta,i_alpha)*conjg(EXCPH_Gkkp(il,i_beta,i_alpha))
 end forall
 !
 contains
   !
   integer function elemental_scattering(i_q_SYM,i_k_BZ)
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
