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
! GKKP formulation following the F. Paleari PhD thesis
!
subroutine EXCPH_gkkp_eval_pal(iq_s,iq_bz,il,k,q,ID_gkkp)
 !
 use pars,           ONLY:SP,cZERO,rZERO,cONE
 !use electrons,      ONLY:levels
 use D_lattice,      ONLY:i_time_rev,sop_inv,sop_tab,nsym,i_time_rev
 use R_lattice,      ONLY:bz_samp,qindx_C
 use BS_solvers,     ONLY:io_BSS_diago,BS_mat,BSS_eh_table,BSS_free
 use IO_m,           ONLY:NONE,OP_APP_CL,OP_RD_CL,DUMP,OP_RD,RD_CL_IF_END,OP_WR_CL, &
&                         deliver_IO_error_message,manage_action,RD
 use IO_int,         ONLY:io_control
 !use X_m,            ONLY:X_t
 use BS,             ONLY:BS_H_dim,BS_bands
 use ELPH,           ONLY:ph_freqs_sq,elph_branches,FAN_deltaE_treshold,GKKP,Q_io_map,K_io_map
 use EXCPH,          ONLY:BS_mat_in,EXCPH_gkkp,EXCPH_gkkp_sq,Xi,BSS_eh_table_in,&
&                         EXCPH_states,EXCPH_sum,BSS_eh_table_m1,k_plus_q_table,&
&                         l_const_elph,l_abs_elph,l_abs_exc,l_elec_only,l_hole_only
 !
#include<memory.h>
 !
 !type(levels)  ::E
 type(bz_samp) ::k,q
 !type(X_t)     ::X
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
 complex(SP)             :: elph_gkkp
 integer, external       :: io_ELPH
 !complex(SP)                :: aux_phase_1,aux_phase_2
 !
 ! Loop scheme:
 !  i_l ->i_beta ->i_star [load gkkp] ->i_H
 !                                    ->ivp [Calculate Xi(1)]
 !                                    ->icp [Calculate Xi(2)]
 !                                    ->i_alpha ->i_H [Calculate Gkkp]
 Xi=cZERO
 !
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
       ip_bz = elemental_scattering(iq_s,ik_bz)
       !
       ! 1=DOWN (hole contribution)
       !========
       !
       do ivp=BS_bands(1),BS_bands(2)
         !
         if (l_elec_only) exit
         !
         i_Hp=BSS_eh_table_m1(ip_bz,ivp,ic)
         if (i_Hp==0) cycle
         !
         ! [NB] Here we apply the conj and SWAP band indices in the matrix elements
         !      Notes: (iv,ivp) => Code: (ivp,iv)
         !      See issues #690 and #609
         if (.not.l_const_elph) then
           ik_db=K_io_map(ik_bz)
           if(.not.l_abs_elph) elph_gkkp=conjg(GKKP%dVc(il,ivp,iv,ik_db,1)) /sqrt(2._SP*ph_E)
           if(     l_abs_elph) elph_gkkp=  abs(GKKP%dVc(il,ivp,iv,ik_db,1))/sqrt(2._SP*ph_E)
           !ik_db=K_io_map(ik_minus_iq_bz)
           !elph_gkkp=GKKP%dVc(il,ivp,iv,ik_db,1)/sqrt(2._SP*ph_E)
         endif
         !
         !ik_db=K_io_map(ik_bz)
         !write(*,*) elph_gkkp,conjg(GKKP%dVc(il,ivp,iv,ik_db,1))/sqrt(2._SP*ph_E)
         !
         !write(iq_bz*1000+i_beta,*) i_Hp,ik_bz,ivp,ic,abs(BS_mat(i_Hp,i_beta)),ip_bz,ik_db,k%ptbz(ik_bz,:)
         !
         if (l_abs_exc) then
           Xi(i_H,1)=Xi(i_H,1)+elph_gkkp*abs(BS_mat(i_Hp,i_beta))
         else
           if(.not.t_rev) Xi(i_H,1)=Xi(i_H,1)+elph_gkkp*conjg(BS_mat(i_Hp,i_beta))
           if(     t_rev) Xi(i_H,1)=Xi(i_H,1)+elph_gkkp*      BS_mat(i_Hp,i_beta)
         endif
         !
       enddo
       !
       ! ip_bz = R_iq_s^-1 R_ik_s ik_ibz
       !
       ip_bz = elemental_scattering(iq_s,ik_plus_iq_bz)
       !
       ! 2=UP (electron contribution)
       !======
       !
       do icp=BS_bands(1),BS_bands(2)
         !
         if (l_hole_only) exit
         !
         i_Hp=BSS_eh_table_m1(ip_bz,iv,icp)
         if (i_Hp==0) cycle
         ! [NB] Here we apply the conj and SWAP band indices in the matrix elements
         !      Notes: (icp,ic) => Code: (ic,icp)
         !      See issues #690 and #609
         if (.not.l_const_elph) then
           ik_db=K_io_map(ik_plus_iq_bz)
           if(.not.l_abs_elph) elph_gkkp=conjg(GKKP%dVc(il,ic,icp,ik_db,1))/sqrt(2._SP*ph_E)
           if(     l_abs_elph) elph_gkkp=abs(GKKP%dVc(il,ic,icp,ik_db,1))/sqrt(2._SP*ph_E)
           !ik_db=K_io_map(ik_bz)
           !elph_gkkp=GKKP%dVc(il,ic,icp,ik_db,1)/sqrt(2._SP*ph_E)
         endif
         if (l_abs_exc) then
           Xi(i_H,2)=Xi(i_H,2)+elph_gkkp*abs(BS_mat(i_Hp,i_beta))
         else
           if(.not.t_rev) Xi(i_H,2)=Xi(i_H,2)+elph_gkkp*conjg(BS_mat(i_Hp,i_beta))
           if(     t_rev) Xi(i_H,2)=Xi(i_H,2)+elph_gkkp*      BS_mat(i_Hp,i_beta)
         endif
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
         ! "Left" part of exciton-phonon coupling. The "right" part is its conjugate
         !
         !aux_phase_1 = EXP( (0.0,1.0)*ATAN2( AIMAG(Xi(i_H,1)), REAL(Xi(i_H,1)) ) )
         !aux_phase_2 = EXP( (0.0,1.0)*ATAN2( AIMAG(Xi(i_H,2)), REAL(Xi(i_H,2)) ) )
         !
         if (l_abs_exc) then
           EXCPH_Gkkp(il,i_beta,i_alpha)=EXCPH_Gkkp(il,i_beta,i_alpha)+&
&                                        abs(BS_mat_in(i_H,i_alpha))*(Xi(i_H,1)-Xi(i_H,2))!*conjg(aux_phase_2)*aux_phase_1)
         else
           EXCPH_Gkkp(il,i_beta,i_alpha)=EXCPH_Gkkp(il,i_beta,i_alpha)+&
&                                        BS_mat_in(i_H,i_alpha)*(Xi(i_H,1)-Xi(i_H,2))!*conjg(aux_phase_2)*aux_phase_1)
         endif
         !
       enddo
     enddo
     !
   enddo !Loop i_beta
 !
 ! Compute the squared couplings
 !
 forall(i_alpha=EXCPH_states(1):EXCPH_states(2),i_beta=EXCPH_sum(1):EXCPH_sum(2))
   !EXCPH_Gkkp_sq(il,i_beta,i_alpha)=EXCPH_Gkkp(il,i_beta,i_alpha)*conjg(EXCPH_Gkkp(il,i_beta,i_alpha))
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
