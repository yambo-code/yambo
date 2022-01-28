!
!        Copyright (C) 2000-2021 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM FP
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
subroutine EXCPH_Gkkp_eval(iq,E,k,q,X)
 !
 use pars,           ONLY:SP,cZERO
 use drivers,        ONLY:l_EXCPH_Sigma
 use electrons,      ONLY:levels,n_sp_pol
 use D_lattice,      ONLY:i_time_rev,sop_inv,sop_tab
 use R_lattice,      ONLY:nqibz,bz_samp,qindx_B_load
 use BS_solvers,     ONLY:io_BSS_diago,BSS_n_eig,BS_mat,BSS_eh_table,BSS_free,BSS_write_eig_2_db
 use IO_m,           ONLY:NONE,OP_APP_CL,OP_RD_CL,DUMP,OP_RD,RD_CL_IF_END,OP_WR_CL,deliver_IO_error_message
 use IO_int,         ONLY:io_control
 use X_m,            ONLY:X_t
 use BS,             ONLY:BS_H_dim,BS_bands,L_kind,BS_res_ares_n_mat
 use ELPH,           ONLY:ph_freqs_sq,elph_branches,FAN_deltaE_treshold,GKKP
 use EXCPH,          ONLY:BS_mat_gamma,BSS_eh_table_gamma,EXCPH_Gkkp,EXCPH_Gkkp_sq,&
&                         EXCPH_states,EXCPH_sum,EXCPH_kind
 use LIVE_t,         ONLY:live_timing
#include<memory.h>
 !
 type(levels)  ::E
 type(bz_samp) ::k,q
 type(X_t)     ::X
 !
 ! Work Space
 !
 integer                 :: iq,ID_BS,ID_exc_Gkkp,io_err,i_star,iq_bz,il,i_H,i_Hp,i_alpha,&
&                           iv,ic,ik_bz,ivp,icp,iq_s,i_beta,ip_bz,ik_plus_iq_bz,STATES(2),SUM(2)
 integer, save           :: ID_gkkp
 real(SP)                :: ph_E,elph_gkkp_sq
 complex(SP),allocatable :: BS_E(:),A_rot(:,:),Xi(:,:,:)
 integer,    allocatable :: BSS_eh_table_m1(:,:,:),k_plus_q_table(:,:)
 integer,       external :: io_ELPH,io_EXCPH_gkkp
 integer, external ::qindx_B_init,qindx_B_close 
 integer  :: qindx_tmp(2),qindx_ID,qindx_ID_frag
 !
 if (i_time_rev/=0) then
   call warning("Excitonic Gkkp: Spatial inversion not detected and Time-Reversal symmetry not implemented")
   !return
 endif
 if (BS_res_ares_n_mat/=1) then
   call warning("Excitonic Gkkp: anti-resonant part not implemented")
   return
 endif
 !
 call section('-','Excitonic Gkkp m.e.')
 !======================================
 !
 ! LIMITATIONS:
 !  [1] The matrix elements can only be computed with all symmetries turned off
 !      until the proper rules for expansion of phonon and exciton eigenstates are
 !      figured out and implemented.
 !
 ! Headers ...
 !
 call io_control(ACTION=OP_RD_CL,COM=NONE,MODE=DUMP,SEC=(/1/),ID=ID_BS)
 io_err=io_BSS_diago(iq,1,ID_BS,X,bsE=BS_E)
 !
 if (.not.BSS_write_eig_2_db) call error("BSS diago DB does not contain the eigenstates")
 !
 ! pre-allocs...
 !
 YAMBO_ALLOC(BS_mat,(BS_H_dim,BSS_n_eig))
 YAMBO_ALLOC(A_rot,(BS_H_dim,BSS_n_eig))
 YAMBO_ALLOC(BSS_eh_table,(BS_H_dim,3+n_sp_pol-1))
 YAMBO_ALLOC(BSS_eh_table_m1,(k%nbz,BS_bands(1):BS_bands(2),BS_bands(1):BS_bands(2)))
 YAMBO_ALLOC(k_plus_q_table,(k%nbz,q%nbz))
 !
 STATES=EXCPH_states
 SUM   =EXCPH_sum
 !
 if ((STATES(2).GT.BS_H_dim).or.(SUM(2).GT.BS_H_dim)) then
   call error("Number of exc. states in the excphon coupling must <= than the size of the exc. Hamiltonian")
 endif
 !
 YAMBO_ALLOC(EXCPH_Gkkp,(elph_branches(1):elph_branches(2),SUM(1):SUM(2),STATES(1):STATES(2)))
 YAMBO_ALLOC(EXCPH_Gkkp_sq,(elph_branches(1):elph_branches(2),SUM(1):SUM(2),STATES(1):STATES(2)))
 YAMBO_ALLOC(Xi,(BS_H_dim,SUM(1):SUM(2),2))
 !
 Xi=cZERO
 EXCPH_Gkkp=cZERO
 EXCPH_Gkkp_sq=0
 k_plus_q_table  = 0
 BSS_eh_table    = 0
 BSS_eh_table_m1 = 0
 !
 if (iq==1) then
   YAMBO_ALLOC(BS_mat_gamma,(BS_H_dim,STATES(1):STATES(2)))
   YAMBO_ALLOC(BSS_eh_table_gamma,(STATES(1):STATES(2),3+n_sp_pol-1))
 endif
 !
 ! Eigenstates/values ...
 !
 call read_according_to_scattering_type()
 !
 ! k_plus_q_table
 !
 ! For each K_bz,Q_bz k_plus_q_table(ik_bz,iq_bz)=ip_bz with
 !
 !  K_bz+Q_bz=P_bz
 !
 qindx_tmp=qindx_B_init(qindx_ID,qindx_ID_frag)
 !do ik_bz=1,10 
 !   qindx_tmp=qindx_B_load(ik_bz,ik_bz+500,qindx_ID)
 !   write(*,*) ik_bz, qindx_tmp
 !enddo 
 !stop
 write(*,*) 'start of k+q loop'
 !
 !write(*,*) 'qindx_ID', qindx_ID  
 !qindx_tmp=qindx_B_init(qindx_ID)
 !
 write(*,*) 'qindx_B_init done'
 do ik_bz=1,k%nbz
   IQ_bz_loop: do iq_bz=1,q%nbz
     do ip_bz=1,k%nbz
       qindx_tmp=qindx_B_load(ip_bz,ik_bz,qindx_ID_frag)
       if (qindx_tmp(1)==iq_bz) then
         k_plus_q_table(ik_bz,iq_bz)=ip_bz
         cycle IQ_bz_loop
       endif
     enddo
   enddo IQ_bz_loop
 enddo
 !write(*,*) 'OMP loop is done'
 !write(*,*) qindx_ID
 qindx_tmp=qindx_B_close(qindx_ID,qindx_ID_frag)
 !
 ! BSS_eh_table_m1
 !
 do i_H=1,BS_H_dim
   !
   ik_bz = BSS_eh_table(i_H,1)
   iv    = BSS_eh_table(i_H,2)
   ic    = BSS_eh_table(i_H,3)
   !
   BSS_eh_table_m1(ik_bz,iv,ic)=i_H
   !
 enddo
 !
 call live_timing('Excitonic Gkkp',q%nstar(iq)*(elph_branches(2)-elph_branches(1)+1))
 !
 ! Headers ...
 !
 if (iq==1) then
   call io_control(ACTION=OP_RD,COM=NONE,SEC=(/1/),ID=ID_gkkp)
   io_err=io_ELPH(ID_gkkp,'gkkp_expanded') ! For now we have to work in the full BZ by turning all symmetries off.
   !io_err=io_ELPH(ID_gkkp,'gkkp')
   call deliver_IO_error_message(io_err,"GKKP")
   if (io_err/=0) call error('El-Ph database not found')
   !
   ! EXCPHON database is written here only if the self-energy will not be computed
   if (.not.l_EXCPH_Sigma) then
     call io_control(ACTION=OP_WR_CL,SEC=(/1/),ID=ID_exc_Gkkp)
     io_err=io_EXCPH_gkkp(ID_exc_Gkkp,'excitonic_Gkkp')
   endif
   !
 endif
 !
 !
 ! Loop scheme:
 !  i_l ->i_beta ->i_star [load gkkp] ->i_H
 !                                    ->ivp [Calculate Xi(1)]
 !                                    ->icp [Calculate Xi(2)]
 !                                    ->i_alpha ->i_H [Calculate Gkkp]
 !
 do il=elph_branches(1),elph_branches(2)
   !
   do i_beta=SUM(1),SUM(2)
     !
     do i_star=1,q%nstar(iq)
       !
       iq_s=q%star(iq,i_star)
       !
       iq_bz = q%k_table(iq, iq_s)
       if (iq_bz/=1) then
         call warning("Exit because you are not Gamma")
         return
       endif
       !
       ! elph_gkkp(:,ib,ob,ik_bz) = < ib k | dV_ql | ob k - q >
       !
       ! with k and q in the BZ
       !
       call io_control(ACTION=RD_CL_IF_END,SEC=(/iq_bz+1/),ID=ID_gkkp)
       io_err=io_ELPH(ID_gkkp,'gkkp_expanded') ! For now we have to work in the full BZ by turning all symmetries off.
       !io_err=io_ELPH(ID_gkkp,'gkkp')
       !
       ph_E=sqrt(abs(ph_freqs_sq(iq_bz,il)))
       !
       if (abs(ph_E)<FAN_deltaE_treshold) cycle
       !
       do i_H=1,BS_H_dim
         !
         ! K = (ik_bz,iv,ic)
         !
         ik_bz = BSS_eh_table(i_H,1)
         iv    = BSS_eh_table(i_H,2)
         ic    = BSS_eh_table(i_H,3)
         !
         ! ip_bz=R_iq_s^-1 R_ik_s ik_ibz
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
           elph_gkkp_sq=GKKP%dVc(il,iv,ivp,ik_bz,1)/sqrt(2.*ph_E)
           !
           if (i_Hp/=0) then
             Xi(i_H,i_beta,1)=Xi(i_H,i_beta,1)+elph_gkkp_sq*conjg(BS_mat(i_Hp,i_beta))
             !write (*,*) "iq=",iq,"i_star=",i_star,"ivp=",ivp,"i_H=",i_H,"i_Hp=",i_Hp
           endif
           !
         enddo
         !
         ! ip_bz=R_iq_s^-1 R_ik_s ik_ibz
         !
         ik_plus_iq_bz = k_plus_q_table(ik_bz,iq_bz) ! K_bz+Q_bz
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
           elph_gkkp_sq=GKKP%dVc(il,icp,ic,ip_bz,1)/sqrt(2.*ph_E)
           !
           if (i_Hp/=0) then
             Xi(i_H,i_beta,2)=Xi(i_H,i_beta,2)+elph_gkkp_sq*conjg(BS_mat(i_Hp,i_beta))
           endif
           !
         enddo
         !
       enddo
       !
       ! Gkkp m.e.
       !=============
       !
       do i_alpha=STATES(1),STATES(2)
         !
         do i_H=1,BS_H_dim
           !
           ! "Left" part of exciton-phonon coupling. The "right" part is its conjugate
           !
           EXCPH_Gkkp(il,i_beta,i_alpha)=EXCPH_Gkkp(il,i_beta,i_alpha)+&
&                                        BS_mat_gamma(i_H,i_alpha)*(Xi(i_H,i_beta,1)-Xi(i_H,i_beta,2))
           !
         enddo
       enddo
       !
       Xi=cZERO
       !
       call live_timing(steps=1)
       !
     enddo !Loop i_star
     !
   enddo !Loop i_beta
   !
 enddo !Loop il
 !
 call compute_squared_couplings()
 !
 if (.not.l_EXCPH_Sigma) then
   ! I/O
   !=====
   call io_control(ACTION=OP_APP_CL,SEC=(/iq+1/),ID=ID_exc_Gkkp)
   io_err=io_EXCPH_gkkp(ID_exc_Gkkp,'excitonic_Gkkp')
   !call msg('s','I/O done')
   !
 endif
 !
 ! Some checks and reminders.
 !write (*,*) sqrt(abs(ph_freqs_sq(iq,:)))*HA2EV
 !
 call live_timing( )
 !
 ! CLEANs...
 !===========
 !
 if (iq==nqibz) then
   YAMBO_FREE(BS_mat_gamma)
   YAMBO_FREE(BSS_eh_table_gamma)
 endif
 YAMBO_FREE(BS_mat)
 YAMBO_FREE(A_rot)
 YAMBO_FREE(Xi)
 if (.not.l_EXCPH_Sigma) then
   YAMBO_FREE(EXCPH_Gkkp)
   YAMBO_FREE(EXCPH_Gkkp_sq)
 endif
 YAMBO_FREE(k_plus_q_table)
 YAMBO_FREE(BSS_eh_table)
 YAMBO_FREE(BSS_eh_table_m1)
 call BSS_free( )
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
   subroutine read_according_to_scattering_type()
       !
       if (EXCPH_kind=='TT'.or.EXCPH_kind=='LL') then
         !
         call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/2,3/),ID=ID_BS)
         io_err=io_BSS_diago(iq,1,ID_BS,X,bsE=BS_E)
         !
         if (iq==1) then
           !
           call k_build_up_BZ_tables(k)
           call k_build_up_BZ_tables(q)
           !
           BS_mat_gamma=BS_mat(:,STATES(1):STATES(2))
           BSS_eh_table_gamma=BSS_eh_table(STATES(1):STATES(2),:)
           !
         endif
       endif
       !
       if (EXCPH_kind=='LT') then
         !
         if (iq==1) then
           !
           L_kind = "Lbar"
           call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/2,3/),ID=ID_BS)
           io_err=io_BSS_diago(iq,1,ID_BS,X,bsE=BS_E)
           !
           call k_build_up_BZ_tables(k)
           call k_build_up_BZ_tables(q)
           !
           BS_mat_gamma=BS_mat(:,STATES(1):STATES(2))
           BSS_eh_table_gamma=BSS_eh_table(STATES(1):STATES(2),:)
           !
           L_kind = "Lfull"
           !
         endif
         !
         call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/2,3/),ID=ID_BS)
         io_err=io_BSS_diago(iq,1,ID_BS,X,bsE=BS_E)
         !
       endif
       !
       if (EXCPH_kind=='TL') then
        !
        if (iq==1) then
          !
          L_kind = "Lfull"
          call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/2,3/),ID=ID_BS)
          io_err=io_BSS_diago(iq,1,ID_BS,X,bsE=BS_E)
          !
          call k_build_up_BZ_tables(k)
          call k_build_up_BZ_tables(q)
          !
          BS_mat_gamma=BS_mat(:,STATES(1):STATES(2))
          BSS_eh_table_gamma=BSS_eh_table(STATES(1):STATES(2),:)
          !
          L_kind = "Lbar"
          !
        endif
        !
        call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/2,3/),ID=ID_BS)
        io_err=io_BSS_diago(iq,1,ID_BS,X,bsE=BS_E)
        !
       endif
       !
       if (EXCPH_kind=='IL') then
         !
         if (iq==1) then
           !
           L_kind = "Lfull"
           call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/2,3/),ID=ID_BS)
           io_err=io_BSS_diago(iq,1,ID_BS,X,bsE=BS_E)
           !
           call k_build_up_BZ_tables(k)
           call k_build_up_BZ_tables(q)
           !
           BS_mat_gamma=BS_mat(:,STATES(1):STATES(2))
           BSS_eh_table_gamma=BSS_eh_table(STATES(1):STATES(2),:)
           !
           L_kind = "Ltilde"
           !
         endif
         !
         call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/2,3/),ID=ID_BS)
         io_err=io_BSS_diago(iq,1,ID_BS,X,bsE=BS_E)
         !
       endif
       !
       if (EXCPH_kind=='IT') then
         !
         if (iq==1) then
           !
           L_kind = "Lbar"
           call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/2,3/),ID=ID_BS)
           io_err=io_BSS_diago(iq,1,ID_BS,X,bsE=BS_E)
           !
           call k_build_up_BZ_tables(k)
           call k_build_up_BZ_tables(q)
           !
           BS_mat_gamma=BS_mat(:,STATES(1):STATES(2))
           BSS_eh_table_gamma=BSS_eh_table(STATES(1):STATES(2),:)
           !
           L_kind = "Ltilde"
           !
         endif
         !
         call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/2,3/),ID=ID_BS)
         io_err=io_BSS_diago(iq,1,ID_BS,X,bsE=BS_E)
         !
       endif
       !
   end subroutine
   !
   subroutine compute_squared_couplings()
       !
       integer :: i_alpha,i_beta,il
       !
       do i_alpha=STATES(1),STATES(2)
         !
         do i_beta=SUM(1),SUM(2)
           !
           do il=elph_branches(1),elph_branches(2)
             !
             EXCPH_Gkkp_sq(il,i_beta,i_alpha)=EXCPH_Gkkp_sq(il,i_beta,i_alpha)+&
&                                             EXCPH_Gkkp(il,i_beta,i_alpha)*conjg(EXCPH_Gkkp(il,i_beta,i_alpha))
             !
           enddo
           !
         enddo
         !
       enddo
       !
   end subroutine
   !
end subroutine
