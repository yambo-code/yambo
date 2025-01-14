!
!        Copyright (C) 2000-2021 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM FP AC DS
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
!> @brief Calculate exciton-phonon matrix elements and satellites renormalizations
!!
!! @param[in]       E                quasi-particle index
!! @param[in]       k                q-index in the BZ
!! @param[in]       q                q-index in distributed array
!! @param[in]       X                phonon-branch index
!!
!! @param[out]      EXCPH_gkkp                  Exciton-phonon matrix elements
!! @param[out]      EXCPH_gkkp_sq               Exciton-phonon matrix elements squared
!
subroutine EXCPH_gkkp_driver(E,k,q,X)
 !
 use pars,           ONLY:SP,cZERO,rZERO,schlen
 use electrons,      ONLY:levels
 use R_lattice,      ONLY:bz_samp,qindx_B
 use D_lattice,      ONLY:nsym
 use parser_m,       ONLY:parser
 use X_m,            ONLY:X_t
 use BS_solvers,     ONLY:BS_mat,BSS_eh_table,BSS_eh_table_m1,BSS_alloc,BSS_free!,BS_H_dim,BSS_n_eig
 !use BS,             ONLY:BSqpts,BS_H_dim,BS_bands
 use ELPH,           ONLY:elph_branches,l_GKKP_expanded_DB_exists!,Q_io_map
 use IO_m,           ONLY:NONE,OP_APP_CL,OP_RD_CL,DUMP,OP_RD,RD_CL_IF_END,OP_WR_CL,deliver_IO_error_message, &
&                         REP,VERIFY,WR_CL,OP_WR,WR_CL_IF_END,manage_action
 use IO_int,         ONLY:io_control
 use EXCPH,          ONLY:BS_mat_in,BS_mat_out,BSS_eh_table_in,EXCPH_gkkp,EXCPH_gkkp_sq,BS_E_in,BS_E,BSS_eh_table_m1_in,&
&                         EXCPH_states,EXCPH_sum,k_plus_q_table,build_Q_map,EXCPH_q, &
&                         EXCPH_gkkp_alloc,EXCPH_gkkp_free,l_const_elph,l_abs_elph,l_abs_exc,l_elec_only,l_hole_only, &
&                         n_exc_in,BSE_alloc,BSE_free,BSE_in_alloc,BSE_out_alloc,BSE_in_free
 use LIVE_t,         ONLY:live_timing
 use timing_m,       ONLY:timing
 use com,            ONLY:msg
 !
#include<y_memory.h>
 !
 type(levels)  ::E
 type(bz_samp) ::k,q
 type(X_t)     ::X
 !
 ! Work Space
 !
 integer, external :: io_ELPH,io_EXCPH_gkkp,io_RIM
 integer  :: S_index(EXCPH_states(1):EXCPH_States(2)),ID_phases
 integer  :: ID_gkkp,ID_exc_gkkp,io_err,io_ID,IO_ACT
 integer  :: il,iq_bz,iq,i_star,iq_s,elph_branches_save(2)!,iq_db
 logical  :: l_min_mem
 ! Debug
 !integer :: ik
 !open(1, file='K_grid_bz.dat')
 !
 call section('*','Excitonic gkkp')
 !======================================
 !
 ! Define n_exc_in
 !
 n_exc_in=EXCPH_states(2)-EXCPH_states(1)+1
 ! Sorting index is trivial, no sorting done in exc-ph.
 do il=EXCPH_states(1),EXCPH_States(2)
   S_index(il)=il
 enddo
 !
 ! Check if Lout and Lin are present
 ! and read their kind
 ! 
 ! Load RIM if present
 !
 call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1/),MODE=DUMP,ID=io_ID)
 io_err=io_RIM(io_ID)
 !
 if(io_err==0) call msg('sr','RIM found and loaded')
 !
 call EXCPH_load_L(1,X,'check','Lout')
 call EXCPH_load_L(1,X,'check','Lin')
 !
 call parser('ConstElph',l_const_elph)
 call parser('AbsElph',  l_abs_elph)
 call parser('AbsExc',   l_abs_exc)
 call parser('ElectronContributionOnly', l_elec_only)
 call parser('HoleContributionOnly', l_hole_only)
 if (l_const_elph) call warning('Constant electron-phonon coupling')
 if(l_abs_elph) call warning('Absolute value of electron-phonon coupling ')
 if(l_abs_exc)  call warning('Absolute value of exciton wave functions  ')
 if(l_elec_only) call warning('Only electron contribution to the exciton-phonon coupling')
 if(l_hole_only) call warning('Only hole contribution to the exciton-phonon coupling')
 !
 elph_branches_save  =elph_branches
 call io_control(ACTION=OP_RD_CL,COM=REP,MODE=DUMP,SEC=(/1/),ID=ID_gkkp)
 ! Here we read the fully k-expanded and q-expanded matrix elements from the SAVE. 
 io_err=io_ELPH(ID_gkkp,'gkkp_expanded')
 elph_branches =elph_branches_save
 !
 ! We build the q-phonon / q-yambo map only in the case without symmetries,
 ! where we expect the indices to differ.
 if(io_err==0) then
   l_GKKP_expanded_DB_exists=.true.
   call ELPH_databases_grids_map(k,q)
   call k_ibz2bz(q,"i",.false.)
   call build_Q_map(q)
   call k_ibz2bz(q,"d",.false.)
 endif
 !
 ! Check if exciton-phonon matrix elements have been already calculated
 ! and are consistent with the Lin and Lout specified in input
 !
 call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1/),MODE=VERIFY,ID=ID_exc_gkkp)
 io_err=io_EXCPH_gkkp(ID_exc_gkkp)
 !
 if(io_err==0) return
 if(io_err/=0) call warning("[EXCPH] Exc-ph matrix elements not correct or missing. To be computed")
 !
 call timing('EXCPH_gkkp',OPR='start')
 !
 call k_build_up_BZ_tables(k)
 call k_build_up_BZ_tables(q)
 !
 l_min_mem=.false.
 !
 ! Pre-requisite: calculate electronic phases (needed to rotate the excitonic wavefunctions)
 if(nsym>1) call ELEC_WF_phases(E,k,l_min_mem,ID_phases)
 !
 call EXCPH_load_L(1,X,'check','Lin') ! At present the Lin can be only gamma iq=1
 !
 call BSS_alloc()
 !
 call BSE_alloc(k,q)
 !
 call EXCPH_load_L(1,X,'load','Lin')
 !
 ! Alloc BS_in variables
 ! 
 call BSE_in_alloc(k)  ! computed at the BSE step
 call BSE_out_alloc()  ! rotated via unitary transformations
 !
 ! Allocate EXCPH_gkkp
 !
 call EXCPH_gkkp_alloc()
 !
 ! Debug
 !call k_ibz2bz(k,"i",.false.)
 !do ik=1,k%nbz
 !!  write (1,*) k%ptbz(ik,:)
 !enddo
 !close(1)
 !
 ! K plus q table
 !
 call build_k_plus_q_table()
 !
 ! BSS_eh_table_m1
 !
 call build_inverse_eh_table(k%nbz)
 !
 ! Save the Lin in information
 ! 
 BS_mat_in         =BS_mat(:,EXCPH_states(1):EXCPH_states(2))
 !
 BSS_eh_table_in   =BSS_eh_table
 BSS_eh_table_m1_in=BSS_eh_table_m1(:,:,:,1,1)
 BS_E_in           =BS_E
 !
 ! Open gkkp_expanded
 !
 elph_branches_save  =elph_branches
 call io_control(ACTION=OP_RD,COM=REP,MODE=VERIFY,SEC=(/1/),ID=ID_gkkp)
 io_err=io_ELPH(ID_gkkp,'gkkp_expanded')
 call deliver_IO_error_message(io_err,"GKKP")
 elph_branches =elph_branches_save
 !
 if (io_err/=0) call error('El-Ph database not found')
 !
 ! Write headers
 !
 call io_control(ACTION=OP_WR,SEC=(/1/),ID=ID_exc_gkkp)
 io_err=io_EXCPH_gkkp(ID_exc_gkkp)
 !
 ! Get q-point coordinates in the q-BZ
 YAMBO_ALLOC(EXCPH_q,(q%nbz,3))
 call get_excph_q_momenta()
 !
 ! Main q-loop
 !
 !call k_ibz2bz(k,"i",.false.)
 call live_timing('Excitonic Gkkp',q%nbz*(elph_branches(2)-elph_branches(1)+1))
 !
 !open(47, file = 'BS_mat_q.dat')
 !write (47,*) "# BS_mat_q rotated: each q, each exciton state alpha, all transitions" 
 !
 do iq=1,q%nibz
   !
   if(iq>1) call EXCPH_load_L(iq,X,'check','Lout')
   call EXCPH_load_L(iq,X,'load','Lout')
   !
   do i_star=1,q%nstar(iq)
     !
     iq_s=q%star(iq,i_star)
     iq_bz = q%k_table(iq,iq_s)
     !
     BS_mat_out=cZERO
     ! Here we obtain the rotated BS_mat_out at iq_bz
     call EXC_WF_symm_qpoint(k,q,EXCPH_states(1),n_exc_in,iq,iq_s,1,ID_phases,S_index,BS_mat_out,l_min_mem)
     !
     EXCPH_gkkp          =cZERO
     EXCPH_gkkp_sq       =rZERO
     !
     do il=elph_branches(1),elph_branches(2)
       !
       call EXCPH_gkkp_eval(iq_s,iq_bz,il,k,q,ID_gkkp) ! Calculation of the exc-ph matrix element at iq_bz
       !
       call live_timing(steps=1)
       !
     enddo
     !
     !iq_db=Q_io_map(iq_bz)
     !if (iq_db/=iq_bz) write(*,*) "WARNING: iq_db/=iq_bz",iq_db,iq_bz
     !
     ! I/O
     !=====
     IO_ACT=manage_action(WR_CL_IF_END,iq_bz,1,q%nbz)
     call io_control(ACTION=IO_ACT,SEC=(/iq_bz+1/),ID=ID_exc_gkkp)
     io_err=io_EXCPH_gkkp(ID_exc_gkkp)
     !
   enddo
   !
 enddo
 !
 !close(47)
 !
 call live_timing( )
 !
 call BSS_free()
 call BSE_free()
 call BSE_in_free()
 call EXCPH_gkkp_free()
 !
 ! Report all relevant data of the exciton-phonon coupling
 !
 YAMBO_FREE(EXCPH_q)
 ! 
 call timing('EXCPH_gkkp',OPR='stop')
 !
 return
 !
 contains
   !
   subroutine get_excph_q_momenta()
     !    
     integer iq,i_star,iq_s,iq_bz
     !
     call k_ibz2bz(q,"i",.false.)
     !
     do iq=1,q%nibz
       !
       do i_star=1,q%nstar(iq)
         !
         iq_s=q%star(iq,i_star)
         iq_bz = q%k_table(iq, iq_s)
         !
         EXCPH_q(iq_bz,:) = q%ptbz(iq_bz,:)
         !
       enddo
     enddo
     !
     call k_ibz2bz(q,"d",.false.)
     !
   end subroutine get_excph_q_momenta
   !
   subroutine build_k_plus_q_table()
     !      
     use parallel_int,        ONLY:PARALLEL_global_indexes
     use R_lattice,           ONLY:bse_scattering
     !
     !integer, external ::qindx_B_init,qindx_B_close,io_QINDX 
     !integer :: qindx_ID,qindx_ID_frag,qindx_tmp(2),ID,io_err
     integer, external ::io_QINDX 
     integer :: ID,io_err
     integer :: ik_bz,iq_bz,ip_bz
     !
     if(.not.bse_scattering) call error(' Please run setup with BSEscatt flag ')
     !
     call PARALLEL_SETUP_K_scheme("Kdef")
     !
     ! here qindx_B is allocated
     call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/5/),ID=ID)
     io_err=io_QINDX(k,q,ID,'minus_q')
     if (io_err/=0) then
       call msg('s',"Missing k/q scattering database")
       call error("Please run a setup with the BSEscatt flag activated")
     endif
     !
     call live_timing('K plus q table',k%nbz)
     do ik_bz=1,k%nbz
       call live_timing(steps=1)
       IQ_bz_loop: do iq_bz=1,q%nbz
         do ip_bz=1,k%nbz
           if (qindx_B(ik_bz,ip_bz,1)==iq_bz) then
             k_plus_q_table(ik_bz,iq_bz)=ip_bz
             cycle IQ_bz_loop
           endif
         enddo
       enddo IQ_bz_loop
     enddo
     call live_timing( )
     !
   end subroutine build_k_plus_q_table
   !
end subroutine
