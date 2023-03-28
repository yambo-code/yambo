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
 use electrons,      ONLY:levels,n_sp_pol
 use R_lattice,      ONLY:bz_samp,qindx_B !_load
 use parser_m,       ONLY:parser
 use X_m,            ONLY:X_t
 use BS_solvers,     ONLY:BS_mat,BSS_eh_table,BS_H_dim,BSS_n_eig,BSS_alloc,BSS_free
 use BS,             ONLY:BS_H_dim,BS_bands,BSqpts
 use ELPH,           ONLY:elph_branches,Q_io_map,l_GKKP_expanded_DB_exists
 use IO_m,           ONLY:NONE,OP_APP_CL,OP_RD_CL,DUMP,OP_RD,RD_CL_IF_END,OP_WR_CL,deliver_IO_error_message, &
&                         REP,VERIFY,WR_CL,OP_WR,WR_CL_IF_END,manage_action
 use IO_int,         ONLY:io_control
 use EXCPH,          ONLY:BS_mat_in,BSS_eh_table_in,EXCPH_gkkp,EXCPH_gkkp_sq,BS_E_in,BS_E,BSS_eh_table_m1_in,&
&                         EXCPH_states,EXCPH_sum,BSS_eh_table_m1,k_plus_q_table,build_Q_map,EXCPH_q, &
&                         EXCPH_gkkp_alloc,EXCPH_gkkp_free,l_const_elph,l_abs_elph,l_abs_exc,l_elec_only,l_hole_only, &
&                         n_exc_in,EXCPH_kind,EXCPH_kind_name,PALEARI,BERNARDI,BSE_alloc,BSE_free
 use LIVE_t,         ONLY:live_timing
 use timing_m,       ONLY:timing
 use com,            ONLY:msg
 !
#include<memory.h>
 !
 type(levels)  ::E
 type(bz_samp) ::k,q
 type(X_t)     ::X
 !
 ! Work Space
 !
 integer, external :: io_ELPH,io_EXCPH_gkkp,io_RIM
 integer  :: ID_gkkp,ID_exc_gkkp,io_err,io_ID,IO_ACT
 integer  :: il,iq_bz,iq,i_star,iq_s,iq_db,elph_branches_save(2)
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
 if(l_abs_elph) call warning('Absolute value of electron-phonon coupling ')
 call parser('AbsExc',   l_abs_exc)
 if(l_abs_exc)  call warning('Absolute value of exciton-phonon coupling  ') 
 call parser('ElectronContributionOnly', l_elec_only)
 call parser('HoleContributionOnly', l_hole_only)
 !
 ! Exciton-phonon kind
 !
 select case (trim(EXCPH_kind_name))
    case('PALEARI')
        EXCPH_kind=PALEARI
    case('BERNARDI')
        EXCPH_kind=BERNARDI
    case default
        call error("Unknow exciton-phonon coupling type")
 end select
 !
 call msg('rs','Use coupling convention from',trim(EXCPH_kind_name)) 
 !
 elph_branches_save  =elph_branches
 call io_control(ACTION=OP_RD_CL,COM=REP,MODE=DUMP,SEC=(/1/),ID=ID_gkkp)
 io_err=io_ELPH(ID_gkkp,'gkkp_expanded') ! For now we have to work in the full BZ by turning all symmetries off.
 elph_branches =elph_branches_save
 !
 ! I build the map only in the case without symmetries
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
 call EXCPH_load_L(1,X,'check','Lin') ! At present the Lin can be only gamma iq=1
 !
 call k_build_up_BZ_tables(k)
 call k_build_up_BZ_tables(q)
 !
 call BSS_alloc()
 !
 call BSE_alloc(k,q)
 !
 call EXCPH_load_L(1,X,'load','Lin')
 !
 ! Alloc BS_in variables
 ! 
 call BSE_in_alloc()
 !
 ! Alloage EXCPH_gkkp
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
 call build_inverse_eh_table()
 !
 ! Save the Lin in information
 ! 
 BS_mat_in         =BS_mat(:,EXCPH_states(1):EXCPH_states(2))
 BSS_eh_table_in   =BSS_eh_table
 BSS_eh_table_m1_in=BSS_eh_table_m1
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
 do iq=1,q%nibz
   ! 
   if(iq>1) call EXCPH_load_L(iq,X,'check','Lout')
   call EXCPH_load_L(iq,X,'load','Lout')
   !
   do i_star=1,q%nstar(iq)
     !
     iq_s=q%star(iq,i_star)
     iq_bz = q%k_table(iq, iq_s)
     !
     EXCPH_gkkp          =cZERO
     EXCPH_gkkp_sq       =rZERO
     !
     do il=elph_branches(1),elph_branches(2)
       !
       if(EXCPH_kind==PALEARI) then
         !      
         call EXCPH_gkkp_eval_pal(iq_s,iq_bz,il,k,q,ID_gkkp)
         ! 
       elseif(EXCPH_kind==BERNARDI) then
         !      
         call EXCPH_gkkp_eval_ber(iq_s,iq_bz,il,k,q,ID_gkkp)
         !
       endif
       !
       call live_timing(steps=1)
       !
     enddo
     !
     ! Add the WEIGHTs factor to calculate the renormalization factors
     !
     iq_db=Q_io_map(iq_bz)
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
 ! [FP] DEBUG
 ! call compare_qindxC_table()
 ! [FP] END DEBUG
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
   subroutine BSE_in_alloc()
     implicit none
     !      
     ! pre-allocs...
     !
     YAMBO_ALLOC(BS_E_in,(BSS_n_eig))
     YAMBO_ALLOC(BS_mat_in,(BS_H_dim,EXCPH_states(1):EXCPH_states(2)))
     YAMBO_ALLOC(BSS_eh_table_in,(BS_H_dim,3+n_sp_pol-1))
     YAMBO_ALLOC(BSS_eh_table_m1_in,(k%nbz,BS_bands(1):BS_bands(2),BS_bands(1):BS_bands(2)))
     !
     BS_mat_in=cZERO
     BS_E_in  =cZERO
     BSS_eh_table_in     = 0
     BSS_eh_table_m1_in  = 0
     !
   end subroutine BSE_in_alloc
   !
   subroutine BSE_in_free
     implicit none
     YAMBO_FREE(BS_E_in)
     YAMBO_FREE(BS_mat_in)
     YAMBO_FREE(BSS_eh_table_in)
     YAMBO_FREE(BSS_eh_table_m1_in)
   end subroutine BSE_in_free
   !
   subroutine get_excph_q_momenta()
     !    
     integer iq,i_star,iq_s,iq_bz
     !
     call k_ibz2bz(q,"i",.false.)
     !
!     write(*,*)
!     write(*,*) 'Excitoni q-list in the BZ '
     do iq=1,q%nibz
       !
       do i_star=1,q%nstar(iq)
         !
         iq_s=q%star(iq,i_star)
         iq_bz = q%k_table(iq, iq_s)
!         write(*,*) iq_bz,q%ptbz(iq_bz,:)
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
   subroutine build_inverse_eh_table()
     !
     integer :: iv,ic,i_H,ik_bz
     !     
     call live_timing('BSE inverse eh-table',BS_H_dim)
     do i_H=1,BS_H_dim
       !
       ik_bz = BSS_eh_table(i_H,1)
       iv    = BSS_eh_table(i_H,2)
       ic    = BSS_eh_table(i_H,3)
       !
       BSS_eh_table_m1(ik_bz,iv,ic)=i_H
       call live_timing(steps=1)
       !
     enddo
     call live_timing( )
     !
   end subroutine build_inverse_eh_table
   !
   subroutine compare_qindxC_table()
     !
     ! [FP] DEBUG FUNCTION (to be removed?)
     !
     use R_lattice, ONLY:qindx_C
     !
     integer :: ik_plus_iq_C, ik_plus_iq_tbl, ik_bz
     integer :: iq,i_star,iq_s,iq_bz
     !
     !call build_k_plus_q_table()
     call k_ibz2bz(q,"i",.false.)
     call k_ibz2bz(k,"i",.false.)
     !
     do ik_bz=1, k%nbz
       write(*,*) ik_bz,k%ptbz(ik_bz,:)
     enddo
     !
     do iq=1,q%nibz
       !
       do i_star=1,q%nstar(iq)
         !
         iq_s=q%star(iq,i_star)
         iq_bz = q%k_table(iq, iq_s)
         !
         do ik_bz=1, k%nbz
           !
           ik_plus_iq_C   = qindx_C(ik_bz,iq_bz,1)
           ik_plus_iq_tbl = k_plus_q_table(ik_bz,iq_bz)
           write(*,*) k%ptbz(ik_bz,:),q%ptbz(iq_bz,:)
           write(*,*) k%ptbz(ik_plus_iq_C,:),ik_plus_iq_C
           write(*,*) k%ptbz(ik_plus_iq_tbl,:),ik_plus_iq_tbl
           !
         enddo
         !
       enddo
     enddo
     !
     call k_ibz2bz(q,"d",.false.)
     call k_ibz2bz(k,"d",.false.)
     !
   end subroutine compare_qindxC_table
   !
    !
end subroutine
