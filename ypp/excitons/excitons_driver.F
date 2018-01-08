!
!        Copyright (C) 2000-2018 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM DV DS
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
subroutine excitons_driver(k,Xk,en,Xen,q)
 !
 use pars,          ONLY:SP,pi
 use R_lattice,     ONLY:bz_samp
 use com,           ONLY:error
 use stderr,        ONLY:intc
 use X_m,           ONLY:X_t
 use QP_CTL_m,      ONLY:QP_apply
 use electrons,     ONLY:levels,n_spin,n_sp_pol,spin
 use YPP,           ONLY:l_sort,l_exc_wf,l_spin,l_amplitude,excitons_sort_and_report
 use IO_m,          ONLY:io_control,OP_RD_CL,DUMP,NONE,REP,VERIFY
 use BS_solvers,    ONLY:BSS_eh_table,BSS_write_eig_2_db,io_BSS_diago,BS_mat,BSS_n_eig
 use BS,            ONLY:BS_H_dim,BS_K_dim,BS_bands,BS_q,BS_K_coupling,BS_cpl_mode,&
&                        BS_cpl_K_exchange,BS_cpl_K_corr
#if defined _YPP_ELPH
 use YPP_ELPH,      ONLY:l_eliashberg,l_gkkp
#endif
#if defined _YPP_RT
 use YPP_real_time, ONLY:l_RT_abs
#endif
 !
#include<memory.h>
 !
 type(bz_samp) ::Xk,k,q
 type(levels)  ::Xen,en
 !
 ! Work Space
 !
 logical          :: l_flag
 !
 integer :: n_lambda
 integer ,allocatable :: BS_E_degs(:)
 integer ,allocatable :: lambda_s(:)
 !
 !... I/0
 !
 integer           ::io_err,io_ID,i_sec
 integer, external ::io_BS,io_E_SOC_map
 type(X_t)         ::Xbsk
 !
 !... Energies and Residulas
 !
 complex(SP), allocatable ::BS_R_left(:)
 complex(SP), allocatable ::BS_R_right(:)
 complex(SP), allocatable ::BS_E(:)
 !
 real(SP),    pointer     ::BS_E_SOC_corr(:,:) => null()
 real(SP),    pointer     ::BS_R_PL(:,:)       => null()
 complex(SP), pointer     ::BS_R_kerr(:)       => null()
 !
 integer                  ::iq,neh
 !
 call section('*','Excitonic Properties')
 !
 ! GPL_EXCLUDE_START
 !
 ! E SOC perturbative
 ! 
 if(n_spin==1) then
   call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1/),MODE=DUMP,ID=io_ID)
   io_err=io_E_SOC_map(en,k,io_ID)
 endif
 !
 ! GPL_EXCLUDE_END
 !
 !
 ! Here I read information from the BS database
 !
 call io_control(ACTION=OP_RD_CL,COM=NONE,MODE=DUMP,SEC=(/1/),ID=io_ID)
 io_err=io_BS(1,Xbsk,io_ID) 
 !
 if (io_err/=0) call error(' ndb.BS not found or not compatible with ypp input')
 !
 ! X type. Also used in PARALLEL setup
 !
 Xbsk%whoami=2
 !
 ! Apply quasi-particle correction if presents
 ! 
 call QP_apply(BS_bands,Xen,Xk,'G',msg_fmt='s')
 !
 if (BS_cpl_K_exchange)                   BS_cpl_mode='x'
 if (BS_cpl_K_corr)                       BS_cpl_mode='c'
 if (BS_cpl_K_exchange.and.BS_cpl_K_corr) BS_cpl_mode='xc'
 !
 BS_H_dim=BS_K_dim
 if (BS_K_coupling) BS_H_dim=2*BS_K_dim
 !
 ! Get state indexes
 !
 n_lambda = get_lambda()
 !
 l_flag=l_sort .or. (l_exc_wf.and.n_lambda>0) .or. (l_amplitude.and.n_lambda>0) .or. l_spin
#if defined _YPP_ELPH
 l_flag=l_flag .or. l_eliashberg .or. l_gkkp
#endif
#if defined _YPP_RT
 l_flag=l_flag .or. l_RT_abs
#endif
 !
 if(.not.l_flag) return
 !
 YAMBO_ALLOC(lambda_s,(n_lambda))
 n_lambda = get_lambda()
 !
 ! Allocation of energies and residuum, then also eigenvector
 !
 YAMBO_ALLOC(BS_R_right,(BS_H_dim))
 YAMBO_ALLOC(BS_R_left,(BS_H_dim))
 YAMBO_ALLOC(BS_E,(BS_H_dim))
 iq=BS_q(1)
 !
 if(en%nb_SOC/=0) then
   YAMBO_ALLOC_P(BS_E_SOC_corr,(2/n_sp_pol,BS_H_dim))
 endif
 !
 ! Here I read energies and residuum
 !
 call io_control(ACTION=OP_RD_CL,COM=NONE,MODE=DUMP,SEC=(/1/),ID=io_ID)
 io_err=io_BSS_diago(iq,io_ID,BS_H_dim,BSS_n_eig,BS_E,BS_R_left,BS_R_right,BS_E_SOC_corr,BS_R_kerr,BS_R_PL)
 !
 if (io_err/=0) call error(' ndb.BSS_diago not found')
 !
 do i_sec=1,2
   call io_control(ACTION=OP_RD_CL,COM=REP,MODE=VERIFY,SEC=(/i_sec/),ID=io_ID)
   io_err=io_BSS_diago(iq,io_ID,BS_H_dim,BSS_n_eig,BS_E,BS_R_left,BS_R_right,BS_E_SOC_corr,BS_R_kerr,BS_R_PL)
   if (io_err/=0) call error(' ndb.BSS_diago, SEC '//trim(intc(i_sec))//', not compatible with ypp input')
 enddo

 !
 ! Sort energies and write to output
 !===================================
 if (l_sort) call excitons_sort_and_report(Xen,BS_R_right,BS_E,BS_E_SOC_corr=BS_E_SOC_corr)
 !
 if(.not.l_sort) then
   !
   if(.not.BSS_write_eig_2_db) &
&    call error(' Diagonalization database does not contain excitonic eigenstates.'//&
&               ' Re-run yambo using the WRbsWF flag in the input file.')
   !
   YAMBO_ALLOC(BS_E_degs,(BSS_n_eig))
   !
   ! Sort energies to find degenerate states
   !========================================
   call excitons_find_degeneracies(BS_E,BS_E_degs)
   !
   ! Loading tables and eigenvectors
   !
   YAMBO_ALLOC(BSS_eh_table,(BS_H_dim,3+n_sp_pol-1))
   YAMBO_ALLOC(BS_mat,(BS_H_dim,BSS_n_eig))
   !
   call io_control(ACTION=OP_RD_CL,COM=REP,MODE=VERIFY,SEC=(/3/),ID=io_ID)
   io_err=io_BSS_diago(iq,io_ID,BS_H_dim,BSS_n_eig,BS_E,BS_R_left,BS_R_right,BS_E_SOC_corr,BS_R_kerr,BS_R_PL)
   if (io_err/=0) call error(' ndb.BSS_diago, SEC 3, not compatible with ypp input')
   !
   ! Create the anti-resonant part of the eh_table
   !
   if (BS_K_coupling) then
     do neh=BS_K_dim+1,BS_H_dim
       BSS_eh_table(neh,:)=BSS_eh_table(neh-BS_K_dim,1)
       !
       ! invert conduction <-> valence order
       !
       BSS_eh_table(neh,2)=BSS_eh_table(neh-BS_K_dim,3)
       BSS_eh_table(neh,3)=BSS_eh_table(neh-BS_K_dim,2)
     enddo
   endif
   !
 endif
 ! 
 ! Exciton's spin
 !=====================
 if (l_spin)       call exciton_spin(Xk,Xen,BS_R_right,BS_E,BS_E_degs,lambda_s,n_lambda)
 !
 ! Exciton's Amplitude
 !=====================
 if (l_amplitude)  call excitons_amplitudes(Xk,Xen,BS_E_degs,lambda_s,n_lambda)
 !
 ! Exciton's WFs
 !=====================
 if (l_exc_wf)     call exciton_WFs(Xk,BS_E_degs,lambda_s,n_lambda)
 !
 !
 !GPL_EXCLUDE_START
 !
 ! Excitonic Eliashberg function 
 !================================
#if defined _YPP_ELPH
 !
 if (l_eliashberg) call ELPH_general_gFsq(k,en,Xk,Xen,q,BS_E_degs,lambda_s,n_lambda)
 !
 if (l_gkkp)       call ELPH_excitonic_gkkp(Xk,lambda_s,n_lambda)
 !
#endif
 !
#if defined _YPP_RT
 !
 ! Transient absorption 
 !======================
 if(l_RT_abs)      call RT_transient_absorption(Xbsk,Xen,Xk,q,BS_H_dim,BS_E,BS_R_right)
 !
#endif
 !
 !GPL_EXCLUDE_END
 !
 !
 ! CLEAN
 !
 YAMBO_FREE(BS_R_left)
 YAMBO_FREE(BS_R_right)
 YAMBO_FREE(BS_E)
 YAMBO_FREE(lambda_s)
 if(.not.l_sort) then
   YAMBO_FREE(BSS_eh_table)
   YAMBO_FREE(BS_mat)
 endif
 if(associated(BS_E_SOC_corr)) then
   YAMBO_FREE_P(BS_E_SOC_corr)
 endif
 !
 contains
   !
    integer function get_lambda()
     !
     use pars,   ONLY:schlen
     use stderr, ONLY:string_split
     use YPP,    ONLY:state_ctl
     !
     implicit none
     !
     integer          ::i_start,i_end,i_str,i_st
     character(schlen)::str_piece(50)
     !
     call string_split(state_ctl,str_piece)
     !
     get_lambda=0
     if (allocated(lambda_s)) lambda_s=0
     !
     i_str=1
     !
     do while (i_str<50)
       !
       if (len_trim(str_piece(i_str))==0) exit
       !
       if (trim(str_piece(i_str+1))=="-") then
         read(str_piece(i_str  ),*) i_start
         read(str_piece(i_str+2),*) i_end
         i_str=i_str+3
       else
         read(str_piece(i_str),*) i_start
         i_end=i_start
         i_str=i_str+1
       endif
       !
       do i_st=i_start,i_end
         !
         if (allocated(BS_E_degs)) then
           if (i_st>i_start.and.BS_E_degs(i_st)/=i_st) cycle
         endif
         !
         if (allocated(lambda_s)) then
           if (.not.any(lambda_s==i_st)) then
             get_lambda=get_lambda+1
             lambda_s(get_lambda)=i_st
           endif
         else
           get_lambda=get_lambda+1
         endif
       enddo
     enddo
     !
   end function
   !
   !
end subroutine
