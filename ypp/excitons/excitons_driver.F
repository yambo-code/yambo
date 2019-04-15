!
!        Copyright (C) 2000-2019 the YAMBO team
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
 use pars,          ONLY:SP,pi,schlen
 use R_lattice,     ONLY:bz_samp
 use com,           ONLY:error
 use stderr,        ONLY:intc
 use X_m,           ONLY:X_t
 use QP_CTL_m,      ONLY:QP_apply
 use electrons,     ONLY:levels,n_spin,n_sp_pol,spin,E_SOC_nbands
 use YPP,           ONLY:l_sort,l_exc_wf,l_spin,l_amplitude,excitons_sort_and_report,EXCITONS_user_indexes
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
 integer ,allocatable :: BS_E_degs(:)
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
 iq=BS_q(1)
 !
 ! X type. Also used in PARALLEL setup
 !
 Xbsk%whoami=2
 !
 ! Here I read information from the BSS_diago database
 !
 call io_control(ACTION=OP_RD_CL,COM=NONE,MODE=DUMP,SEC=(/1/),ID=io_ID)
 io_err=io_BSS_diago(iq,io_ID,Xbsk)
 !
 if (io_err/=0) call error(' ndb.BSS_diago not found')
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
 l_flag=l_sort .or. l_exc_wf .or.l_amplitude .or. l_spin
#if defined _YPP_ELPH
 l_flag=l_flag .or. l_eliashberg .or. l_gkkp
#endif
#if defined _YPP_RT
 l_flag=l_flag .or. l_RT_abs
#endif
 !
 if(.not.l_flag) return
 !
 ! Allocation of energies and residuum, then also eigenvector
 !
 YAMBO_ALLOC(BS_R_right,(BSS_n_eig))
 YAMBO_ALLOC(BS_R_left,(BSS_n_eig))
 YAMBO_ALLOC(BS_E,(BSS_n_eig))
 if(E_SOC_nbands/=0) then
   YAMBO_ALLOC_P(BS_E_SOC_corr,(2/n_sp_pol,BSS_n_eig))
 endif
 !
 do i_sec=1,2
   call io_control(ACTION=OP_RD_CL,COM=REP,MODE=VERIFY,SEC=(/i_sec/),ID=io_ID)
   io_err=io_BSS_diago(iq,io_ID,Xbsk,BS_E,BS_R_left,BS_R_right,BS_E_SOC_corr,BS_R_kerr,BS_R_PL)
   if (io_err/=0) call error(' ndb.BSS_diago, SEC '//trim(intc(i_sec))//', not compatible with ypp input')
 enddo
 !
 ! Sort energies and write to output
 !===================================
 if (l_sort) call excitons_sort_and_report(BS_R_right,BS_E,BS_E_SOC_corr=BS_E_SOC_corr)
 !
 ! Sort energies to find degenerate states
 !========================================
 YAMBO_ALLOC(BS_E_degs,(BSS_n_eig))
 call excitons_find_degeneracies(BS_E,BS_E_degs)
 !
 ! Define the USER set of excitonic states
 !=========================================
 call get_user_excitonic_states( )
 !
 if(.not.l_sort) then
   !
   if(.not.BSS_write_eig_2_db) &
&    call error(' Diagonalization database does not contain excitonic eigenstates.'//&
&               ' Re-run yambo using the WRbsWF flag in the input file.')
   !
   ! Adjust the user provided indexes to the average ones.
   !
   ! Loading tables and eigenvectors
   !
   YAMBO_ALLOC(BSS_eh_table,(BS_H_dim,3+n_sp_pol-1))
   YAMBO_ALLOC(BS_mat,(BS_H_dim,BSS_n_eig))
   !
   call io_control(ACTION=OP_RD_CL,COM=REP,MODE=VERIFY,SEC=(/3/),ID=io_ID)
   io_err=io_BSS_diago(iq,io_ID,Xbsk,BS_E,BS_R_left,BS_R_right,BS_E_SOC_corr,BS_R_kerr,BS_R_PL)
   if (io_err/=0) call error(' ndb.BSS_diago, SEC 3, not compatible with ypp input')
   !
   ! Create the anti-resonant part of the eh_table
   !
   if (BS_K_coupling) then
     do neh=BS_K_dim+1,BS_H_dim
       !
       BSS_eh_table(neh,:)=BSS_eh_table(neh-BS_K_dim,:)
       !
       ! invert conduction <-> valence order
       !
       BSS_eh_table(neh,2)=BSS_eh_table(neh-BS_K_dim,3)
       BSS_eh_table(neh,3)=BSS_eh_table(neh-BS_K_dim,2)
       !
     enddo
   endif
   !
 endif
 ! 
 ! Exciton's spin
 !=====================
 if (l_spin)       call excitons_spin(Xk,BS_R_right,BS_E,BS_E_degs)
 !
 ! Exciton's Amplitude
 !=====================
 if (l_amplitude)  call excitons_amplitudes(Xk,Xen,BS_E_degs)
 !
 ! Exciton's WFs
 !=====================
 if (l_exc_wf)     call exciton_WFs(Xk,BS_E_degs)
 !
 !
 !GPL_EXCLUDE_START
 !
 ! Excitonic Eliashberg function 
 !================================
#if defined _YPP_ELPH
 !
 if (l_eliashberg) call ELPH_general_gFsq(k,en,Xk,Xen,q,BS_E_degs)
 !
 if (l_gkkp)       call ELPH_excitonic_gkkp(Xk)
 !
#endif
 !
#if defined _YPP_RT
 !
 ! Transient absorption 
 !======================
 if(l_RT_abs)      call RT_transient_absorption(Xen,Xk,q,BS_H_dim,BS_E,BS_R_right)
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
 YAMBO_FREE(EXCITONS_user_indexes)
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
   subroutine get_user_excitonic_states( )
     !
     use pars,   ONLY:schlen
     use stderr, ONLY:STRING_split,intc
     use YPP,    ONLY:state_ctl,EXCITONS_user_indexes,EXCITONS_n_user_states
     use com,    ONLY:warning
     !
     implicit none
     !
     integer          ::i_start,i_end,i_str,i_st,i_st_p,TMP_index(BSS_n_eig),n_user
     character(schlen)::str_piece(50)
     !
     call STRING_split(state_ctl,str_piece)
     !
     EXCITONS_n_user_states=0
     n_user    =0
     i_str     =1
     TMP_index =0
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
         n_user=n_user+1
         !
         do i_st_p=1,BSS_n_eig
           if (BS_E_degs(i_st_p)==BS_E_degs(i_st)) then
             if (.not.any(TMP_index==i_st_p)) then
               EXCITONS_n_user_states=EXCITONS_n_user_states+1
               TMP_index(EXCITONS_n_user_states)=i_st_p
             endif
           endif
         enddo
         !
       enddo
     enddo
     !
     if (EXCITONS_n_user_states>n_user) call warning(trim(intc(EXCITONS_n_user_states-n_user))//&
&                                                    " excitonic states added to the USER selection to preserve degeneracy")
     !
     YAMBO_ALLOC(EXCITONS_user_indexes,(EXCITONS_n_user_states))
     EXCITONS_user_indexes=TMP_index(:EXCITONS_n_user_states)
     !
   end subroutine
   !
end subroutine
