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
 use YPP_interfaces,ONLY:excitons_sort_and_report
 use YPPm,          ONLY:l_sort,l_exc_wf,l_spin,l_amplitude,EXCITONS_user_indexes,BSiq, &
&                        BS_R_left,BS_R_right,BS_E,BS_E_SOC_corr,BS_R_PL,BS_R_kerr,Xbsk
 use BS_solvers,    ONLY:BSS_eh_table,BSS_write_eig_2_db,io_BSS_diago,BS_mat,BSS_n_eig
 use BS,            ONLY:BS_H_dim,BS_K_dim,BS_bands,BS_K_coupling,BS_cpl_mode,&
&                        BS_cpl_K_exchange,BS_cpl_K_corr,BSqpts
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
 !
 integer                  ::iq,neh
 !
 call section('*','Excitonic Properties @ Q-index #'//trim(intc(BSiq)))
 !
 iq = BSiq
 !
 call excitons_read(k,Xk,en,Xen,BSiq,"check")
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
 call excitons_read(k,Xk,en,Xen,BSiq,"eigenvalues")
 !
 ! Sort energies and write to output
 !===================================
 if (l_sort) call excitons_sort_and_report(iq,BS_R_right,BS_E,BS_E_SOC_corr=BS_E_SOC_corr)
 !
 ! Sort energies to find degenerate states
 !========================================
 YAMBO_ALLOC(BS_E_degs,(BSS_n_eig))
 call excitons_find_degeneracies(BS_E,BS_E_degs)
 !
 ! Define the USER set of excitonic states
 !=========================================
 if(.not.l_sort) call get_user_excitonic_states( )
 !
 if(.not.l_sort) call excitons_read(k,Xk,en,Xen,BSiq,"eigenvectors")
 ! 
 ! Exciton's spin
 !=====================
 if (l_spin)       call excitons_spin(Xk,BS_R_right,BS_E,BS_E_degs)
 !
 ! Exciton's Amplitude
 !=====================
 if (l_amplitude)  call excitons_amplitudes(Xk,Xen,BS_E_degs,iq)
 !
 ! Exciton's WFs
 !=====================
 if (l_exc_wf)     call exciton_WFs(Xk,BS_E_degs,iq)
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
 ! CLEAN
 !
 call excitons_read(k,Xk,en,Xen,BSiq,"clean")
 !
 YAMBO_FREE(EXCITONS_user_indexes)
 !
 contains
   !
   subroutine get_user_excitonic_states( )
     !
     use pars,   ONLY:schlen
     use stderr, ONLY:STRING_split,intc
     use YPPm,   ONLY:state_ctl,EXCITONS_user_indexes,EXCITONS_n_user_states,EXCITON_E_treshold,EXCITON_Res_treshold
     use com,    ONLY:warning,msg
     !
     implicit none
     !
     integer          ::i_start,i_end,i_str,i_st,TMP_index(BSS_n_eig),n_user
     real(SP)         ::Residuals(BSS_n_eig)
     character(schlen)::str_piece(50)
     !
     call STRING_split(state_ctl,str_piece)
     !
     EXCITONS_n_user_states=0
     n_user    =0
     TMP_index =0
     !
     ! 1. Using the "States" variable
     !--------------------------------
     i_str     =1
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
         call ADD_me_to_the_list(i_st,TMP_index)
         !
       enddo
     enddo
     !
     ! 2. Using the "En_treshold" variable
     !------------------------------------
     if (EXCITON_E_treshold>0.) then
       do i_st=1,BSS_n_eig
         if (real(BS_E(i_st))<EXCITON_E_treshold) call ADD_me_to_the_list(i_st,TMP_index)
       enddo
     endif
     !
     ! 3. Using the "Res_treshold" variable
     !--------------------------------------
     if (EXCITON_Res_treshold>0.) then
       Residuals(:) = BS_R_right(:)*conjg(BS_R_right(:))
       do i_st=1,BSS_n_eig
         if (real(Residuals(i_st))>EXCITON_Res_treshold*maxval(Residuals)) call ADD_me_to_the_list(i_st,TMP_index)
       enddo
     endif
     !
     if (EXCITONS_n_user_states==0) return
     !
     if (EXCITONS_n_user_states>n_user) call warning(" "//trim(intc(EXCITONS_n_user_states-n_user))//&
&                                                    " excitonic states added to the USER selection to preserve degeneracy")
     call msg("s"," "//trim(intc(EXCITONS_n_user_states))//" excitonic states selected")
     !
     YAMBO_ALLOC(EXCITONS_user_indexes,(EXCITONS_n_user_states))
     EXCITONS_user_indexes=TMP_index(:EXCITONS_n_user_states)
     !
   end subroutine
   !
   subroutine ADD_me_to_the_list(i_state,TMP_index)
     !
     use YPPm,   ONLY:EXCITONS_n_user_states
     !
     integer i_st_p,i_state,TMP_index(BSS_n_eig)
     !
     do i_st_p=1,BSS_n_eig
       if (BS_E_degs(i_st_p)==BS_E_degs(i_state)) then
         if (.not.any(TMP_index==i_st_p)) then
           EXCITONS_n_user_states=EXCITONS_n_user_states+1
           TMP_index(EXCITONS_n_user_states)=i_st_p
         endif
       endif
     enddo
   end subroutine
   !
end subroutine
