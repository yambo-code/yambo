!
!        Copyright (C) 2000-2021 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM FP AC
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
!> @brief Calculate phonon-assisted absorption and emission (see PRL 122, 187401 )
!!
subroutine EXCPH_optics(E,k,q,X,wv)
 !
 use units,          ONLY:HA2EV
 use pars,           ONLY:SP,cZERO,rZERO,schlen,pi
 use electrons,      ONLY:levels,spin_occ
 use R_lattice,      ONLY:bz_samp,bare_qpg,d3k_factor
 use X_m,            ONLY:X_t
 use com,            ONLY:msg,of_open_close
 use EXCPH,          ONLY:EXCPH_sum,EXCPH_states,L_kind_in,L_kind_out,BS_E,BS_all_E,BS_E_in, &
&                         BS_R_left,BS_R_right,BS_R,BS_Sat_W_DG,BS_Sat_W_DG_PH_abs,free_EXC,alloc_EXC, &
&                         l_DbGd_PH_only,l_DbGd_WEIGHTs,l_no_matrix_elements
 use ELPH,           ONLY:use_PH_DbGd,ph_modes
 use frequency,      ONLY:w_samp
 use parser_m,      ONLY:parser
 !
#include<memory.h>
 !
 type(levels)  ::E
 type(bz_samp) ::k,q
 type(X_t)     ::X
 type(w_samp)  :: wv 
 !
 ! Work space
 !
 complex(SP), allocatable :: PL_ii(:,:),Epsilon_ii(:,:)
 integer, parameter  :: n_output=7
 real(SP)            :: output_data(n_output)
 character(schlen)   :: headers(n_output)
 integer             :: iw,iq,i_alpha,max_filling
 character(schlen)   :: dbgd_string
 !
 call section('*','EXCPH-Optics')
 !
 ! Load excitonic energies 
 !
 if(L_kind_in/=L_kind_out) &
&     call warning(' Lkind_in /= Lkind_out this can generate inconsistencies in the luminescence! ')
 !
 call EXCPH_load_L(1,X,'check','Lout')
 !
 ! Allocations 
 !
 call alloc_EXC(q)
 !
 ! Load excitonic energies and residuals
 !
 call EXCPH_load_L(1,X,'eigenvalues residuals','Lin')
 BS_E_in(:)=BS_E(EXCPH_states(1):EXCPH_states(2)) ! L_in can be different from L_out 
 !
 do iq=1,q%nibz
   call EXCPH_load_L(iq,X,'eigenvalues','Lout')
   BS_all_E(:,iq)=real(BS_E(:)) ! I suppose the BSE eigenvalues are real
 enddo
 !
 ! Calculate the residual
 !
 BS_R=rZERO
 do i_alpha=EXCPH_states(1),EXCPH_states(2)
   BS_R(i_alpha)=real(abs(BS_R_left(i_alpha)*BS_R_right(i_alpha)),SP)
 enddo
 !
 ! This formula can be wrong in presence of the 2D-cutoff
 BS_R=BS_R*real(spin_occ,SP)/(2._SP*pi)**3*d3k_factor*4._SP*pi/bare_qpg(1,1)**2
 !
 call parser('NoMatrxEl',l_no_matrix_elements)
 !  
 ! Turn on double-grid if presents
 call EXCPH_double_grid_setup(q)
 !
 dbgd_string=''
 !
 call msg('rs','Double-grid                  :',use_PH_DbGd)
 !
 if(use_PH_DbGd) then
   dbgd_string='DbGd'
   max_filling=maxval(q%FGbz%N_mapped)+1  ! max number of tilde q around each q-point include the q itself
   YAMBO_ALLOC(BS_Sat_W_DG,(ph_modes,EXCPH_sum(2),EXCPH_states(2),max_filling))
   YAMBO_ALLOC(BS_Sat_W_DG_PH_abs,(ph_modes,EXCPH_sum(2),EXCPH_states(2),max_filling))
   BS_Sat_W_DG       =rZERO
   BS_Sat_W_DG_PH_abs=rZERO
   !
   ! Check double-grid flags
   !
   call parser('DbGdWEIGHTs',l_DbGd_WEIGHTs)
   call parser('DbGdOnlyPh', l_DbGd_PH_only)
   !
   call msg('rs','Double-grid for satellites   :',l_DbGd_WEIGHTs)
   call msg('rs','Double-grid only for phonons :',l_DbGd_PH_only) 
   !
 endif
 !
 ! Frequencies and damping setup
 call FREQUENCIES_setup(wv)
 YAMBO_ALLOC(Epsilon_ii,(wv%n_freqs,3))
 YAMBO_ALLOC(PL_ii,(wv%n_freqs,3))
 !
 ! Calculate X(w) include phonon assisted transitions
 !
 call EXCPH_X_phass(wv,q,Epsilon_ii,PL_ii)
 !
! call X_setup_file_names(1,'bse ','phass',dbgd_string) 
 !
 call print_output()
 !
 ! Deallocations
 !
 call free_EXC()
 YAMBO_FREE(Epsilon_ii)
 YAMBO_FREE(PL_ii)
 if(use_PH_DbGd) then
   YAMBO_FREE(BS_Sat_W_DG)
   YAMBO_FREE(BS_Sat_W_DG_PH_abs)
 endif
 !
 call FREQUENCIES_reset(wv,"all")
 !
 contains
   !
   subroutine print_output()
!     use X_output,      ONLY:X_write_messages_before_headers,X_write_q_plus_G
     implicit none
     !
     character(schlen) :: eps_file_name,pl_file_name
     !
     eps_file_name='eps_bse_ph_ass'
     pl_file_name ='pl_bse_ph_ass'
     if(use_PH_DbGd) then
       eps_file_name='eps_bse_ph_ass_dbgd'
       pl_file_name ='pl_bse_ph_ass_dbgd'
     endif
     !
     call of_open_close(eps_file_name,'ot')
     call of_open_close(pl_file_name,'ot')
     !
!     call X_write_messages_before_headers(1,.FALSE.,.FALSE.,'r ')
     !
!     call X_write_q_plus_G(1,(/0._SP,0._SP,0._SP/),1)
     !
     call msg('o eps_bse pl_bse','# EXCITON PHONON')    
     call msg('o eps_bse pl_bse','# Exciton_states    :',EXCPH_states,INDENT=0)    
     call msg('o eps_bse pl_bse','# EXciton sum       :',EXCPH_sum,INDENT=0)    
     if(use_PH_DbGd) then
       call msg('o eps_bse pl_bse','# Double-grid for satellites   :',l_DbGd_WEIGHTs,INDENT=0)    
       call msg('o eps_bse pl_bse','# Double-grid only for phonons :',l_DbGd_PH_only,INDENT=0)    
     endif
     call msg('o eps_bse pl_bse','#')
     headers(1)='E[eV]'
     headers(2:n_output)=(/'Im(eps_ph )','Re(eps_ph )','Im(eps_nos)','Re(eps_nos)','Re(eps_sat)','Im(eps_sat)'/)
     call msg('o eps_bse','#',headers,INDENT=0,USE_TABS=.TRUE.)
     !
     headers(2:n_output)=(/'Im(pl_ph )','Re(pl_ph )','Im(pl_nos)','Re(pl_nos)','Re(pl_sat)','Im(pl_sat)'/)
     call msg('o pl_bse','#',headers,INDENT=0,USE_TABS=.TRUE.)
     call msg('o eps_bse pl_bse','#')
     !
     ! Write output on file
     !
     do iw=1,wv%n_freqs
       !
       ! Eps
       !
       output_data=(/real(wv%p(iw))*HA2EV,aimag(Epsilon_ii(iw,1)),real(Epsilon_ii(iw,1)), &
&             aimag(Epsilon_ii(iw,2)),real(Epsilon_ii(iw,2)),aimag(Epsilon_ii(iw,3)),real(Epsilon_ii(iw,3))/)
       call msg('o eps_bse','',output_data,INDENT=-2,USE_TABS=.true.)
       !
       ! Luminescence
       !
       output_data=(/real(wv%p(iw))*HA2EV,aimag(PL_ii(iw,1)),real(PL_ii(iw,1)),          &
&               aimag(PL_ii(iw,2)),real(PL_ii(iw,2)),aimag(PL_ii(iw,3)),real(PL_ii(iw,3))/)
       call msg('o pl_bse','',output_data,INDENT=-2,USE_TABS=.true.)
       !
     enddo
     !
     ! Close files
     !
     call of_open_close(eps_file_name)
     call of_open_close(pl_file_name)
     !
   end subroutine print_output
   !
end subroutine
