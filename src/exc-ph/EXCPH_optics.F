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
 use electrons,      ONLY:levels,n_sp_pol,spin_occ
 use R_lattice,      ONLY:bz_samp,bare_qpg,d3k_factor
 use X_m,            ONLY:X_t
 use IO_int,         ONLY:io_control
 use IO_m,           ONLY:manage_action,OP_RD,REP,RD_CL_IF_END,OP_WR,WR,WR_CL,&
&                         OP_WR_CL,OP_APP_CL,DUMP
 use X_output,       ONLY:eps_file_name,pl_file_name,X_setup_file_names,X_write_messages_before_headers
 use parser_m,       ONLY:parser
 use com,            ONLY:msg,of_open_close
 use BS_Solvers,     ONLY:BSS_n_eig
 use EXCPH,          ONLY:EXCPH_sum,EXCPH_states,L_kind_in,L_kind_out,BS_E,BS_all_E,BS_E_in, &
&                         BS_R_left,BS_R_right,BS_R
 use ELPH,           ONLY:PH_freqs,ph_modes,use_PH_DbGd
 use frequency,      ONLY:w_samp
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
 integer, parameter  :: n_output=5
 real(SP)            :: output_data(n_output)
 integer             :: iw,iq,i_alpha,ID_DG,io_err
 integer, external   :: io_Double_Grid
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
 YAMBO_ALLOC(BS_E,(BSS_n_eig))
 YAMBO_ALLOC_P(BS_R_left,(BSS_n_eig))
 YAMBO_ALLOC_P(BS_R_right,(BSS_n_eig))
 YAMBO_ALLOC(BS_R,(BSS_n_eig))
 YAMBO_ALLOC(BS_E_in,(BSS_n_eig))
 YAMBO_ALLOC(BS_all_E,(BSS_n_eig,q%nibz))
 !
 ! Load excitonic energies and residuals
 !
 call EXCPH_load_L(1,X,'eigenvalues residuals','Lin')
 BS_E_in(:)=BS_E(:) ! L_in can be different from L_out 
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
   BS_R(i_alpha)=abs(BS_R_left(i_alpha)*BS_R_right(i_alpha))
 enddo
 !
 ! This formula can be wrong in presence of the 2D-cutoff
 BS_R=BS_R*real(spin_occ,SP)/(2._SP*pi)**3*d3k_factor*4._SP*pi/bare_qpg(1,1)**2
 !
 ! **** DOUBLE GRID PART *****
 !
 ! Fill the PH_freqs array
 ! This structure will contains the phonon frequencies and the
 ! double-grid phonon frequencies calculated with matdyn.x
 !
 PH_freqs%nk=q%nibz
 PH_freqs%nb=ph_modes
 YAMBO_ALLOC(PH_freqs%E,(q%nibz,ph_modes,1))
 !
 ! Check if double-grid is present and use it
 ! load the double-grid phonon energies in PH_freqs
 !
 call io_control(ACTION=OP_RD,COM=REP,MODE=DUMP,SEC=(/1,2/),ID=ID_DG)
 io_err=io_Double_Grid(PH_freqs,q,ID_DG,'phonons')
 !
 dbgd_string=''
 if(io_err==0) then
   !     
   use_PH_DbGd=.TRUE.
   call msg('sr','Using PH-double-grid ')
   dbgd_string='DbGd'
   !
 endif
 !
 ! Frequencies and damping setup
 !
 call FREQUENCIES_setup(wv)
 YAMBO_ALLOC(Epsilon_ii,(wv%n_freqs,2))
 YAMBO_ALLOC(PL_ii,(wv%n_freqs,2))
 !
 ! Calculate X(w) include phonon assisted transitions
 !
 call EXCPH_X_phass(wv,q,Epsilon_ii,PL_ii)
 !
 call X_setup_file_names(1,'bse ','phass',dbgd_string) 
 !
 call print_output()
 !
 ! ! Deallocations
 !
 YAMBO_FREE(Epsilon_ii)
 YAMBO_FREE(PL_ii)
 YAMBO_FREE(BS_E)
 YAMBO_FREE(BS_all_E)
 YAMBO_FREE_P(BS_R_left)
 YAMBO_FREE_P(BS_R_right)
 YAMBO_FREE(BS_R)
 call FREQUENCIES_reset(wv,"all")
 !
 contains
   !
   subroutine print_output()
     use X_output,      ONLY:X_write_messages_before_headers,X_write_q_plus_G
     implicit none
     !
     call of_open_close(eps_file_name,'ot')
     call of_open_close(pl_file_name,'ot')
     !
     call X_write_messages_before_headers(1,.FALSE.,.FALSE.,'r ')
     !
     call X_write_q_plus_G(1,(/0.0,0.0,0.0/),1)
     !
     call msg('o eps_q pl_q','# EXCITON PHONON')    
     call msg('o eps_q pl_q','# Exciton_states    :',EXCPH_states,INDENT=0)    
     call msg('o eps_q pl_q','# EXciton sum       :',EXCPH_sum,INDENT=0)    
     call msg('o eps_q pl_q','#')
     !
     ! Write output on file
     !
     do iw=1,wv%n_freqs
       !
       ! Eps
       !
       output_data=(/real(wv%p(iw))*HA2EV,aimag(Epsilon_ii(iw,1)),real(Epsilon_ii(iw,1)), &
&             aimag(Epsilon_ii(iw,2)),real(Epsilon_ii(iw,2))/)
       call msg('o eps_q','',output_data,INDENT=-2,USE_TABS=.true.)
       !
       ! Luminescence
       !
       output_data=(/real(wv%p(iw))*HA2EV,aimag(PL_ii(iw,1)),real(PL_ii(iw,1)),          &
&               aimag(PL_ii(iw,2)),real(PL_ii(iw,2))/)
       call msg('o pl_q','',output_data,INDENT=-2,USE_TABS=.true.)
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
