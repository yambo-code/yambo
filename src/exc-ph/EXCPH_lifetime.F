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
!> @brief Calculate exciton non-radiative life-time induced by phonon-scattering
!!
subroutine EXCPH_lifetime(E,k,q,X)
 !
 use units,          ONLY:HA2EV
 use pars,           ONLY:SP,cZERO,rZERO,schlen,pi
 use electrons,      ONLY:levels,n_sp_pol,spin_occ
 use R_lattice,      ONLY:bz_samp
 use parser_m,       ONLY:parser
 use com,            ONLY:msg,of_open_close
 use BS_Solvers,     ONLY:BSS_n_eig
 use X_m,            ONLY:X_t
 use IO_int,         ONLY:io_control
 use IO_m,           ONLY:manage_action,OP_RD,REP,RD_CL_IF_END,DUMP
 use EXCPH,          ONLY:EXCPH_sum,EXCPH_states,L_kind_in,L_kind_out,BS_E,BS_all_E,BS_E_in, &
&                         alloc_EXC,free_EXC         
 use ELPH,           ONLY:PH_freqs,ph_modes,use_PH_DbGd,elph_branches
 !
#include<memory.h>
 !
 type(levels)  ::E
 type(bz_samp) ::k,q
 type(X_t)     ::X
 !
 ! Work space
 !
 integer :: ID_exc_gkkp,ID_gkkp,io_err
 integer, external :: io_EXCPH_gkkp,io_ELPH
 integer :: iq_bz,i_alpha,i_beta,il
 !
 call section('*','EXCPH-LifTime')
 !
 ! Load excitonic energies 
 !
 call EXCPH_load_L(1,X,'check','Lout')
 !
 ! Allocations 
 !
 call alloc_EXC(q)
 !
 ! Load excitonic energies 
 !
 call EXCPH_load_L(1,X,'eigenvalues','Lin')
 BS_E_in(:)=BS_E(:) ! L_in can be different from L_out 
 !
 do iq_bz=1,q%nibz
   call EXCPH_load_L(iq_bz,X,'eigenvalues','Lout')
   BS_all_E(:,iq_bz)=real(BS_E(:)) ! I suppose the BSE eigenvalues are real
 enddo
 !
 ! Load the Exciton-phonon elements and phonon energies
 !
 call io_control(ACTION=OP_RD,COM=REP,SEC=(/1/),MODE=DUMP,ID=ID_exc_gkkp)
 io_err=io_EXCPH_gkkp(ID_exc_gkkp,'gkkp')
 if(io_err/=0) call error('excph_gkkp databases not found!')
 !
 call io_control(ACTION=OP_RD,COM=REP,SEC=(/1/),MODE=DUMP,ID=ID_gkkp)
 io_err=io_ELPH(ID_gkkp,'gkkp_expanded no_matrix_elements')
 if(io_err/=0) call error('gkkp_expanded databases not found!')
 !
 ! Calculate the life-times
 !
 do iq_bz=1,q%nbz
   !
   call io_control(ACTION=RD_CL_IF_END,COM=REP,SEC=(/1+iq_bz/),MODE=DUMP,ID=ID_exc_gkkp)
   io_err=io_EXCPH_gkkp(ID_exc_gkkp,'gkkp')
   !
   do i_alpha=EXCPH_states(1),EXCPH_states(2)
     !
     do il=elph_branches(1),elph_branches(2)
       !
       do i_beta=EXCPH_sum(1),EXCPH_sum(2)
         !
         !
       enddo
       !
     enddo
     !
   enddo
   !
 enddo
 !
 ! Deallocations
 !
 call free_EXC()
 !
end subroutine
