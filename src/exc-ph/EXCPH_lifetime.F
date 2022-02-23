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
 integer, external   :: io_Double_Grid
 integer :: io_err,iq,ID_DG
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
 do iq=1,q%nibz
   call EXCPH_load_L(iq,X,'eigenvalues','Lout')
   BS_all_E(:,iq)=real(BS_E(:)) ! I suppose the BSE eigenvalues are real
 enddo
 !
 ! Deallocations
 !
 call free_EXC()
 !
end subroutine
