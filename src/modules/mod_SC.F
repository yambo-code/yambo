!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM MG
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
module SC
 !
 use descriptors,     ONLY:IO_desc
 use pars,            ONLY:SP,schlen,lchlen
 !
#include<memory.h>
 !
 integer           :: SC_bands(2)
 integer           :: SC_nbands
 integer           :: it_to_start
 integer           :: it_now
 integer           :: SC_iterations
 integer           :: SC_fft_size
 integer           :: SC_ng_Sx
 integer           :: SC_X_bands(2)
 integer           :: SC_X_size
 integer           :: SC_up_W_iters
 real(SP)          :: rho_convergence
 real(SP)          :: magn_convergence
 real(SP)          :: E_convergence
 real(SP)          :: SC_rho_threshold
 real(SP)          :: SC_cycle_mixing
 !
 character(schlen) :: SC_preconditioning="simple"
 character(schlen) :: SC_potential
 logical           :: compatible_SC_DB
 logical           :: found_SC_DB
 !
 ! General flags
 !
 logical           :: l_NSC_shot
 logical           :: l_SC_RESTART
 logical           :: l_SC_cycle
 logical           :: l_write_qp_histo
 logical           :: l_Mean_Potential
 logical           :: l_SC_update_W
 logical           :: l_SC_diagonal
 logical           :: l_SC_nl_mix
 logical           :: l_froze_density
 !
 ! Possible Kinds, functionals and external perturbations
 !
 integer           :: SC_kind
 integer           :: SC_xc_functional
 integer           :: SC_perturbation
 !
 ! OEP specific
 !
 integer,parameter    :: OEP_steps = 100 
 real(SP),parameter   :: OEP_threshold = 1.E-5
 character(schlen)    :: OEP_approx
 logical              :: l_oep_iterative,l_oep_EWeight
 !
 ! xc potential
 !
 complex(SP), allocatable :: V_mean(:,:)
 !
 ! Descriptor
 !
 type(IO_desc)        :: SC_desc
 !
 interface
   !
   subroutine load_SC_components(what,E,COM_,MODE_,ik,i_sp_pol,n_bands,kind,xc_functional,&
&                                perturbation)
     use electrons, ONLY:levels
     implicit none
     character(*)           :: what
     type(levels), optional :: E
     integer,      optional :: COM_
     integer,      optional :: MODE_
     integer,      optional :: ik,i_sp_pol
     integer,      optional :: n_bands
     integer,      optional :: kind
     integer,      optional :: xc_functional
     integer,      optional :: perturbation
   end subroutine  
 end interface  
 !
 contains
   !
   subroutine SC_alloc(E)
     use drivers,        ONLY:l_sc_srpa,l_sc_sex,l_sc_coh
     use electrons,      ONLY:n_sp_pol,levels
     use hamiltonian,    ONLY:H_alloc,H_QP_table_setup
     use QP_m,           ONLY:QP_n_states,QP_Sc,QP_nk
     !
     implicit none
     !
     type(levels) :: E
     !
     call H_alloc(E,.true.,.true.)
     !
     call H_QP_table_setup(E)
     !
     SC_nbands=SC_bands(2)-SC_bands(1)+1
     !
     if (l_sc_coh.or.l_sc_sex.or.l_sc_srpa) then
       YAMBO_ALLOC(QP_Sc,(QP_n_states,1))
     endif
     !
   end subroutine
   !
   subroutine SC_free()
     use QP_m,           ONLY:QP_Sc
     use hamiltonian,    ONLY:H_free
     use xc_functionals, ONLY:magn,V_xc
     use DIPOLES,        ONLY:DIP_alloc
     !
     implicit none
     !
     call H_free()
     !
     call DIP_alloc('DIP_P')
     call DIP_alloc('DIP_iR')
     call DIP_alloc('DIP_spin')
     call DIP_alloc('P_square')
     !
     YAMBO_FREE(QP_Sc)
     !
   end subroutine
   !
end module
