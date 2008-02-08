!
! Copyright (C) 2000-2005 A. Marini and the SELF team 
!         http://www.fisica.uniroma2.it/~self
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
module QP_m
 !
 use pars
 implicit none
 !
 integer, parameter :: max_qp_descs=100
 !
 integer           :: QP_dSc_steps
 integer           :: QP_Sc_steps
 integer           :: QP_n_W_freqs
 integer           :: QP_nb
 integer           :: QP_nk
 integer           :: QP_n_states
 integer           :: QP_ng_Sx
 integer           :: QP_n_G_bands(2)
 integer           :: QP_Sx_low_band
 integer           :: GWo_iterations
 logical           :: QP_dSc_test
 logical           :: QP_W_partially_done
 logical           :: GWo_SC_done
 logical           :: CHOSEX_use_empties
 character(schlen) :: QP_rep_string
 character(schlen) :: QP_solver
 character(schlen) :: QP_xc_FUNCTIONAL
 real(SP)          :: QP_G_damp
 real(SP)          :: QP_dSc_delta
 real(SP)          :: QP_W_er(2)
 real(SP)          :: QP_W_dr(2)
 real(SP)          :: QP_G_er(2)
 real(SP)          :: QP_G_dr(2)
 real(SP)          :: QP_cg_percent
 complex(SP),allocatable:: QP_dSc(:,:)
 complex(SP),allocatable:: QP_W(:,:,:)
 complex(SP),allocatable:: QP_Sx(:)
 complex(SP),allocatable:: QP_Sc(:,:)
 complex(SP),allocatable:: QP_G(:,:)
 complex(SP),allocatable:: QP_Vxc(:)
 integer,    allocatable:: QP_solver_state(:)
 integer,    allocatable:: QP_table(:,:)
 logical,    allocatable:: QP_state(:,:)
 !
 ! Mixing for non perturbative calculations
 !
 integer           :: QP_bands_mixed
 real(SP)          :: QP_band_mixing
 !
 type QP_t
   character(schlen)  :: description(max_qp_descs)
   integer,    pointer:: table(:,:)
   real(SP)   ,pointer:: k(:,:)
   real(SP)   ,pointer:: E_bare(:)
   complex(SP),pointer:: E(:)
   complex(SP),pointer:: Z(:)
   integer  :: n_descs
   integer  :: nk
   integer  :: nb
   integer  :: n_states
 end type QP_t
 !
 ! QP CTL
 !
 !?fnQP_db= "EWZ<db-pp.qp"     # [?] QP database.
 !% ?fnQP_E
 !  0 |  0 | 0 |               # [?] QP parameters (E).
 !% 
 !% ?fnQP_W
 !  0 |  0 | 0 | 0 |           # [?] QP parameters (W).
 !%
 !?fbQP_Z= (  0.00000 ,  0.00000 )  eV  # [?] QP parameters (Z).
 !  
 ! 1(X) 2(K) 3(G) 
 !
 character(schlen):: QP_ctl_db(3)
 integer    :: QP_ctl_interp_neigh(3)
 real(SP)   :: QP_ctl_E(3,3)
 real(SP)   :: QP_ctl_W(3,4)
 complex(SP):: QP_ctl_Z(3)
 logical    :: QP_ctl_applied
 character(schlen):: QP_ctl_short_descr(3)
 !
 contains
   !
   subroutine QP_reset(qp)
     type(QP_t)::qp
     qp%description=' '
     qp%n_descs=0
     qp%n_states=0
     nullify(qp%table,qp%k,qp%E,qp%Z,qp%E_bare)
   end subroutine
   !
end module 
