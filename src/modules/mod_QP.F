!
! Copyright (C) 2000-2010 A. Marini and the YAMBO team 
!              http://www.yambo-code.org
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
 use pars,      ONLY:SP,schlen
 implicit none
 !
 integer, parameter:: max_qp_descs=100
 !
 ! Mixing for non perturbative calculations
 !
 integer           :: SC_bands_mixed
 real(SP)          :: SC_band_mixing
 real(SP)          :: SC_E_threshold
 !
 integer           :: QP_dSc_steps
 integer           :: QP_Sc_steps
 integer           :: QP_n_W_freqs
 integer           :: QP_nb
 integer           :: QP_nk
 integer           :: QP_n_states
 integer           :: QP_ng_Sx
 integer           :: QP_n_G_bands(2)
 integer           :: GWo_iterations
 logical           :: QP_dSc_test
 logical           :: QP_W_partially_done
 logical           :: GWo_SC_done
 logical           :: CHOSEX_use_empties
 logical           :: On_Mass_Shell_approx
 logical           :: Force_Newton_QP
 logical           :: use_GreenF_to_eval_QP
 logical           :: GF_is_causal
 character(schlen) :: QP_solver
 real(SP)          :: QP_G_damp
 real(SP)          :: QP_dSc_delta
 real(SP)          :: QP_W_er(2)
 real(SP)          :: QP_W_dr(2)
 real(SP)          :: QP_G_er(2)
 real(SP)          :: QP_G_dr(2)
 real(SP)          :: QP_cg_percent
 real(SP)   ,allocatable:: QP_G_amplitude_integral(:)
 complex(SP),allocatable:: QP_dSc(:,:)
 complex(SP),allocatable:: QP_W(:,:,:)
 complex(SP),allocatable:: QP_Vnl_xc(:)
 complex(SP),allocatable:: QP_Sc(:,:)
 complex(SP),allocatable:: QP_Vxc(:)
 integer,    allocatable:: QP_solver_state(:)
 integer,    allocatable:: QP_table(:,:)
 logical,    allocatable:: QP_state(:,:)
 !
 ! Logicals used for reporting
 !
 logical           :: report_Vnlxc
 logical           :: report_Sc
 logical           :: report_dSc
 logical           :: l_extended_output
 character(12)     :: Vnlxc_kind
 character(10)     :: Vxc_kind
 !
 type QP_t
   !
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
   !
   ! Green Function and Self-Energy
   !
   integer            :: GreenF_n_steps
   complex(SP),pointer:: S_total(:,:)
   complex(SP),pointer:: GreenF(:,:)
   complex(SP),pointer:: GreenF_W(:,:)
   !
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
 character(schlen)       :: QP_ctl_db(3)
 integer                 :: QP_ctl_interp_neigh(3)
 !
 ! Note that QP_ctl_E/W/Z are dimensioned *,2)
 ! to describe spin polarized systems.
 ! The second element of the last argument 
 ! is not used in non-collinear or scalar cases.
 !
 real(SP)                :: QP_ctl_E(3,3,2)
 real(SP)                :: QP_ctl_W(3,4,2)
 complex(SP)             :: QP_ctl_Z(3,2)
 logical                 :: QP_ctl_applied
 !
 contains
   !
   subroutine QP_reset(qp)
     type(QP_t)::qp
     qp%description=' '
     qp%n_descs=0
     qp%n_states=0
     nullify(qp%table,qp%k,qp%E,qp%Z,qp%E_bare,qp%S_total,qp%GreenF,qp%GreenF_W)
   end subroutine
   !
   subroutine QP_states_simmetrize(en,vec_2_sim,state_is_2do)
     !
     use pars,          ONLY:SP
     use units,         ONLY:HARTREE
     use vec_operate,   ONLY:sort,degeneration_finder
     use electrons,     ONLY:levels
     !
     real(SP), optional  ::vec_2_sim(QP_n_states)
     logical,  optional  ::state_is_2do(QP_n_states)
     type(levels) ::en
     !
     ! Work Space
     !
     real(SP) ::Eo_sorted(QP_n_states),sym_value
     integer  ::Eo_sorted_index(QP_n_states),i1,inx,i2,&
&               first_el(QP_n_states),n_of_el(QP_n_states),n_deg_grp
     !
     ! Degenerate bands average.
     ! The PW gkkp m.e. are not symmetrized and, consequently,
     ! degnerate bands can be splitted.
     !
     forall(i1=1:QP_n_states) Eo_sorted(i1)=en%E(QP_table(i1,1),QP_table(i1,3),1)
     !
     call sort(Eo_sorted,indx=Eo_sorted_index)
     call degeneration_finder(Eo_sorted,QP_n_states,first_el,n_of_el,n_deg_grp,0.0001/HARTREE)
     !
     if (present(state_is_2do)) then
       state_is_2do=.TRUE.
     endif
     !
     do i1=1,n_deg_grp
       !
       sym_value=0.
       !
       do i2=first_el(i1),first_el(i1)+n_of_el(i1)-1
         !
         inx   =Eo_sorted_index(i2)
         !
         if (present(state_is_2do)) then
           if (i2>first_el(i1)) state_is_2do(inx)=.FALSE.
         endif
         !
         if (present(vec_2_sim)) then
           sym_value=sym_value+vec_2_sim(inx)/real( n_of_el(i1) )
         endif
         !
       enddo
       !
       do i2=first_el(i1),first_el(i1)+n_of_el(i1)-1
         !
         inx   =Eo_sorted_index(i2)
         !
         if (present(vec_2_sim)) then
           vec_2_sim(inx)=sym_value
         endif
         !
       enddo
       !
     enddo
     !
   end subroutine
   !
end module 
