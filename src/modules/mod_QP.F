!
!        Copyright (C) 2000-2016 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM
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
 integer           :: QP_n_W_freqs_redux
 integer           :: QP_nb
 integer           :: QP_nk
 integer           :: QP_n_states
 integer           :: QP_ng_Sc
 integer           :: QP_ng_Sx
 integer           :: QP_ng_SH
 integer           :: QP_n_G_bands(2)
 integer           :: GWo_iterations
 logical           :: QP_dSc_test
 logical           :: GWo_SC_done
 logical           :: COHSEX_use_empties
 logical           :: l_GW_terminator=.FALSE.
 logical           :: On_Mass_Shell_approx
 logical           :: l_QP_Expand
 logical           :: use_GreenF_to_eval_QP
 logical           :: use_GreenF_Zoom
 logical           :: GF_is_causal
 character(schlen) :: QP_solver
 real(SP)          :: QP_G_Zoom_treshold
 real(SP)          :: QP_time_order_sign=-1 ! T-ordered. Set to +1 (causal) in QP_SET_temperature_pre_factor
 real(SP)          :: QP_G_damp
 real(SP)          :: QP_dSc_delta
 real(SP)          :: QP_G_er(2)
 real(SP)          :: QP_G_dr(2)
 real(SP)          :: QP_cg_percent
 character(schlen) :: GW_terminator_Kind="none"
 real(SP)          :: GW_terminator_E
 real(SP)   ,allocatable:: QP_G_amplitude_integral(:)
 real(SP)   ,allocatable:: QP_G_info(:,:,:)
 real(SP)   ,allocatable:: QP_G_zoom_er(:,:)
 real(SP)   ,allocatable:: QP_W_er(:,:)
 real(SP)   ,allocatable:: QP_W_dr(:,:)
 complex(SP),allocatable:: QP_dSc(:,:)
 complex(SP),allocatable:: QP_W(:,:,:,:)
 complex(SP),allocatable:: QP_Vnl_xc(:)
 complex(SP),allocatable:: QP_Sc(:,:)
 complex(SP),allocatable:: QP_Vxc(:)
 integer,    allocatable:: QP_solver_state(:)
 integer,    allocatable:: QP_table(:,:)
 logical,    allocatable:: QP_state(:,:)
 ! 
 ! QED
 !
 integer           :: QP_QED_ng = 1
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
   integer,    pointer:: table(:,:) => null()
   real(SP)   ,pointer:: k(:,:)     => null()
   real(SP)   ,pointer:: wk(:)      => null()
   real(SP)   ,pointer:: E_bare(:)  => null()
   complex(SP),pointer:: E(:)       => null()
   complex(SP),pointer:: Z(:)       => null()
   integer  :: n_descs
   integer  :: nk
   integer  :: nb
   integer  :: n_states
   !
   ! Green Function and Self-Energy
   !
   integer            :: GreenF_n_steps
   complex(SP),pointer:: S_total(:,:)   => null()
   complex(SP),pointer:: GreenF(:,:)    => null()
   complex(SP),pointer:: GreenF_W(:,:)  => null()
   !
 end type QP_t
 !
 ! QP CTL
 !
 !?fnQP_db= "EWZ<db-pp.qp"     # [?] QP database.
 !% ?fnQP_E
 !  0 |  0 | 0 |               # [?] QP parameters (E).
 !% 
 !% ?fnQP_Wc
 !  0 |  0 | 0 |               # [?] QP parameters (W) conduction.
 !%
 !% ?fnQP_Wv
 !  0 |  0 | 0 |               # [?] QP parameters (W) valence.
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
 real(SP)                :: QP_ctl_Wc_E_ref(3,2)
 real(SP)                :: QP_ctl_Wv_E_ref(3,2)
 real(SP)                :: QP_ctl_Wc(3,3,2)
 real(SP)                :: QP_ctl_Wv(3,3,2)
 complex(SP)             :: QP_ctl_Z(3,2)
 logical                 :: QP_ctl_applied
 !
 ! interface
 !
 interface
   subroutine XCo_Hartree_Fock(E,k,Xk,q,mode)
      use pars,          ONLY:SP,schlen,DP,pi
      use electrons,     ONLY:levels,spin_occ,spin
      use R_lattice,     ONLY:qindx_S,bz_samp
     !
     implicit none
     type(levels) ::E       
     type(bz_samp)::k,Xk,q
     character(*),optional ::mode
   end subroutine XCo_Hartree_Fock
   !
 end interface
 !
 contains
   !
   ! Time-Ordering
   !----------------
   !
   subroutine QP_SET_temperature_pre_factor( )
     use drivers,  ONLY:Finite_Tel
     QP_time_order_sign=-1                ! T-order
     if (Finite_Tel) QP_time_order_sign=1 ! Retared
   end subroutine
   !
   subroutine QP_reset(qp)
     type(QP_t)::qp
     qp%description=' '
     qp%n_descs=0
     qp%n_states=0
     if(associated(qp%table))      deallocate(qp%table)
     if(associated(qp%k))          deallocate(qp%k)
     if(associated(qp%wk))         deallocate(qp%wk)
     if(associated(qp%E))          deallocate(qp%E)
     if(associated(qp%Z))          deallocate(qp%Z)
     if(associated(qp%E_bare))     deallocate(qp%E_bare)
     if(associated(qp%S_total))    deallocate(qp%S_total)
     if(associated(qp%GreenF))     deallocate(qp%GreenF)
     if(associated(qp%GreenF_W))   deallocate(qp%GreenF_W)
     nullify(qp%table,qp%k,qp%wk,qp%E,qp%Z,qp%E_bare,qp%S_total,qp%GreenF,qp%GreenF_W)
   end subroutine
   !
   subroutine QP_states_simmetrize(en,V_real,V_complex,state_is_2do)
     !
     use pars,          ONLY:SP
     use units,         ONLY:HA2EV
     use vec_operate,   ONLY:sort,degeneration_finder
     use electrons,     ONLY:levels
     !
     real(SP),   optional ::V_real(QP_n_states)
     complex(SP),optional ::V_complex(QP_n_states)
     logical,    optional ::state_is_2do(QP_n_states)
     type(levels) ::en
     !
     ! Work Space
     !
     real(SP)    ::Eo_sorted(QP_n_states),r_sum
     complex(SP) ::c_sum
     integer     ::Eo_sorted_index(QP_n_states),i1,inx,i2,&
&                  first_el(QP_n_states),n_of_el(QP_n_states),n_deg_grp
     !
     ! Degenerate bands average.
     ! The PW gkkp m.e. are not symmetrized and, consequently,
     ! degnerate bands can be splitted.
     !
     forall(i1=1:QP_n_states) Eo_sorted(i1)=en%E(QP_table(i1,1),QP_table(i1,3),1)
     !
     call sort(Eo_sorted,indx=Eo_sorted_index)
     call degeneration_finder(Eo_sorted,QP_n_states,first_el,n_of_el,n_deg_grp,0.0001_SP/HA2EV)
     !
     if (present(state_is_2do)) then
       state_is_2do=.TRUE.
     endif
     !
     do i1=1,n_deg_grp
       !
       r_sum=0._SP
       c_sum=(0._SP,0._SP)
       !
       do i2=first_el(i1),first_el(i1)+n_of_el(i1)-1
         !
         inx   =Eo_sorted_index(i2)
         !
         if (present(state_is_2do)) then
           if (i2>first_el(i1)) state_is_2do(inx)=.FALSE.
         endif
         !
         if (present(V_real)) then
           r_sum=r_sum+V_real(inx)/real( n_of_el(i1) )
         endif
         if (present(V_complex)) then
           c_sum=c_sum+V_complex(inx)/real( n_of_el(i1) )
         endif
         !
       enddo
       !
       do i2=first_el(i1),first_el(i1)+n_of_el(i1)-1
         !
         inx   =Eo_sorted_index(i2)
         !
         if (present(V_real)) then
           V_real(inx)=r_sum
         endif
         if (present(V_complex)) then
           V_complex(inx)=c_sum
         endif
         !
       enddo
       !
     enddo
     !
   end subroutine
   !
   ! Lifetime functions e2et/h2ht
   !------------------------------
   !
   ! Gamma_n = 2 i \sum_m  { -/+ i Im[e^-1(e_n -e_m) (spin_occ-f+bose_f) <- e2et
   !                           + i Im[e^-1(e_m -e_n) (         f+bose_f) <- h2ht }
   !
   ! where - for T-ordered theory, + for causal (finite Tel)
   !
   function e2et(is,os,E,F)
     !
     use functions,   ONLY:bose_f
     use electrons,   ONLY:levels
     use drivers,     ONLY:Finite_Tel
     use electrons,   ONLY:spin_occ
     integer      ::is(3),os(3),e2et
     type(levels) ::E
     real(SP) :: F
     real(SP) :: dE !ws
     e2et=0
     !
     ! "Electron 2 Electron" decay
     !
     dE=E%E(is(1),is(2),is(3))-E%E(os(1),os(2),os(3))
     !
     F=QP_time_order_sign*(spin_occ-E%f(os(1),os(2),os(3))+bose_f(dE))
     !
     if (dE>0..and.abs(F)>epsilon(1.)) e2et=1
     if (e2et==0) F=0.
   end function
   !
   function h2ht(is,os,E,F)
     !
     use functions, ONLY:bose_f
     use electrons, ONLY:levels
     integer      ::is(3),os(3),h2ht
     type(levels) ::E
     real(SP)     ::F
     !
     ! Work Space
     !
     real(SP) :: dE 
     h2ht=0
     !
     !"Hole 2 Hole" decay
     !
     dE=E%E(os(1),os(2),os(3))-E%E(is(1),is(2),is(3))
     !
     F=E%f(os(1),os(2),os(3))+bose_f(dE)
     !
     if (dE>0..and.abs(F)>epsilon(1.)) h2ht=1
     if (h2ht==0) F=0.
   end function

   !
end module 
