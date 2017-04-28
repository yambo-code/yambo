!
!        Copyright (C) 2000-2017 the YAMBO team
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
module X_m
 !
 use pars,  ONLY:SP,lchlen,schlen
 use units, ONLY:HA2EV
 !
 implicit none
 !
 ! Polarizability
 !
 real(SP)                 :: eps_2_alpha
 character(6)             :: alpha_dim ! 'adim' 'area' 'length' 'volume'
 logical                  :: eval_alpha
 logical                  :: l_drude
 !
 ! Main Gauge
 !
 character(schlen)        :: global_gauge
 !
 ! Velocity gauge specific variables
 !
 logical                  :: skip_cond_sum_rule
 !
 ! Large Q-vector support 
 !
 real(SP)                 :: q_plus_G_direction(3)
 integer                  :: Q_Shift_Order
 !
 ! Warning about non-local commutator
 !
 logical                  :: Vnl_commutator_warning
 !
 ! Logical used to decid how to use the double grid in X_os
 ! and in particular in X_GreenF_analytical, when the DbGd is used
 !
 logical                  :: X_is_evaluated_for_all_Qs
 !
 ! Dipole M.E.
 !
 character(schlen)        :: Dipole_approach
 logical                  :: use_covariant_approach
 logical                  :: use_shifted_grids_approach
 logical                  :: use_g_space_approach
 logical                  :: use_real_space_approach
 logical                  :: force_v_g_space_approach
 integer                  :: num_shifted_grids
 real(SP)                 :: Dipole_Energy_treshold
 real(SP)                 :: Dipole_Energy_treshold_vv
 complex(SP), allocatable :: DIP_projected(:,:,:,:)
 complex(SP), allocatable :: DIP_iR(:,:,:,:,:)
 complex(SP), allocatable :: DIP_P(:,:,:,:,:)
 complex(SP), allocatable :: DIP_S(:,:,:,:,:)
 complex(SP), allocatable :: DIP_spin(:,:,:,:,:)
 complex(SP), allocatable :: P_square(:,:,:,:)
 !
 ! Absorption & Polarizability
 !
 complex(SP), allocatable :: Epsilon_ii(:,:)
 complex(SP), allocatable :: Epsilon_ij(:,:)
 complex(SP), allocatable :: Joint_DOS(:,:) 
 complex(SP), allocatable :: EEL(:,:)
 complex(SP), allocatable :: Alpha(:,:)
 complex(SP), allocatable :: X_drude_term(:)
 complex(SP), allocatable :: X_fxc(:)
 !
 ! Response function
 !
 character(schlen)        :: Chi_mode
 character(schlen)        :: Chi_linalg_mode
 logical                  :: X_use_lin_sys=.false.
 real(SP),    allocatable :: X_poles(:)
 real(SP),    allocatable :: X_Ein_poles(:)
 integer ,    allocatable :: X_poles_tab(:,:)
 integer ,    allocatable :: X_RLcomp_ig(:)
 complex(SP), allocatable :: X_mat(:,:,:)
 integer :: current_iq
 integer :: iw_ref
 integer :: X_DbGd_nkpts
 logical :: half_X_mat_only
 logical :: self_detect_E_range
 logical :: use_X_DbGd
 real(SP):: X_terminator_E
 real(SP):: X_term_E
 logical :: l_X_terminator=.false.
 character(schlen) :: X_terminator_Kind="none"
 !
 ! I/O parameters
 !
 integer,parameter:: n_MAX_blocks_of_rows=1000 ! Note that in NETCDF the max number of vars is 8192
 integer          :: n_blocks_of_rows
 integer          :: n_rows_each_block
 !
 ! I/O descriptions
 !
 integer          :: n_X_descs
 character(lchlen):: X_description(100)
 !
 type X_t
   integer    :: whoami ! 1:Xo 2:em1s 3:em1d 4:pp
   integer    :: ng
   integer    :: ng_db
   integer    :: iq(2)
   integer    :: ib(2)
   integer    :: ib_lim(2)
   integer    :: db_ib(2)
   integer    :: db_ib_lim(2)
   integer    :: ngostnts
   real(SP)   :: ehe(2)
   real(SP)   :: q0(3)
   real(SP)   :: cg_percentual
   real(SP)   :: ppaE
   complex(SP):: Wd
   logical          :: Vnl_included
   character(2)     :: ordering
   character(schlen):: f_xc
 end type
 !
 interface
   logical function OPTICS_eels(wv,nw,be_verbose,ordering,eps,eel)
     use pars,          ONLY:SP
     integer              :: nw 
     complex(SP)          :: wv(nw)
     logical              :: be_verbose
     character(2)         :: ordering
     complex(SP) ,optional:: eps(nw),eel(nw)
   end function
 end interface
 !
 contains
   !
   subroutine X_alloc(what,d)
     !
     use electrons, ONLY:n_sp_pol
     use memory_m,  ONLY:mem_est
     use pars,      ONLY:cZERO
     !
     character(*)      :: what
     integer, optional :: d(:)
     !
     ! Work Space
     !
     integer :: err
     !
     if (present(d)) then
       select case(what)
       case('X')
         if (allocated(X_mat)) return
         allocate(X_mat(d(1),d(2),d(3)),stat=err)
         call mem_est(what,(/size(X_mat)/),errors=(/err/))
         X_mat=cZERO
       case('DIP_iR')
         if (allocated(DIP_iR)) return
         allocate(DIP_iR(d(1),d(2):d(3),d(4):d(5),d(6),n_sp_pol),stat=err)
         call mem_est(what,(/size(dip_iR)/),errors=(/err/))
         DIP_iR=cZERO
       case('DIP_P')
         if (allocated(DIP_P)) return
         allocate(DIP_P(d(1),d(2):d(3),d(4):d(5),d(6),n_sp_pol),stat=err)
         call mem_est(what,(/size(dip_P)/),errors=(/err/))
         DIP_P=cZERO
       case('DIP_S')
         if (allocated(DIP_S)) return
         allocate(DIP_S(d(1),d(2),d(3),d(4),n_sp_pol),stat=err)
         call mem_est(what,(/size(dip_S)/),errors=(/err/))
         DIP_S=cZERO
       case('DIP_spin')
         if (allocated(DIP_spin)) return
         allocate(DIP_spin(d(1),d(2):d(3),d(4):d(5),d(6),n_sp_pol),stat=err)
         call mem_est(what,(/size(dip_P)/),errors=(/err/))
         DIP_spin=cZERO
       case('DIP_projected')
         if (allocated(DIP_projected)) return
         allocate(DIP_projected(d(1):d(2),d(3):d(4),d(5),n_sp_pol),stat=err)
         call mem_est(what,(/size(DIP_projected)/),errors=(/err/))
         DIP_projected=cZERO
       case('P_square')
         if (allocated(P_square)) return
         allocate(P_square(d(1):d(2),d(3):d(4),d(5),n_sp_pol),stat=err)
         call mem_est(what,(/size(P_square)/),errors=(/err/))
         P_square=cZERO
       end select
     else
       select case(what)
       case('X')
         !
         ! I clean the RL components mapping
         !
         if (allocated(X_RLcomp_ig)) deallocate(X_RLcomp_ig)
         !
         ! and the matrix itself
         !
         if (.not.allocated(X_mat)) return
         deallocate(X_mat)
         !
       case('DIP_iR')
         if (.not.allocated(DIP_iR)) return
         deallocate(DIP_iR)
       case('DIP_P')
         if (.not.allocated(DIP_P)) return
         deallocate(DIP_P)
       case('DIP_S')
         if (.not.allocated(DIP_S)) return
         deallocate(DIP_S)
       case('DIP_spin')
         if (.not.allocated(DIP_spin)) return
         deallocate(DIP_spin)
       case('DIP_projected')
         if (.not.allocated(DIP_projected)) return
         deallocate(DIP_projected)
       case('P_square')
         if (.not.allocated(P_square)) return
         deallocate(P_square)
       end select
       call mem_est(what)
     endif
   end subroutine
   !
   subroutine X_duplicate(Xi,Xo)
     type(X_t):: Xi,Xo
     Xo%ng=Xi%ng
     Xo%ng_db=Xi%ng_db
     Xo%iq=Xi%iq
     Xo%ib    =Xi%ib
     Xo%ib_lim=Xi%ib_lim
     Xo%db_ib    =Xi%db_ib
     Xo%db_ib_lim=Xi%db_ib_lim
     Xo%ngostnts=Xi%ngostnts
     Xo%ordering=Xi%ordering
     Xo%Vnl_included=Xi%Vnl_included
     Xo%ehe=Xi%ehe
     Xo%q0=Xi%q0
     Xo%cg_percentual=Xi%cg_percentual
     Xo%ppaE=Xi%ppaE
     Xo%Wd=Xi%Wd
     Xo%f_xc=Xi%f_xc
   end subroutine
   !
   subroutine X_reset(X,type)
     use pars,  ONLY:czero
     integer, optional :: type
     type(X_t)::X
     X%ng=1
     X%ng_db=0
     X%iq=(/1,-1/)
     X%ngostnts=0
     X%whoami=0
     X%Vnl_included=.false.
     if (present(type)) X%whoami=type
     X%ehe=(/-1._SP,-1._SP/)/HA2EV
     X%q0=(/1._SP,0._SP,0._SP/)
     X%Wd=cZERO
     X%cg_percentual=100._SP
     X%ppaE=1._SP
     X%ordering="T"
     X%f_xc="none"       
   end subroutine
   !
end module
