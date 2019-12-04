!
!        Copyright (C) 2000-2019 the YAMBO team
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
 use pars,           ONLY:SP,lchlen,schlen
 use units,          ONLY:HA2EV
 use matrix,         ONLY:PAR_matrix
 !
#include <dev_defs.h>
#include <memory.h>
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
 ! Double Grid
 !
 ! Logical used to decid how to use the double grid in X_os
 ! and in particular in X_GreenF_analytical, when the DbGd is used
 !
 integer              :: X_DbGd_nkpts
 logical              :: use_X_DbGd
 real(SP)             :: X_DbGd_percentual=0.5
 integer ,allocatable :: X_DbGd_nkpt_at_q(:)
 integer ,allocatable :: X_DbGd_nkpt_at_q_table(:,:,:)
#ifdef _CUDA
 !
 ! GPU X_irredux_residuals workspace
 !
 complex(SP), allocatable, device :: rhotw_save_d(:), rhotw_save2_d(:)
#endif
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
 integer :: X_ng = 0
 integer :: current_iq
 logical :: X_FILL_UP_matrix_only
 logical :: self_detect_E_range
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
 ! PARALLEL distribution
 !
 type(PAR_matrix), allocatable, target :: X_par(:)
 type(PAR_matrix), target      :: X_par_lower_triangle
 logical                       :: X_lower_triangle_matrix_in_use=.FALSE.
 integer                       :: X_rows(2) = 0
 integer                       :: X_cols(2) = 0
 complex(SP),      allocatable :: X_mat(:,:,:)
 complex(SP),      allocatable DEV_ATTRIBUTE :: X_mat_d(:,:,:)
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
     use electrons, ONLY:n_sp_pol,n_spinor
     use pars,      ONLY:cZERO
     !
     character(*)      :: what
     integer, optional :: d(:)
     !
     if (present(d)) then
       select case(what)
       case('EPS')
         if (allocated(Epsilon_ii)) return
         YAMBO_ALLOC(Epsilon_ii,(d(1),d(2))) !2(int)3(nonint)4(n-1 Haydock/inv shifted)
         YAMBO_ALLOC(X_drude_term,(d(1)))
         YAMBO_ALLOC(Joint_DOS,(d(1),d(2)))
         Epsilon_ii   = cZERO
         X_drude_term = cZERO
         Joint_DOS    = cZERO
       case('X')
         if (allocated(X_mat)) return
         YAMBO_ALLOC(X_mat,(d(1),d(2),d(3)))
         X_mat=cZERO
#ifdef _CUDA
         allocate(X_mat_d, source=X_mat)
#endif
       end select
     else
       select case(what)
       case('EPS')
         YAMBO_FREE(Epsilon_ii)
         YAMBO_FREE(X_drude_term)
         YAMBO_FREE(Joint_DOS)
       case('X')
         !
         ! I clean the RL components mapping
         !
         YAMBO_FREE(X_RLcomp_ig)
         !
         ! and the matrix itself
         !
         YAMBO_FREE(X_mat)
         YAMBO_FREE(X_mat_d)
         !
       end select
     endif
     !
   end subroutine
   !
   subroutine X_duplicate(Xi,Xo)
     type(X_t):: Xi,Xo
     Xo%ng           =Xi%ng
     Xo%ng_db        =Xi%ng_db
     Xo%iq           =Xi%iq
     Xo%ib           =Xi%ib
     Xo%ib_lim       =Xi%ib_lim
     Xo%db_ib        =Xi%db_ib
     Xo%db_ib_lim    =Xi%db_ib_lim
     Xo%ngostnts     =Xi%ngostnts
     Xo%ordering     =Xi%ordering
     Xo%Vnl_included =Xi%Vnl_included
     Xo%ehe          =Xi%ehe
     Xo%q0           =Xi%q0
     Xo%cg_percentual=Xi%cg_percentual
     Xo%ppaE         =Xi%ppaE
     Xo%Wd           =Xi%Wd
     Xo%f_xc=Xi%f_xc
   end subroutine
   !
   subroutine X_reset(X,type)
     use pars,  ONLY:czero
     integer, optional :: type
     type(X_t)::X
     X%ng           =1
     X%ng_db        =0
     !DEBUG>
     !X%ib           =(/1,-1/)
     !X%ib_lim       =(/1,-1/)
     !DEBUG<
     X%iq           =(/1,-1/)
     X%ngostnts     =0
     X%whoami       =0
     X%Vnl_included =.false.
     if (present(type)) X%whoami=type
     X%ehe          =(/-1._SP,-1._SP/)/HA2EV
     X%q0           =(/1._SP,0._SP,0._SP/)
     X%Wd           =cZERO
     X%cg_percentual=100._SP
     X%ppaE         =1._SP
     X%ordering     ="T"
     X%f_xc         ="none"       
   end subroutine
   !
end module
