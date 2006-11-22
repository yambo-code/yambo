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
 module X_m
  use pars, ONLY:SP,HARTREE,lchlen
!
! Absorption & Polarizability
!
  real(SP)                 :: eps_2_alpha
  complex(SP), allocatable :: X_epsilon(:,:)
  complex(SP), allocatable :: X_fxc(:)
  character(6)             :: alpha_dim ! 'adim' 'area' 'length' 'volume'
!
! Response function
!
  real(SP),    allocatable :: X_poles(:)
  integer ,    allocatable :: X_poles_tab(:,:)
  complex(SP), allocatable :: X_rhoq0(:,:,:,:)
  complex(SP), allocatable :: X_q0grad(:,:,:,:,:)
  complex(SP), allocatable :: X_mat(:,:,:)
  integer :: iq_now
  logical :: half_X_mat_only
  logical :: self_detect_E_range
  logical :: use_X_RIM
  character(lchlen) :: long_path
  type X_t
   integer  :: whoami ! 1:Xo 2:em1s 3:em1d 4:pp
   integer  :: ng
   integer  :: ng_db
   integer  :: iq(2)
   integer  :: ib(2)
   integer  :: ngostnts
   real(SP) :: ehe(2)
   real(SP) :: q0(3)
   real(SP) :: cg_percentual
   real(SP) :: ppaE
   logical  :: Vnl_included
   character(1) :: ordering
   complex(SP)  :: Wd
  end type

  contains

 subroutine X_alloc(what,d)
!==========================
 use electrons, ONLY:n_spin
 use memory_m,  ONLY:mem_est
 implicit none
 type(X_t):: X
 character(*)      :: what
 integer, optional :: d(:)
!ws
 integer :: err

 if (present(d)) then
   select case(what)
   case('X')
     if (allocated(X_mat)) return
     allocate(X_mat(d(1),d(2),d(3)),stat=err)
     call mem_est(what,(/product(d)/),errors=(/err/))
   case('OptGrad')
     if (allocated(X_q0grad)) return
     allocate(X_q0grad(d(1),d(2),d(3),d(4),n_spin),stat=err)
     call mem_est(what,(/product(d)*n_spin/),errors=(/err/))
   case('OptOsc')
     if (allocated(X_rhoq0)) return
     allocate(X_rhoq0(d(1),d(2),d(3),n_spin),stat=err)
     call mem_est(what,(/product(d)*n_spin/),errors=(/err/))
   end select
 else
   select case(what)
   case('X')
     if (.not.allocated(X_mat)) return
     deallocate(X_mat)
   case('OptGrad')
     if (.not.allocated(X_q0grad)) return
     deallocate(X_q0grad)
   case('OptOsc')
     if (.not.allocated(X_rhoq0)) return
     deallocate(X_rhoq0)
   end select
   call mem_est(what)
 endif
 end subroutine

 subroutine X_duplicate(Xi,Xo)
!=============================
 implicit none
 type(X_t):: Xi,Xo
 Xo%ng=Xi%ng
 Xo%ng_db=Xi%ng_db
 Xo%iq=Xi%iq
 Xo%ib=Xi%ib
 Xo%ngostnts=Xi%ngostnts
 Xo%ordering=Xi%ordering
 Xo%Vnl_included=Xi%Vnl_included
 Xo%ehe=Xi%ehe
 Xo%q0=Xi%q0
 Xo%cg_percentual=Xi%cg_percentual
 Xo%ppaE=Xi%ppaE
 Xo%Wd=Xi%Wd
 end subroutine

 subroutine X_reset(X,type)
!==========================
 implicit none
 integer, optional :: type
 type(X_t)::X
 X%ng=1
 X%ng_db=0 
 X%iq=(/1,1/)
 X%whoami=0
 X%Vnl_included=.false.
 if (present(type)) X%whoami=type
 X%ehe=(/-1.,-1./)/HARTREE
 X%q0=(/1.,0.,0./)
 X%Wd=cmplx(0.,0.,SP)
 X%cg_percentual=100.
 X%ppaE=1.
 X%ordering="t"       
 end subroutine

 end module

