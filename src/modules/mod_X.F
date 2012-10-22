!
! Copyright (C) 2000-2012 A. Marini and the YAMBO team 
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
module X_m
 !
 use pars,  ONLY:SP,lchlen,schlen
 use units, ONLY:HA2EV
 implicit none
 !
 ! Polarizability
 !
 real(SP)                 :: eps_2_alpha
 character(6)             :: alpha_dim ! 'adim' 'area' 'length' 'volume'
 logical                  :: eval_alpha
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
 ! Dipole M.E.
 !
 logical                  :: Dipole_bands_ordered
 logical                  :: Dipole_uses_shifted_grids
 complex(SP), allocatable :: DIP_q_dot_iR(:,:,:,:)
 complex(SP), allocatable :: DIP_iR(:,:,:,:,:)
 complex(SP), allocatable :: DIP_P(:,:,:,:,:)
 complex(SP), allocatable :: P_square(:,:,:,:)
 !
 ! Absorption & Polarizability
 !
 complex(SP), allocatable :: X_epsilon(:,:)
 complex(SP), allocatable :: X_drude_term(:)
 complex(SP), allocatable :: X_fxc(:)
 !
 ! Response function
 !
 real(SP),    allocatable :: X_poles(:)
 integer ,    allocatable :: X_poles_tab(:,:)
 integer ,    allocatable :: X_RLcomp_ig(:)
 complex(SP), allocatable :: X_mat(:,:,:)
 integer :: current_iq
 integer :: X_RIM_nkpts
 logical :: half_X_mat_only
 logical :: self_detect_E_range
 logical :: use_X_RIM
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
   integer    :: ngostnts
   real(SP)   :: ehe(2)
   real(SP)   :: q0(3)
   real(SP)   :: cg_percentual
   real(SP)   :: ppaE
   complex(SP):: Wd
   logical          :: Vnl_included
   character(1)     :: ordering
   character(schlen):: f_xc
 end type
 !
 interface
   logical function O_eels(wv,nw,be_verbose,ordering,eps,eel)
     use pars,          ONLY:SP
     integer              :: nw 
     complex(SP)          :: wv(nw)
     logical              :: be_verbose
     character(1)         :: ordering
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
         call mem_est(what,(/product(d)/),errors=(/err/))
       case('DIP_iR')
         if (allocated(DIP_iR)) return
         allocate(DIP_iR(d(1),d(2),d(3),d(4),n_sp_pol),stat=err)
         call mem_est(what,(/product(d)*n_sp_pol/),errors=(/err/))
       case('DIP_P')
         if (allocated(DIP_P)) return
         allocate(DIP_P(d(1),d(2),d(3),d(4),n_sp_pol),stat=err)
         call mem_est(what,(/product(d)*n_sp_pol/),errors=(/err/))
       case('DIP_q_dot_iR')
         if (allocated(DIP_q_dot_iR)) return
         allocate(DIP_q_dot_iR(d(1),d(2),d(3),n_sp_pol),stat=err)
         call mem_est(what,(/product(d)*n_sp_pol/),errors=(/err/))
       case('P_square')
         if (allocated(P_square)) return
         allocate(P_square(d(1),d(2),d(3),n_sp_pol),stat=err)
         call mem_est(what,(/product(d)*n_sp_pol/),errors=(/err/))
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
       case('DIP_P')
         if (.not.allocated(DIP_P)) return
         deallocate(DIP_iR)
       case('DIP_q_dot_iR')
         if (.not.allocated(DIP_q_dot_iR)) return
         deallocate(DIP_q_dot_iR)
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
     Xo%ib=Xi%ib
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
     integer, optional :: type
     type(X_t)::X
     X%ng=1
     X%ng_db=0 
     X%iq=(/1,-1/)
     X%whoami=0
     X%Vnl_included=.false.
     if (present(type)) X%whoami=type
     X%ehe=(/-1.,-1./)/HA2EV
     X%q0=(/1.,0.,0./)
     X%Wd=cmplx(0.,0.,SP)
     X%cg_percentual=100.
     X%ppaE=1.
     X%ordering="t"       
     X%f_xc="none"       
   end subroutine
   !
end module
