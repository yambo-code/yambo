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
 module BS
!=========
  use pars
  implicit none
!
!  BS Kernel building
!
  integer :: BS_n_g_exch
  integer :: BS_K_dim
  integer :: BS_bands(2)
  integer :: BS_columns
  integer :: BS_n_g_W
  integer :: BS_q(2)
  integer :: BS_identifier
  integer :: BS_blk_coord(2)
  real(SP):: BS_eh_en(2)
  real(SP):: BS_eh_win(2)
  logical :: BS_W_is_diagonal
  logical :: BS_K_coupling
  logical :: BS_res_K_exchange
  logical :: BS_res_K_corr
  logical :: BS_cpl_K_exchange
  logical :: BS_cpl_K_corr
  logical :: BS_K_is_ALDA
  logical :: BS_DB_is_fragmented
  character(schlen) :: BS_res_mode
  character(schlen) :: BS_cpl_mode
  integer    , allocatable :: BS_eh_table(:,:)
  integer    , allocatable :: BS_blk_dim(:)
  complex(SP), allocatable :: BS_mat(:,:)
  complex(SP), allocatable :: BS_cpl_mat(:,:)
  real(SP)   , allocatable :: BS_eh_E(:)
!
! Oscillators
!
  integer  :: O_n_cc_scatt
  integer  :: O_n_vv_scatt
  integer  :: O_ng
  integer,    allocatable :: O_n_c_states(:)
  integer,    allocatable :: O_n_v_states(:)
  integer,    allocatable :: O_c_state(:,:)
  integer,    allocatable :: O_v_state(:,:)
  integer,    allocatable :: O_phase(:,:)
  integer,    allocatable :: O_table(:,:,:,:)
  complex(SP),allocatable :: O_cc(:,:)
  complex(SP),allocatable :: O_vv(:,:)
!
! Solvers
!
  character(lchlen):: BSS_description(100)
  character(lchlen):: BSS_file_name
  character(schlen):: BSS_mode
  character(schlen):: BSS_of_title(7)
  integer  :: BSS_n_descs
  integer  :: BSS_n_freqs
  real(SP) :: BSS_er(2)
  real(SP) :: BSS_dr(2)
  real(SP) :: BSS_q0(3)
  real(SP) :: Haydock_treshold
  real(SP) :: Haydock_v0_mod
  complex(SP),allocatable :: BSS_rhoq0(:)

  interface 
    subroutine Kernel_by_V(iq,iter,Vi,Vo)
!========================================
    use pars,          ONLY:SP
    integer               ::iq
    integer,     optional ::iter
    complex(SP), optional ::Vi(:),Vo(:)
    end subroutine
  end interface

 contains

 integer function cumulative_index(ik1,ik2)
!==========================================
 implicit none
 integer :: ik1,ik2
 cumulative_index=(ik2**2+ik2)/2-ik1+2
 end function

 function BS_blocks(I)
!=====================
 use R_lattice, ONLY:nXkibz
 implicit none
 integer :: BS_blocks(2)
 integer :: I,ik1,ik2
 BS_blocks=0
 do ik1=1,nXkibz
   do ik2=1,nXkibz
     if (cumulative_index(ik1,ik2)==I) then
       BS_blocks=(/ik1,ik2/)
       return
     endif
   enddo
 enddo

 end function

 end module BS
