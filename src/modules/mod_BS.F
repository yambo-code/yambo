!
! Copyright (C) 2000-2013 A. Marini and the YAMBO team 
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
module BS
 !
 use pars, ONLY:SP,schlen,lchlen
 implicit none
 !
 ! BS Kernel building
 !
 integer :: BS_n_g_exch
 integer :: BS_K_dim
 integer :: BS_bands(2)
 integer :: BS_columns
 integer :: BS_n_g_W
 integer :: BS_q(2)
 integer :: BS_identifier
 integer :: BS_k_and_row_restart(2)
 real(SP):: BS_eh_en(2)
 real(SP):: BS_eh_win(2)
 real(SP):: BS_drude_f_eh
 logical :: BS_W_is_diagonal
 logical :: BS_K_coupling
 logical :: BS_anti_res
 logical :: BS_res_K_exchange
 logical :: BS_res_K_corr
 logical :: BS_cpl_K_exchange
 logical :: BS_cpl_K_corr
 logical :: BS_K_is_ALDA
 logical :: BS_DB_is_fragmented
 logical :: BS_not_const_eh_f
 character(schlen) :: BSE_mode
 character(schlen) :: BS_res_mode
 character(schlen) :: BS_cpl_mode
 integer    , allocatable :: BS_eh_table(:,:)
 integer    , allocatable :: BS_blk_dim(:)
 complex(SP), allocatable :: BS_mat(:,:)
 complex(SP), allocatable :: BS_cpl_mat(:,:)
 real(SP)   , allocatable :: BS_eh_E(:)
 real(SP)   , allocatable :: BS_eh_f(:)
 complex(SP), allocatable :: BS_eh_Z(:)
 real(SP)   , allocatable :: BS_eh_W(:)
 !
 ! Oscillators ...
 !
 integer  :: O_ng
 integer,    allocatable :: O_phase(:,:,:)
 integer,    allocatable :: O_table(:,:,:,:,:)
 !
 ! ... first table of possible c/v states
 !
 integer,    allocatable :: O_n_c_states(:,:)
 integer,    allocatable :: O_n_v_states(:,:)
 integer,    allocatable :: O_c_state(:,:,:)
 integer,    allocatable :: O_v_state(:,:,:)
 !
 ! ... then table of possible cv->nm transitions
 !
 integer  :: O_n_scatt
 complex(SP),allocatable :: BS_O(:,:)
 !
 ! Solvers
 !
 character(lchlen):: BSS_description(100)
 character(schlen):: BSS_mode
 character(schlen):: BSS_inversion_mode
 integer  :: BSS_n_descs
 integer  :: BSS_n_freqs
 real(SP) :: BSS_er(2)
 real(SP) :: BSS_dr(2)
 real(SP) :: BSS_damp_reference
 real(SP) :: BSS_q0(3)
 real(SP) :: Haydock_treshold
 real(SP) :: Haydock_v0_mod
 logical  :: BSS_write_eig_2_db
 logical  :: Haydock_terminator
 logical  :: BSS_uses_RIM
 logical  :: BSS_Vnl_included
 logical  :: BSS_uses_GreenF
 logical  :: BSS_uses_partial_diago
#if defined _DISTRIBUTED
 logical  :: BS_exchange_done,BS_exchange_db
#endif
 complex(SP),allocatable :: BSS_rhoq0(:)
 !
 interface 
   !
#if defined _DISTRIBUTED
   integer function ioBS_DISTRIBUTED(iq,X,ID,blk_only)
     use X_m,           ONLY:X_t
     type(X_t):: X 
     logical, optional :: blk_only
     integer  :: iq,ID  
   end function
#endif
   !
   subroutine K_BSmat_by_V(iq,iter,Vi,Vo,iter_par)
     use pars,          ONLY:SP
     integer               ::iq
     integer               ::iter
     complex(SP)           ::Vi(:),Vo(:)
     real(SP),optional     ::iter_par
   end subroutine
   !
   integer function variables_BS(ID,local_description,X)
     use pars,       ONLY:lchlen
     use X_m,        ONLY:X_t
     integer            ::ID
     character(lchlen)  ::local_description(100)
     type(X_t), optional::X
   end function
   !
   integer function ioBSS_Haydock(ID,it,BS_H_dim,reached_treshold,Af,Bf,Vnm1,Vn,Vnp1,Cf,Cf_kerr)
     use pars,          ONLY:SP
     integer             :: ID,it,BS_H_dim
     real(SP)            :: reached_treshold,Bf(it+1),Af(it)
     complex(SP)         :: Vnm1(BS_H_dim),Vn(BS_H_dim)
     complex(SP),optional:: Cf(it+1),Vnp1(BS_H_dim)
     complex(SP),optional:: Cf_kerr(it+1)
   end function
   !
 end interface
 !
 contains
   !
   integer function BS_IO_index(ik1,ik2)
     integer :: ik1,ik2
     BS_IO_index=(ik2**2+ik2)/2-ik1+2
   end function
   !
   function BS_blocks(I)
     use R_lattice, ONLY:nXkibz
     integer :: BS_blocks(2)
     integer :: I,ik1,ik2
     BS_blocks=0
     do ik1=1,nXkibz
       do ik2=1,nXkibz
         if (BS_IO_index(ik1,ik2)==I) then
           BS_blocks=(/ik1,ik2/)
           return
         endif
       enddo
     enddo
   end function
   !
end module BS
