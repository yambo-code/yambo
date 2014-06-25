!
!        Copyright (C) 2000-2014 the YAMBO team
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
module BS
 !
 use pars,      ONLY:SP,schlen,lchlen
 use collision, ONLY:ggwinfo
 !
 implicit none
 !
 ! Modes and logicals
 !====================
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
 character(schlen) :: BSK_mode
 character(schlen) :: BS_res_mode
 character(schlen) :: BS_cpl_mode
 !
 ! Dimensions
 !=============
 integer :: BS_K_dim
 integer :: BS_bands(2)
 integer :: BS_q(2)
 integer :: BS_identifier
 !
 ! Drude occupation
 !------------------
 real(SP):: BS_drude_f_eh
 !
 ! PW components ...
 !==================
 integer :: O_ng
 integer :: BS_n_g_exch
 integer :: BS_n_g_W
 !
 ! Collision
 !===========
 type(ggwinfo) :: BS_collision
 !
 ! CV transitions space ...
 !=========================
 type BS_T_group
   !
   integer                       :: ik         ! ik index (BZ)
   integer                       :: first_cv   ! Position in the cv transitions @ k
   integer                       :: size       
   complex(SP), allocatable      :: table(:,:) ! (/is,iv,ic,i_sp/) (i_sp is spin, is is symmetry)
   complex(SP), allocatable      :: dipoles_R(:)
   complex(SP), allocatable      :: dipoles_A(:)
   complex(SP), allocatable      :: Z(:)
   real(SP)   , allocatable      :: E(:)
   real(SP)   , allocatable      :: f(:)
   real(SP)   , allocatable      :: W(:)
   !
   ! Exchange Oscillators
   !
   complex(SP), allocatable      :: O_x(:,:)
   !
 end type BS_T_group
 !
 type(BS_T_group), allocatable  :: BS_T_grp(:)
 integer         , allocatable  :: BS_T_grp_1st_el(:)
 integer         , allocatable  :: BS_nT_at_k(:)
 integer         , allocatable  :: BS_nT_grps_at_k(:)
 integer                        :: BS_nT_grps
 !
 ! Blocks ...
 !============
 type BS_block
   integer                       :: ik         ! ik index (BZ)
   integer                       :: ip         ! ip index (ik -> ip transition) (BZ)
   integer                       :: iT_k       ! Index (among n_BS_T_grps) of the cv transitions @ k
   integer                       :: iT_p       ! Index (among n_BS_T_grps) of the cv transitions @ p
   integer                       :: size(2)    ! rows x columns
   character(1)                  :: mode       ! "R"esonant/"A"ntiresonant
   !
   ! Matrix Block
   !--------------
   complex(SP), allocatable      :: mat(:,:)
   !
   ! Oscillators...
   !
   ! ... correlation (resonant)
   !----------------------------
   integer                       :: N_C_cc_oscillators 
   integer                       :: N_C_vv_oscillators 
   integer(SP), allocatable      :: O_c_cc_table(:)
   integer(SP), allocatable      :: O_c_vv_table(:)
   complex(SP), allocatable      :: O_cc(:,:)
   complex(SP), allocatable      :: O_vv(:,:)
   !
   ! ... correlation (anti-resonant)
   !---------------------------------
   integer                       :: N_C_cv_oscillators = 0
   integer(SP), allocatable      :: O_c_cv_table(:)
   complex(SP), allocatable      :: O_cv(:,:)
   !
 end type BS_block
 !
 integer                    :: n_BS_blks = 0
 type(BS_block), allocatable:: BS_blk(:)
 !
 integer,    allocatable :: O_table(:,:,:,:,:)
 complex(SP),allocatable :: O_phase(:,:,:)
 !
 ! ... first table of possible c/v states
 integer,    allocatable :: O_n_c_states(:,:)
 integer,    allocatable :: O_n_v_states(:,:)
 integer,    allocatable :: O_c_state(:,:,:)
 integer,    allocatable :: O_v_state(:,:,:)
 !
 ! ... then table of possible cv->nm transitions
 integer  :: O_n_scatt
 complex(SP),allocatable :: BS_O(:,:)
 !
 integer :: BS_columns
 integer :: BS_k_and_row_restart(2)
 real(SP):: BS_eh_en(2)
 real(SP):: BS_eh_win(2)
 integer    , allocatable :: BS_eh_table(:,:)
 complex(SP), allocatable :: BS_mat_diagonal(:)
 complex(SP), allocatable :: BS_mat(:,:)
 complex(SP), allocatable :: BS_cpl_mat(:,:)
 real(SP)   , allocatable :: BS_eh_E(:)
 real(SP)   , allocatable :: BS_eh_f(:)
 complex(SP), allocatable :: BS_eh_Z(:)
 real(SP)   , allocatable :: BS_eh_W(:)
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
 complex(SP),allocatable :: BSS_rhoq0(:)
 !
 interface 
   !
   subroutine K_BSmat_by_V(iq,iter,Vi,Vo,iter_par,call_kind)
     use pars,          ONLY:SP
     integer               ::iq
     integer               ::iter
     complex(SP)           ::Vi(:),Vo(:)
     real(SP),optional     ::iter_par
     character(3),optional ::call_kind
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
   integer function ioBSS_Haydock(ID,it,BS_H_dim,reached_treshold,Af,Bf,Vnm1,Vn,Vnp1,Cf)
     use pars,          ONLY:SP
     integer             :: ID,it,BS_H_dim
     real(SP)            :: reached_treshold,Bf(it+1),Af(it)
     complex(SP)         :: Vnm1(BS_H_dim),Vn(BS_H_dim)
     complex(SP),optional:: Cf(it+1),Vnp1(BS_H_dim)
   end function
   !
 end interface
 !
 contains
   !
   integer function T_group(I)
     integer :: i_g,I
     do i_g=1,BS_nT_grps-1
       if (BS_T_grp_1st_el(i_g)<=I.and.I<BS_T_grp_1st_el(i_g+1)) then
         T_group=i_g
         return
       endif
     enddo
     if (I>=BS_T_grp_1st_el(BS_nT_grps-1)) T_group=BS_nT_grps
   end function
   !
   function BS_blocks(I)
     use R_lattice,      ONLY:nXkibz
     use matrix_operate, ONLY:UP_matrix_index
     integer :: BS_blocks(2)
     integer :: I,ik1,ik2
     BS_blocks=0
     do ik1=1,nXkibz
       do ik2=1,nXkibz
         if (UP_matrix_index(ik1,ik2)==I) then
           BS_blocks=(/ik1,ik2/)
           return
         endif
       enddo
     enddo
   end function
   !
   subroutine BS_oscillators_alloc(EXCHANGE,iT,CORRELATION,iB)
     !
     use pars,       ONLY:IP
     use stderr,     ONLY:intc
     use memory_m,   ONLY:mem_est
     !
     logical :: EXCHANGE,CORRELATION
     integer :: iT,iB
     !
     if (EXCHANGE) then
       allocate(BS_T_grp(iT)%O_x(BS_n_g_exch,BS_T_grp(iT)%size))
       call mem_est("BS_T_group_"//trim(intc(iT))//"_X_oscillators",(/size (BS_T_grp(iT)%O_x)/))
     endif
     !
   end subroutine
   !
   subroutine BS_oscillators_free( )
     !
   end subroutine
   !
   subroutine BS_Blocks_and_Transitions_alloc(E,iT)
     !
     use pars,       ONLY:IP
     use stderr,     ONLY:intc
     use memory_m,   ONLY:mem_est
     use electrons,     ONLY:levels
     integer      :: iT,nT
     type(levels) :: E
     !
     nT=BS_T_grp(iT)%size
     !
     allocate(BS_T_grp(iT)%dipoles_R(nT))
     call mem_est("BS_T_grp_dipoles_R_N"//trim(intc(iT)),(/nT/))
     if (BS_K_coupling) then
       allocate(BS_T_grp(iT)%dipoles_A(nT))
       call mem_est("BS_T_grp_dipoles_A_N"//trim(intc(iT)),(/nT/))
     endif
     allocate(BS_T_grp(iT)%table(nT,4))
     call mem_est("BS_T_grp_table_N"//trim(intc(iT)),(/nT/),(/IP/))
     allocate(BS_T_grp(iT)%E(nT))
     call mem_est("BS_T_grp_E_N"//trim(intc(iT)),(/nT/),(/SP/))
     allocate(BS_T_grp(iT)%f(nT))
     call mem_est("BS_T_grp_f_N"//trim(intc(iT)),(/nT/),(/SP/))
     if (associated(E%Z)) then
       allocate(BS_T_grp(iT)%Z(nT))
       call mem_est("BS_T_grp_Z_N"//trim(intc(iT)),(/nT/))
     endif
     if (associated(E%W)) then
       allocate(BS_T_grp(iT)%W(nT))
       call mem_est("BS_T_grp_W_N"//trim(intc(iT)),(/nT/),(/SP/))
     endif
     !
   end subroutine
   !
   subroutine BS_Blocks_and_Transitions_free()
     use stderr,     ONLY:intc
     use memory_m,   ONLY:mem_est
     use parallel_m, ONLY:PAR_BS_T_grps_index,PAR_BS_nT_col_grps
     integer :: i_g
     if (allocated(BS_nT_at_k))          deallocate(BS_nT_at_k)
     if (allocated(BS_nT_grps_at_k))     deallocate(BS_nT_grps_at_k)
     if (allocated(PAR_BS_T_grps_index)) deallocate(PAR_BS_T_grps_index)
     call mem_est("BS_nT_at_k PAR_BS_T_grps_index BS_nT_grps_at_k")
     do i_g=1,PAR_BS_nT_col_grps
       deallocate(BS_T_grp(i_g)%dipoles_R)
       call mem_est("BS_T_grp_dipoles_R_N"//trim(intc(i_g)))
       if (BS_K_coupling) then
         deallocate(BS_T_grp(i_g)%dipoles_A)
         call mem_est("BS_T_grp_dipoles_A_N"//trim(intc(i_g)))
       endif
       deallocate(BS_T_grp(i_g)%table)
       call mem_est("BS_T_grp_table_N"//trim(intc(i_g)))
       deallocate(BS_T_grp(i_g)%E)
       call mem_est("BS_T_grp_E_N"//trim(intc(i_g)))
       deallocate(BS_T_grp(i_g)%f)
       call mem_est("BS_T_grp_f_N"//trim(intc(i_g)))
       if (allocated(BS_T_grp(i_g)%Z)) then
         deallocate(BS_T_grp(i_g)%Z)
         call mem_est("BS_T_grp_Z_N"//trim(intc(i_g)))
       endif
       if (allocated(BS_T_grp(i_g)%W)) then
         deallocate(BS_T_grp(i_g)%W)
         call mem_est("BS_T_grp_W_N"//trim(intc(i_g)))
       endif
     enddo
   end subroutine
   !
end module BS
