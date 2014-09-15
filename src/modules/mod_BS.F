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
 use pars,       ONLY:SP,schlen,lchlen
 use collision,  ONLY:ggwinfo
 use memory_m,   ONLY:mem_est
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
 integer :: BS_H_dim
 integer :: BS_bands(2)
 integer :: BS_q(2)
 integer :: BS_identifier
 !
 ! Transition Controls
 !=====================
 real(SP):: BS_eh_en(2)
 real(SP):: BS_eh_win(2)
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
 type(ggwinfo), save :: K_EXCH_collision
 type(ggwinfo), save :: K_CORR_collision
 !
 ! Haydock
 !========== 
 type Haydock_vector
   !
   complex(SP), allocatable :: fragment(:)
   !
 end type Haydock_vector
 !
 logical, allocatable :: Haydock_parallel_mask(:)
 integer, allocatable :: Haydock_parallel_group_comm(:)
 integer, allocatable :: Haydock_parallel_col_comm(:)
 !
 ! CV transitions space ...
 !=========================
 type BS_T_group
   !
   integer                       :: ik         ! ik index (IBZ)
   integer                       :: first_cv   ! Position in the cv transitions @ k
   integer                       :: size       
   integer,     allocatable      :: table(:,:) ! (/ikbz,iv,ic,i_sp/) (i_sp is spin, is is symmetry)
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
   integer                       :: ik         ! ik index (IBZ)
   integer                       :: ip         ! ip index (ik -> ip transition) (IBZ)
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
   ! ... correlation
   !------------------
   integer                       :: N_oscillators 
   integer,     allocatable      :: O_table(:,:,:,:) ! sym,n,n',spin @ k
   complex(SP), allocatable      :: O(:,:)
   !
 end type BS_block
 !
 integer                    :: n_BS_blks = 0
 type(BS_block), allocatable:: BS_blk(:)
 !
 ! WF phases
 !===========
 complex(SP),allocatable :: WF_phase(:,:,:,:)
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
 !
 ! Temporary (LARGE) arrays to be used in solvers ONLY
 !
 integer    , allocatable :: BSS_eh_table(:,:)
 real(SP)   , allocatable :: BSS_eh_E(:)
 real(SP)   , allocatable :: BSS_eh_f(:)
 real(SP)   , allocatable :: BSS_eh_W(:)
 complex(SP), allocatable :: BS_mat_diagonal(:)
 complex(SP), allocatable :: BS_mat(:,:)
 complex(SP), allocatable :: BS_cpl_mat(:,:)
 complex(SP), allocatable :: BSS_rhoq0(:)
 complex(SP), allocatable :: BSS_eh_Z(:)
 !
 interface 
   !
   integer function variables_BS(ID,local_description,X)
     use pars,       ONLY:lchlen
     use X_m,        ONLY:X_t
     integer            ::ID
     character(lchlen)  ::local_description(100)
     type(X_t), optional::X
   end function
   !
   integer function io_BSS_Haydock(ID,it,BS_H_dim,reached_treshold,Af,Bf,Vnm1,Vn,Vnp1,Cf)
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
   subroutine BS_oscillators_alloc(EXCHANGE,iG,CORRELATION,iB)
     !
     use pars,       ONLY:IP
     use stderr,     ONLY:intc
     use memory_m,   ONLY:mem_est
     !
     logical :: EXCHANGE,CORRELATION
     integer :: iG,iB
     !
     if (EXCHANGE) then
       allocate(BS_T_grp(iG)%O_x(BS_n_g_exch,BS_T_grp(iG)%size))
       call mem_est("BS_T_group_"//trim(intc(iG))//"_X_oscillators",(/size (BS_T_grp(iG)%O_x)/))
     endif
     !
     if (CORRELATION) then
       allocate(BS_blk(iB)%O(O_ng,BS_blk(iB)%N_oscillators))
       call mem_est("BS_blk_"//trim(intc(iB))//"_Oscillators",(/size (BS_blk(iB)%O)/))
     endif
     !
   end subroutine
   !
   subroutine BS_oscillators_free(iG,iB)
     use stderr,     ONLY:intc
     use memory_m,   ONLY:mem_est
     integer :: iG,IB,i_g,i_b
     do i_g=1,BS_nT_grps
       if (iG/=0.and.i_g/=iG) cycle
       if (allocated(BS_T_grp(i_g)%O_x)) then
         deallocate(BS_T_grp(i_g)%O_x)
         call mem_est("BS_T_group_"//trim(intc(i_g))//"_X_oscillators")
       endif
     enddo
     do i_b=1,n_BS_blks
       if (iB/=0.and.i_b/=iB) cycle
       if (allocated(BS_blk(i_b)%O)) then
         deallocate(BS_blk(i_b)%O)
         call mem_est("BS_blk_"//trim(intc(i_b))//"_O")
         deallocate(BS_blk(i_b)%O_table)
         call mem_est("BS_blk_"//trim(intc(i_b))//"_O_table")
       endif
     enddo
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
   subroutine BS_mat_alloc( )
     use pars,      ONLY:IP,cZERO,rZERO
     use electrons, ONLY:n_sp_pol
     integer :: i_g
     logical :: W_is_allocated,Z_is_allocated
     BS_H_dim=BS_K_dim
     if (BS_K_coupling) BS_H_dim=2*BS_K_dim
     allocate(BS_mat(BS_H_dim,BS_H_dim))
     call mem_est('BS_mat',(/size(BS_mat)/))
     BS_mat    = cZERO
     allocate(BSS_rhoq0(BS_H_dim))
     call mem_est('BSS_rhoq0',(/size(BSS_rhoq0)/))
     BSS_rhoq0 = cZERO
     allocate(BSS_eh_table(BS_K_dim,3+n_sp_pol-1),BSS_eh_E(BS_K_dim),BSS_eh_f(BS_K_dim))
     call mem_est("BSS_eh_table BSS_eh_E BSS_eh_f",&
&                 (/(3+n_sp_pol-1)*BS_K_dim,BS_K_dim,BS_K_dim/),(/IP,SP,SP/))
     BSS_eh_table = 0
     BSS_eh_E     = rZERO
     BSS_eh_f     = rZERO
     W_is_allocated=.FALSE.
     Z_is_allocated=.FALSE.
     do i_g=1,BS_nT_grps
       if (.not.W_is_allocated) W_is_allocated=allocated(BS_T_grp(i_g)%W)
       if (.not.Z_is_allocated) Z_is_allocated=allocated(BS_T_grp(i_g)%Z)
     enddo
     if (W_is_allocated) then
       allocate(BSS_eh_W(BS_K_dim))
       call mem_est("BSS_eh_W",(/BS_K_dim/),(/SP/))
       BSS_eh_W = rZERO
     endif
     if (Z_is_allocated) then
       allocate(BSS_eh_Z(BS_K_dim))
       call mem_est("BSS_eh_Z",(/BS_K_dim/))
       BSS_eh_Z = cZERO
     endif
   end subroutine
   !
   subroutine BS_mat_free()
     if (allocated(BSS_eh_table))      deallocate(BSS_eh_table)
     if (allocated(BSS_rhoq0))        deallocate(BSS_rhoq0)
     if (allocated(BSS_eh_E))          deallocate(BSS_eh_E)
     if (allocated(BSS_eh_W))          deallocate(BSS_eh_W)
     if (allocated(BSS_eh_f))          deallocate(BSS_eh_f)
     if (allocated(BSS_eh_Z))          deallocate(BSS_eh_Z)
     if (allocated(BS_mat))           deallocate(BS_mat)
     if( allocated(BS_mat_diagonal) ) deallocate(BS_mat_diagonal)
     call mem_est("BSS_eh_table BSS_eh_E BSS_eh_f BSS_eh_W")
     call mem_est("BSS_eh_table BSS_eh_Z BS_mat BS_mat_diagonal")
   end subroutine
   !
   subroutine BS_Blocks_and_Transitions_free()
     use stderr,     ONLY:intc
     use parallel_m, ONLY:PAR_BS_T_grps_index,PAR_BS_nT_col_grps
     integer :: i_g
     !
     if (allocated(BS_nT_at_k))          deallocate(BS_nT_at_k)
     if (allocated(BS_nT_grps_at_k))     deallocate(BS_nT_grps_at_k)
     if (allocated(PAR_BS_T_grps_index)) deallocate(PAR_BS_T_grps_index)
     call mem_est("BS_nT_at_k PAR_BS_T_grps_index BS_nT_grps_at_k")
     !
     do i_g=1,BS_nT_grps
       !
       if (.not.allocated(BS_T_grp(i_g)%table)) cycle
       !
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
       !
     enddo
     !
   end subroutine
   !
   subroutine BS_Blocks_symmetrize_K()
   !
   integer :: i_B,i_Tk,i_Tp,i_r,i_c
   !
   do i_B=1,n_BS_blks
     !
     i_Tk  =BS_blk(i_B)%iT_k
     i_Tp  =BS_blk(i_B)%iT_p
     !
     if (.not.(i_Tk.eq.i_Tp)) cycle
     forall(i_r=1:BS_blk(i_B)%size(1),i_c=1:BS_blk(i_B)%size(2),i_r.lt.i_c)&
              &BS_blk(i_B)%mat(i_c,i_r) = conjg(BS_blk(i_B)%mat(i_r,i_c))
   end do
   !
   end subroutine BS_Blocks_symmetrize_K
   !
   subroutine BS_HAYVEC_alloc(V)
     type(Haydock_vector) :: V(:)
     ! ws:
     integer :: i_g
     !
     do i_g=1,BS_nT_grps
       if (.not.Haydock_parallel_mask(i_g)) cycle
       allocate(V(i_g)%fragment(BS_T_grp(i_g)%size))
     enddo
     !
   end subroutine BS_HAYVEC_alloc
   !
   subroutine BS_HAYVEC_free(V)
     use stderr,     ONLY:intc 
     type(Haydock_vector) :: V(:)
     ! ws:
     integer :: i_g
     !
     do i_g=1,BS_nT_grps
       if (allocated(V(i_g)%fragment)) then
         deallocate(V(i_g)%fragment)
         !
       end if
     end do
     !
   end subroutine BS_HAYVEC_free
   !
end module BS
