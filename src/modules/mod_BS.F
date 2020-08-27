!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM, MG
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
 use pars,          ONLY:IP,SP,schlen,lchlen
 use collision_el,  ONLY:elemental_collision

 use memory, ONLY:MEM_err,MEM_msg,MEM_count,MEM_global_mesg,IPL
 implicit none
 !
 ! Modes and logicals
 !====================
 logical :: l_BSE_minimize_memory
 logical :: l_BSE_kernel_complete
 logical :: l_BSE_restart
 logical :: l_BSK_IO_triangular
 logical :: BS_W_is_diagonal
 logical :: BS_K_coupling
 logical :: BS_res_K_exchange
 logical :: BS_res_K_corr
 logical :: BS_cpl_K_exchange
 logical :: BS_cpl_K_corr
 logical :: BS_K_is_ALDA
 logical :: BS_not_const_eh_f
 logical :: BS_perturbative_SOC
 logical :: BS_K_has_been_calculated_loaded
 character(schlen) :: L_kind
 character(schlen) :: BSE_mode
 character(schlen) :: BSK_mode
 character(schlen) :: BS_res_mode
 character(schlen) :: BS_cpl_mode
 !
 ! Dimensions
 !=============
 integer :: BS_K_dim
 integer :: BS_H_dim
 integer :: BS_IO_dim(2)
 integer :: BS_bands(2)
 integer :: BS_q(2)
 integer :: BS_identifier
 real(SP):: BS_K_cutoff
 real(SP):: BS_max_val
 real(SP),allocatable :: BSqpts(:,:)
 !
 ! Live Timing
 !=============
 integer(IP) :: BS_LiveTiming_steps     = 0
 !
 ! Transition Controls
 !=====================
 real(SP):: BS_eh_en(2)
 real(SP):: BS_eh_win(2)
 !
 ! PW components ...
 !==================
 integer :: O_ng
 integer :: BS_n_g_exch
 integer :: BS_n_g_W
 !
 ! Collision
 !===========
 type(elemental_collision), save :: K_EXCH_collision
 type(elemental_collision), save :: K_CORR_collision
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
   complex(SP), allocatable      :: dipoles_R_kerr(:)
   complex(SP), allocatable      :: dipoles_A_kerr(:)
   complex(SP), allocatable      :: dipoles_R_mag(:,:)
   complex(SP), allocatable      :: dipoles_A_mag(:,:)
   complex(SP), allocatable      :: dipoles_R_PL(:,:)
   complex(SP), allocatable      :: dipoles_A_PL(:,:)
   complex(SP), allocatable      :: dipoles_R_dichroism(:,:)
   complex(SP), allocatable      :: dipoles_A_dichroism(:,:)
   complex(SP), allocatable      :: Z(:)
   real(SP)   , allocatable      :: E(:)
   real(SP)   , allocatable      :: E_SFLIP(:)
   real(SP)   , allocatable      :: f(:)
   real(SP)   , allocatable      :: f_RES(:)
   real(SP)   , allocatable      :: f_SFLIP(:)
   real(SP)   , allocatable      :: f_PL(:,:)
   real(SP)   , allocatable      :: W(:)
   real(SP)   , allocatable      :: E_SOC_corr(:,:)
   !
   ! Exchange Oscillators
   !
   complex(SP), allocatable      :: O_x(:,:)
   !
 end type BS_T_group
 !
 type(BS_T_group), allocatable, target :: BS_T_grp(:)
 integer         , allocatable  :: BS_T_grp_1st_el(:)
 integer         , allocatable  :: BS_nT_at_k(:)
 integer                        :: BS_nT_grps
 !
 ! Blocks ...
 !============
 type BS_block
   integer                       :: ik            ! ik index (IBZ)
   integer                       :: ip            ! ip index (ik -> ip transition) (IBZ)
   integer                       :: iT_k          ! Index (among n_BS_T_grps) of the cv transitions @ k
   integer                       :: iT_p          ! Index (among n_BS_T_grps) of the cv transitions @ p
   integer                       :: size(2)       ! rows x columns
   integer                       :: coordinate(2) ! Position of the left-bottom corner in the whole matrix
   character(1)                  :: mode          ! "R"esonant/"A"ntiresonant
   !
   ! Matrix Block
   !--------------
   complex(SP), allocatable      :: mat(:,:)
   character, allocatable        :: done(:,:)
   !
   ! Oscillators...
   !
   ! ... correlation
   !------------------
   integer                       :: N_oscillators 
   integer,     allocatable      :: kp_table(:,:)           ! 2,nkibz @ k = (i_kmq,i_pmq)
   integer,     allocatable      :: O_table(:,:,:,:,:,:,:)  ! 2,sym,nkmq,npmq,n,n',spin @ k = (iOnn,i_block_ref)
   complex(SP), allocatable      :: O_c(:,:)
   !
 end type BS_block
 !
 integer                    :: n_BS_blks
 integer                    :: n_BS_blks_min
 type(BS_block), allocatable, target :: BS_blk(:)
 !
 ! WF phases
 !===========
 complex(SP),allocatable :: WF_phase(:,:,:,:)
 !
 ! Screened interaction
 !======================
 complex(SP),allocatable ::BS_W(:,:,:)
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
     if (BS_nT_grps==1) then
       T_group=1
       return
     endif
     if (I>=BS_T_grp_1st_el(BS_nT_grps-1)) T_group=BS_nT_grps
   end function
   !
   subroutine BS_oscillators_alloc(EXCHANGE,iG,CORRELATION,iB)
     !
     use pars,       ONLY:IP
     use stderr,     ONLY:intc
     !
     logical :: EXCHANGE,CORRELATION
     integer :: iG,iB
     !
     if (EXCHANGE) then
       if (BS_T_grp(iG)%size==0) return
       allocate(BS_T_grp(iG)%O_x (BS_n_g_exch,BS_T_grp(iG)%size),  &
 & stat=MEM_err,errmsg=MEM_msg)
    if (     allocated(BS_T_grp(iG)%O_x)) &
 & call MEM_count("BS_T_grp(iG)%O_x",BS_T_grp(iG)%O_x)
    if (.not.allocated(BS_T_grp(iG)%O_x)) &
 & call MEM_error("BS_T_grp(iG)%O_x")

     endif
     !
     if (CORRELATION) then
       if (BS_blk(iB)%N_oscillators==0) return
       allocate(BS_blk(iB)%O_c (O_ng,BS_blk(iB)%N_oscillators),  &
 & stat=MEM_err,errmsg=MEM_msg)
    if (     allocated(BS_blk(iB)%O_c)) &
 & call MEM_count("BS_blk(iB)%O_c",BS_blk(iB)%O_c)
    if (.not.allocated(BS_blk(iB)%O_c)) &
 & call MEM_error("BS_blk(iB)%O_c")
     endif
     !
   end subroutine
   !
   subroutine BS_oscillators_free(iG_ref,iB_ref)
     use stderr,     ONLY:intc
     integer :: iG_ref,iB_ref
     integer :: iG,iB,ik_loop,ip_loop,ik_now,ip_now
     do iG=iG_ref,1,-1
       if (.not.allocated(BS_T_grp(iG)%O_x)) &
 & call MEM_free("BS_T_grp(iG)%O_x",int(-1,KIND=IPL))
    if (     allocated(BS_T_grp(iG)%O_x)) &
 & call MEM_free("BS_T_grp(iG)%O_x",size(BS_T_grp(iG)%O_x,KIND=IPL))
    if (     allocated(BS_T_grp(iG)%O_x)) &
 & deallocate(BS_T_grp(iG)%O_x)
     enddo
     if(iB_ref==0) return
     ik_now=BS_blk(iB_ref)%ik
     ip_now=BS_blk(iB_ref)%ip
     if(iB_ref==n_BS_blks) then
       ik_now=0
       ip_now=0
     endif
     do iB=iB_ref,1,-1
       ik_loop=BS_blk(iB)%ik
       ip_loop=BS_blk(iB)%ip
       if ( ik_now==ik_loop .and. ip_now==ip_loop ) cycle
       if ( ik_now==ik_loop .and. ip_now==ip_loop .and. .not.l_BSE_minimize_memory ) cycle
       if (.not.allocated(BS_blk(iB)%O_c)) exit
       if (.not.allocated(BS_blk(iB)%O_c)) &
 & call MEM_free("BS_blk(iB)%O_c",int(-1,KIND=IPL))
    if (     allocated(BS_blk(iB)%O_c)) &
 & call MEM_free("BS_blk(iB)%O_c",size(BS_blk(iB)%O_c,KIND=IPL))
    if (     allocated(BS_blk(iB)%O_c)) &
 & deallocate(BS_blk(iB)%O_c)
       if (.not.allocated(BS_blk(iB)%O_table)) &
 & call MEM_free("BS_blk(iB)%O_table",int(-1,KIND=IPL))
    if (     allocated(BS_blk(iB)%O_table)) &
 & call MEM_free("BS_blk(iB)%O_table",size(BS_blk(iB)%O_table,KIND=IPL))
    if (     allocated(BS_blk(iB)%O_table)) &
 & deallocate(BS_blk(iB)%O_table)
       if (.not.allocated(BS_blk(iB)%kp_table)) &
 & call MEM_free("BS_blk(iB)%kp_table",int(-1,KIND=IPL))
    if (     allocated(BS_blk(iB)%kp_table)) &
 & call MEM_free("BS_blk(iB)%kp_table",size(BS_blk(iB)%kp_table,KIND=IPL))
    if (     allocated(BS_blk(iB)%kp_table)) &
 & deallocate(BS_blk(iB)%kp_table)
     enddo
     !
   end subroutine
   !
   subroutine BS_Blocks_and_Transitions_alloc(E,iT)
     !
     use pars,       ONLY:IP,cZERO
     use drivers,    ONLY:l_kerr,l_photolum,l_magnons,l_dichroism
     use stderr,     ONLY:intc
     use electrons,  ONLY:levels,n_sp_pol
     integer      :: iT,nT
     type(levels) :: E
     !
     if(iT==0) then
       return
     endif
     !
     nT=BS_T_grp(iT)%size
     !
     allocate(BS_T_grp(iT)%dipoles_R (nT),  &
 & stat=MEM_err,errmsg=MEM_msg)
    if (     allocated(BS_T_grp(iT)%dipoles_R)) &
 & call MEM_count("BS_T_grp(iT)%dipoles_R",BS_T_grp(iT)%dipoles_R)
    if (.not.allocated(BS_T_grp(iT)%dipoles_R)) &
 & call MEM_error("BS_T_grp(iT)%dipoles_R")
     allocate(BS_T_grp(iT)%dipoles_A (nT),  &
 & stat=MEM_err,errmsg=MEM_msg)
    if (     allocated(BS_T_grp(iT)%dipoles_A)) &
 & call MEM_count("BS_T_grp(iT)%dipoles_A",BS_T_grp(iT)%dipoles_A)
    if (.not.allocated(BS_T_grp(iT)%dipoles_A)) &
 & call MEM_error("BS_T_grp(iT)%dipoles_A")
     BS_T_grp(iT)%dipoles_R=cZERO
     BS_T_grp(iT)%dipoles_A=cZERO
     if(l_kerr) then
       allocate(BS_T_grp(iT)%dipoles_R_kerr (nT),  &
 & stat=MEM_err,errmsg=MEM_msg)
    if (     allocated(BS_T_grp(iT)%dipoles_R_kerr)) &
 & call MEM_count("BS_T_grp(iT)%dipoles_R_kerr",BS_T_grp(iT)%dipoles_R_kerr)
    if (.not.allocated(BS_T_grp(iT)%dipoles_R_kerr)) &
 & call MEM_error("BS_T_grp(iT)%dipoles_R_kerr")
       allocate(BS_T_grp(iT)%dipoles_A_kerr (nT),  &
 & stat=MEM_err,errmsg=MEM_msg)
    if (     allocated(BS_T_grp(iT)%dipoles_A_kerr)) &
 & call MEM_count("BS_T_grp(iT)%dipoles_A_kerr",BS_T_grp(iT)%dipoles_A_kerr)
    if (.not.allocated(BS_T_grp(iT)%dipoles_A_kerr)) &
 & call MEM_error("BS_T_grp(iT)%dipoles_A_kerr")
     endif
     if(l_magnons) then
       allocate(BS_T_grp(iT)%dipoles_R_mag (2,nT),  &
 & stat=MEM_err,errmsg=MEM_msg)
    if (     allocated(BS_T_grp(iT)%dipoles_R_mag)) &
 & call MEM_count("BS_T_grp(iT)%dipoles_R_mag",BS_T_grp(iT)%dipoles_R_mag)
    if (.not.allocated(BS_T_grp(iT)%dipoles_R_mag)) &
 & call MEM_error("BS_T_grp(iT)%dipoles_R_mag")
       allocate(BS_T_grp(iT)%dipoles_A_mag (2,nT),  &
 & stat=MEM_err,errmsg=MEM_msg)
    if (     allocated(BS_T_grp(iT)%dipoles_A_mag)) &
 & call MEM_count("BS_T_grp(iT)%dipoles_A_mag",BS_T_grp(iT)%dipoles_A_mag)
    if (.not.allocated(BS_T_grp(iT)%dipoles_A_mag)) &
 & call MEM_error("BS_T_grp(iT)%dipoles_A_mag")
     endif
     if(l_photolum) then
       allocate(BS_T_grp(iT)%dipoles_R_PL (nT,3),  &
 & stat=MEM_err,errmsg=MEM_msg)
    if (     allocated(BS_T_grp(iT)%dipoles_R_PL)) &
 & call MEM_count("BS_T_grp(iT)%dipoles_R_PL",BS_T_grp(iT)%dipoles_R_PL)
    if (.not.allocated(BS_T_grp(iT)%dipoles_R_PL)) &
 & call MEM_error("BS_T_grp(iT)%dipoles_R_PL")
       allocate(BS_T_grp(iT)%dipoles_A_PL (nT,3),  &
 & stat=MEM_err,errmsg=MEM_msg)
    if (     allocated(BS_T_grp(iT)%dipoles_A_PL)) &
 & call MEM_count("BS_T_grp(iT)%dipoles_A_PL",BS_T_grp(iT)%dipoles_A_PL)
    if (.not.allocated(BS_T_grp(iT)%dipoles_A_PL)) &
 & call MEM_error("BS_T_grp(iT)%dipoles_A_PL")
     if(l_dichroism) then
       allocate(BS_T_grp(iT)%dipoles_R_dichroism (2,nT),  &
 & stat=MEM_err,errmsg=MEM_msg)
    if (     allocated(BS_T_grp(iT)%dipoles_R_dichroism)) &
 & call MEM_count("BS_T_grp(iT)%dipoles_R_dichroism",BS_T_grp(iT)%dipoles_R_dichroism)
    if (.not.allocated(BS_T_grp(iT)%dipoles_R_dichroism)) &
 & call MEM_error("BS_T_grp(iT)%dipoles_R_dichroism")
       allocate(BS_T_grp(iT)%dipoles_A_dichroism (2,nT),  &
 & stat=MEM_err,errmsg=MEM_msg)
    if (     allocated(BS_T_grp(iT)%dipoles_A_dichroism)) &
 & call MEM_count("BS_T_grp(iT)%dipoles_A_dichroism",BS_T_grp(iT)%dipoles_A_dichroism)
    if (.not.allocated(BS_T_grp(iT)%dipoles_A_dichroism)) &
 & call MEM_error("BS_T_grp(iT)%dipoles_A_dichroism")
     endif      
       
     endif
     allocate(BS_T_grp(iT)%table (nT,4),  &
 & stat=MEM_err,errmsg=MEM_msg)
    if (     allocated(BS_T_grp(iT)%table)) &
 & call MEM_count("BS_T_grp(iT)%table",BS_T_grp(iT)%table)
    if (.not.allocated(BS_T_grp(iT)%table)) &
 & call MEM_error("BS_T_grp(iT)%table")
     allocate(BS_T_grp(iT)%E (nT),  &
 & stat=MEM_err,errmsg=MEM_msg)
    if (     allocated(BS_T_grp(iT)%E)) &
 & call MEM_count("BS_T_grp(iT)%E",BS_T_grp(iT)%E)
    if (.not.allocated(BS_T_grp(iT)%E)) &
 & call MEM_error("BS_T_grp(iT)%E")
     allocate(BS_T_grp(iT)%E_SFLIP (nT),  &
 & stat=MEM_err,errmsg=MEM_msg)
    if (     allocated(BS_T_grp(iT)%E_SFLIP)) &
 & call MEM_count("BS_T_grp(iT)%E_SFLIP",BS_T_grp(iT)%E_SFLIP)
    if (.not.allocated(BS_T_grp(iT)%E_SFLIP)) &
 & call MEM_error("BS_T_grp(iT)%E_SFLIP")
     allocate(BS_T_grp(iT)%f (nT),  &
 & stat=MEM_err,errmsg=MEM_msg)
    if (     allocated(BS_T_grp(iT)%f)) &
 & call MEM_count("BS_T_grp(iT)%f",BS_T_grp(iT)%f)
    if (.not.allocated(BS_T_grp(iT)%f)) &
 & call MEM_error("BS_T_grp(iT)%f")
     allocate(BS_T_grp(iT)%f_RES (nT),  &
 & stat=MEM_err,errmsg=MEM_msg)
    if (     allocated(BS_T_grp(iT)%f_RES)) &
 & call MEM_count("BS_T_grp(iT)%f_RES",BS_T_grp(iT)%f_RES)
    if (.not.allocated(BS_T_grp(iT)%f_RES)) &
 & call MEM_error("BS_T_grp(iT)%f_RES")
     allocate(BS_T_grp(iT)%f_SFLIP (nT),  &
 & stat=MEM_err,errmsg=MEM_msg)
    if (     allocated(BS_T_grp(iT)%f_SFLIP)) &
 & call MEM_count("BS_T_grp(iT)%f_SFLIP",BS_T_grp(iT)%f_SFLIP)
    if (.not.allocated(BS_T_grp(iT)%f_SFLIP)) &
 & call MEM_error("BS_T_grp(iT)%f_SFLIP")
     allocate(BS_T_grp(iT)%f_PL (2,nT),  &
 & stat=MEM_err,errmsg=MEM_msg)
    if (     allocated(BS_T_grp(iT)%f_PL)) &
 & call MEM_count("BS_T_grp(iT)%f_PL",BS_T_grp(iT)%f_PL)
    if (.not.allocated(BS_T_grp(iT)%f_PL)) &
 & call MEM_error("BS_T_grp(iT)%f_PL")
     if (allocated(E%Z)) then
       allocate(BS_T_grp(iT)%Z (nT),  &
 & stat=MEM_err,errmsg=MEM_msg)
    if (     allocated(BS_T_grp(iT)%Z)) &
 & call MEM_count("BS_T_grp(iT)%Z",BS_T_grp(iT)%Z)
    if (.not.allocated(BS_T_grp(iT)%Z)) &
 & call MEM_error("BS_T_grp(iT)%Z")
     endif
     if (allocated(E%W)) then
       allocate(BS_T_grp(iT)%W (nT),  &
 & stat=MEM_err,errmsg=MEM_msg)
    if (     allocated(BS_T_grp(iT)%W)) &
 & call MEM_count("BS_T_grp(iT)%W",BS_T_grp(iT)%W)
    if (.not.allocated(BS_T_grp(iT)%W)) &
 & call MEM_error("BS_T_grp(iT)%W")
     endif
     if (allocated(E%E_SOC)) then
       allocate(BS_T_grp(iT)%E_SOC_corr (2/n_sp_pol,nT),  &
 & stat=MEM_err,errmsg=MEM_msg)
    if (     allocated(BS_T_grp(iT)%E_SOC_corr)) &
 & call MEM_count("BS_T_grp(iT)%E_SOC_corr",BS_T_grp(iT)%E_SOC_corr)
    if (.not.allocated(BS_T_grp(iT)%E_SOC_corr)) &
 & call MEM_error("BS_T_grp(iT)%E_SOC_corr")
     endif
     !
   end subroutine
   !
   subroutine BS_blks_free()
     integer :: iB
     do iB=1,n_BS_blks
       if (.not.allocated(BS_blk(iB)%mat)) &
 & call MEM_free("BS_blk(iB)%mat",int(-1,KIND=IPL))
    if (     allocated(BS_blk(iB)%mat)) &
 & call MEM_free("BS_blk(iB)%mat",size(BS_blk(iB)%mat,KIND=IPL))
    if (     allocated(BS_blk(iB)%mat)) &
 & deallocate(BS_blk(iB)%mat)
       deallocate(BS_blk(iB)%done)
     enddo
   end subroutine
   !
   subroutine BS_Blocks_and_Transitions_free()
     use stderr,     ONLY:intc
     use drivers,    ONLY:l_kerr,l_photolum,l_magnons,l_dichroism
     use parallel_m, ONLY:PAR_BS_T_grps_index
     integer :: iT
     !
     if (.not.allocated(BS_nT_at_k)) &
 & call MEM_free("BS_nT_at_k",int(-1,KIND=IPL))
    if (     allocated(BS_nT_at_k)) &
 & call MEM_free("BS_nT_at_k",size(BS_nT_at_k,KIND=IPL))
    if (     allocated(BS_nT_at_k)) &
 & deallocate(BS_nT_at_k)
     if (.not.allocated(PAR_BS_T_grps_index)) &
 & call MEM_free("PAR_BS_T_grps_index",int(-1,KIND=IPL))
    if (     allocated(PAR_BS_T_grps_index)) &
 & call MEM_free("PAR_BS_T_grps_index",size(PAR_BS_T_grps_index,KIND=IPL))
    if (     allocated(PAR_BS_T_grps_index)) &
 & deallocate(PAR_BS_T_grps_index)
     if (.not.allocated(BS_T_grp_1st_el)) &
 & call MEM_free("BS_T_grp_1st_el",int(-1,KIND=IPL))
    if (     allocated(BS_T_grp_1st_el)) &
 & call MEM_free("BS_T_grp_1st_el",size(BS_T_grp_1st_el,KIND=IPL))
    if (     allocated(BS_T_grp_1st_el)) &
 & deallocate(BS_T_grp_1st_el)
     !
     do iT=1,BS_nT_grps
       !
       if (.not.allocated(BS_T_grp(iT)%table)) cycle
       !
       if (.not.allocated(BS_T_grp(iT)%dipoles_R)) &
 & call MEM_free("BS_T_grp(iT)%dipoles_R",int(-1,KIND=IPL))
    if (     allocated(BS_T_grp(iT)%dipoles_R)) &
 & call MEM_free("BS_T_grp(iT)%dipoles_R",size(BS_T_grp(iT)%dipoles_R,KIND=IPL))
    if (     allocated(BS_T_grp(iT)%dipoles_R)) &
 & deallocate(BS_T_grp(iT)%dipoles_R)
       if (.not.allocated(BS_T_grp(iT)%dipoles_A)) &
 & call MEM_free("BS_T_grp(iT)%dipoles_A",int(-1,KIND=IPL))
    if (     allocated(BS_T_grp(iT)%dipoles_A)) &
 & call MEM_free("BS_T_grp(iT)%dipoles_A",size(BS_T_grp(iT)%dipoles_A,KIND=IPL))
    if (     allocated(BS_T_grp(iT)%dipoles_A)) &
 & deallocate(BS_T_grp(iT)%dipoles_A)
       if (l_kerr) then
         if (.not.allocated(BS_T_grp(iT)%dipoles_R_kerr)) &
 & call MEM_free("BS_T_grp(iT)%dipoles_R_kerr",int(-1,KIND=IPL))
    if (     allocated(BS_T_grp(iT)%dipoles_R_kerr)) &
 & call MEM_free("BS_T_grp(iT)%dipoles_R_kerr",size(BS_T_grp(iT)%dipoles_R_kerr,KIND=IPL))
    if (     allocated(BS_T_grp(iT)%dipoles_R_kerr)) &
 & deallocate(BS_T_grp(iT)%dipoles_R_kerr)
         if (.not.allocated(BS_T_grp(iT)%dipoles_A_kerr)) &
 & call MEM_free("BS_T_grp(iT)%dipoles_A_kerr",int(-1,KIND=IPL))
    if (     allocated(BS_T_grp(iT)%dipoles_A_kerr)) &
 & call MEM_free("BS_T_grp(iT)%dipoles_A_kerr",size(BS_T_grp(iT)%dipoles_A_kerr,KIND=IPL))
    if (     allocated(BS_T_grp(iT)%dipoles_A_kerr)) &
 & deallocate(BS_T_grp(iT)%dipoles_A_kerr)
       endif
       if (l_magnons) then
         if (.not.allocated(BS_T_grp(iT)%dipoles_R_mag)) &
 & call MEM_free("BS_T_grp(iT)%dipoles_R_mag",int(-1,KIND=IPL))
    if (     allocated(BS_T_grp(iT)%dipoles_R_mag)) &
 & call MEM_free("BS_T_grp(iT)%dipoles_R_mag",size(BS_T_grp(iT)%dipoles_R_mag,KIND=IPL))
    if (     allocated(BS_T_grp(iT)%dipoles_R_mag)) &
 & deallocate(BS_T_grp(iT)%dipoles_R_mag)
         if (.not.allocated(BS_T_grp(iT)%dipoles_A_mag)) &
 & call MEM_free("BS_T_grp(iT)%dipoles_A_mag",int(-1,KIND=IPL))
    if (     allocated(BS_T_grp(iT)%dipoles_A_mag)) &
 & call MEM_free("BS_T_grp(iT)%dipoles_A_mag",size(BS_T_grp(iT)%dipoles_A_mag,KIND=IPL))
    if (     allocated(BS_T_grp(iT)%dipoles_A_mag)) &
 & deallocate(BS_T_grp(iT)%dipoles_A_mag)
       endif
       if (l_dichroism) then
         if (.not.allocated(BS_T_grp(iT)%dipoles_R_dichroism)) &
 & call MEM_free("BS_T_grp(iT)%dipoles_R_dichroism",int(-1,KIND=IPL))
    if (     allocated(BS_T_grp(iT)%dipoles_R_dichroism)) &
 & call MEM_free("BS_T_grp(iT)%dipoles_R_dichroism",size(BS_T_grp(iT)%dipoles_R_dichroism,KIND=IPL))
    if (     allocated(BS_T_grp(iT)%dipoles_R_dichroism)) &
 & deallocate(BS_T_grp(iT)%dipoles_R_dichroism)
         if (.not.allocated(BS_T_grp(iT)%dipoles_A_dichroism)) &
 & call MEM_free("BS_T_grp(iT)%dipoles_A_dichroism",int(-1,KIND=IPL))
    if (     allocated(BS_T_grp(iT)%dipoles_A_dichroism)) &
 & call MEM_free("BS_T_grp(iT)%dipoles_A_dichroism",size(BS_T_grp(iT)%dipoles_A_dichroism,KIND=IPL))
    if (     allocated(BS_T_grp(iT)%dipoles_A_dichroism)) &
 & deallocate(BS_T_grp(iT)%dipoles_A_dichroism)
       endif
         
       if (.not.allocated(BS_T_grp(iT)%table)) &
 & call MEM_free("BS_T_grp(iT)%table",int(-1,KIND=IPL))
    if (     allocated(BS_T_grp(iT)%table)) &
 & call MEM_free("BS_T_grp(iT)%table",size(BS_T_grp(iT)%table,KIND=IPL))
    if (     allocated(BS_T_grp(iT)%table)) &
 & deallocate(BS_T_grp(iT)%table)
       if (.not.allocated(BS_T_grp(iT)%E)) &
 & call MEM_free("BS_T_grp(iT)%E",int(-1,KIND=IPL))
    if (     allocated(BS_T_grp(iT)%E)) &
 & call MEM_free("BS_T_grp(iT)%E",size(BS_T_grp(iT)%E,KIND=IPL))
    if (     allocated(BS_T_grp(iT)%E)) &
 & deallocate(BS_T_grp(iT)%E)
       if (.not.allocated(BS_T_grp(iT)%E_SFLIP)) &
 & call MEM_free("BS_T_grp(iT)%E_SFLIP",int(-1,KIND=IPL))
    if (     allocated(BS_T_grp(iT)%E_SFLIP)) &
 & call MEM_free("BS_T_grp(iT)%E_SFLIP",size(BS_T_grp(iT)%E_SFLIP,KIND=IPL))
    if (     allocated(BS_T_grp(iT)%E_SFLIP)) &
 & deallocate(BS_T_grp(iT)%E_SFLIP)
       if (.not.allocated(BS_T_grp(iT)%f)) &
 & call MEM_free("BS_T_grp(iT)%f",int(-1,KIND=IPL))
    if (     allocated(BS_T_grp(iT)%f)) &
 & call MEM_free("BS_T_grp(iT)%f",size(BS_T_grp(iT)%f,KIND=IPL))
    if (     allocated(BS_T_grp(iT)%f)) &
 & deallocate(BS_T_grp(iT)%f)
       if (.not.allocated(BS_T_grp(iT)%f_RES)) &
 & call MEM_free("BS_T_grp(iT)%f_RES",int(-1,KIND=IPL))
    if (     allocated(BS_T_grp(iT)%f_RES)) &
 & call MEM_free("BS_T_grp(iT)%f_RES",size(BS_T_grp(iT)%f_RES,KIND=IPL))
    if (     allocated(BS_T_grp(iT)%f_RES)) &
 & deallocate(BS_T_grp(iT)%f_RES)
       if (.not.allocated(BS_T_grp(iT)%f_SFLIP)) &
 & call MEM_free("BS_T_grp(iT)%f_SFLIP",int(-1,KIND=IPL))
    if (     allocated(BS_T_grp(iT)%f_SFLIP)) &
 & call MEM_free("BS_T_grp(iT)%f_SFLIP",size(BS_T_grp(iT)%f_SFLIP,KIND=IPL))
    if (     allocated(BS_T_grp(iT)%f_SFLIP)) &
 & deallocate(BS_T_grp(iT)%f_SFLIP)
       if (.not.allocated(BS_T_grp(iT)%f_PL)) &
 & call MEM_free("BS_T_grp(iT)%f_PL",int(-1,KIND=IPL))
    if (     allocated(BS_T_grp(iT)%f_PL)) &
 & call MEM_free("BS_T_grp(iT)%f_PL",size(BS_T_grp(iT)%f_PL,KIND=IPL))
    if (     allocated(BS_T_grp(iT)%f_PL)) &
 & deallocate(BS_T_grp(iT)%f_PL)
       if (.not.allocated(BS_T_grp(iT)%Z)) &
 & call MEM_free("BS_T_grp(iT)%Z",int(-1,KIND=IPL))
    if (     allocated(BS_T_grp(iT)%Z)) &
 & call MEM_free("BS_T_grp(iT)%Z",size(BS_T_grp(iT)%Z,KIND=IPL))
    if (     allocated(BS_T_grp(iT)%Z)) &
 & deallocate(BS_T_grp(iT)%Z)
       if (.not.allocated(BS_T_grp(iT)%W)) &
 & call MEM_free("BS_T_grp(iT)%W",int(-1,KIND=IPL))
    if (     allocated(BS_T_grp(iT)%W)) &
 & call MEM_free("BS_T_grp(iT)%W",size(BS_T_grp(iT)%W,KIND=IPL))
    if (     allocated(BS_T_grp(iT)%W)) &
 & deallocate(BS_T_grp(iT)%W)
       if (.not.allocated(BS_T_grp(iT)%E_SOC_corr)) &
 & call MEM_free("BS_T_grp(iT)%E_SOC_corr",int(-1,KIND=IPL))
    if (     allocated(BS_T_grp(iT)%E_SOC_corr)) &
 & call MEM_free("BS_T_grp(iT)%E_SOC_corr",size(BS_T_grp(iT)%E_SOC_corr,KIND=IPL))
    if (     allocated(BS_T_grp(iT)%E_SOC_corr)) &
 & deallocate(BS_T_grp(iT)%E_SOC_corr)
       !
     enddo
     !
     if (.not.allocated(BS_T_grp)) &
 & call MEM_free("BS_T_grp",int(-1,KIND=IPL))
    if (     allocated(BS_T_grp)) &
 & call MEM_free("BS_T_grp",size(BS_T_grp,KIND=IPL))
    if (     allocated(BS_T_grp)) &
 & deallocate(BS_T_grp)
     if (.not.allocated(BS_blk)) &
 & call MEM_free("BS_blk",int(-1,KIND=IPL))
    if (     allocated(BS_blk)) &
 & call MEM_free("BS_blk",size(BS_blk,KIND=IPL))
    if (     allocated(BS_blk)) &
 & deallocate(BS_blk)
     !
   end subroutine
   !
   function BS_Block_size(i_block)
     !
     integer              :: i_block
     integer(IP)          :: BS_Block_size
     !
     if (BS_blk(i_block)%iT_k==BS_blk(i_block)%iT_p.and.BS_blk(i_block)%mode=="R") then
       BS_Block_size= (BS_blk(i_block)%size(1)*(BS_blk(i_block)%size(1)+1))/2
     else
       BS_Block_size=BS_blk(i_block)%size(1)*BS_blk(i_block)%size(2)
     endif
     !
   end function BS_Block_size
   !
   subroutine BS_Blocks_symmetrize_K()
     !
     ! Symmetrize the DIAGONAL blocks only (i_Tk=i_Tp)
     !
     integer :: i_B,i_Tk,i_Tp,i_r,i_c
     !
     do i_B=1,n_BS_blks
       !
       i_Tk  =BS_blk(i_B)%iT_k
       i_Tp  =BS_blk(i_B)%iT_p
       !
       ! Resonant part (mode ==R) is hermitian
       ! Coupling part (mode ==A) is symmetric
       !
       if (i_Tk/=i_Tp) cycle
       !
       do i_r=1,BS_blk(i_B)%size(1)
         do i_c=i_r+1,BS_blk(i_B)%size(2)
           if (BS_blk(i_B)%mode=="R") then
             BS_blk(i_B)%mat(i_c,i_r) = conjg(BS_blk(i_B)%mat(i_r,i_c))
           else
             BS_blk(i_B)%mat(i_c,i_r) = BS_blk(i_B)%mat(i_r,i_c)
           endif
         enddo
       enddo
       !
     enddo
     !
   end subroutine BS_Blocks_symmetrize_K
   !
end module BS
