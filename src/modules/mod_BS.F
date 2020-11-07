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
#include<memory.h>
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
 character(schlen) :: BSE_prop
 character(schlen) :: BSK_mode
 character(schlen) :: BS_res_mode
 character(schlen) :: BS_cpl_mode
 !
 ! Kind of response function
 !===========================
 logical :: l_BS_jdos
 logical :: l_BS_trace
 logical :: l_BS_abs
 logical :: l_BS_kerr
 logical :: l_BS_photolum
 logical :: l_BS_magnons
 logical :: l_BS_dichroism
 logical :: l_BS_anomalous_hall
 logical :: l_BS_ares_asymm ! For cases where the ares block is not -R*:
                            ! - n_sp_pol==2 finite Q or magnons, Res and Ares are independent: --> no coupling = diag 2 BSE matricies
                            !                                                                  -->    coupling = 1 BSE matrix with 4 blocks, no symmetrization
                            ! - n_spinor==2, finite Q only,      Res and Ares are independent: --> no coupling = diag 2 BSE matricies
                            !                                                                  -->    coupling = 1 BSE matrix with 4 blocks, no symmetrization
 !
 ! Dimensions
 !=============
 integer :: BS_K_dim(2)
 integer :: BS_H_dim
 integer :: BS_mat_res_ares_dim ! If resonant and non-resonant part are not related
 integer :: BS_res_ares_dim     ! If resonant and non-resonant part are not related
 integer :: BS_IO_dim(2,2)
 integer :: BS_bands(2)
 integer :: BS_q(2)
 integer :: BS_identifier
 integer :: BS_dip_size
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
 ! Dimensions tables
 ! 
 ! R/A Asymmetry: finite q BSE & magnons with n_sp_pol=2
 ! In the asymmetric case also the anti-resonant residuals are in the groups
 !
 !  R/A symm    RES/RET/CPL   BSS_f/BSS_E/BSS_dip   res_ares_dim    BS_mats_to_solve     BS_mat_size    
 !
 !    S          RES             BS_K_dim(1)              1              1                 BS_K_dim(1)
 !
 !    S          RET             BS_K_dim(1)              1              1                 BS_K_dim(1)
 !    S          CPL             BS_H_dim                 1              1                 BS_H_dim
 !
 !    A          RET          BS_K_dim(1)+BS_K_dim(2)     2              2                 BS_K_dim(1),BS_K_dim(2)
 !    A          CPL             BS_H_dim                 2              1                 BS_H_dim
 !
 !
 ! CV transitions space ...
 !=========================
 type BS_T_group
   !
   integer                       :: i_T_ref                    ! first group of the res or ares block
   integer                       :: i_res_ares                 ! if it is a resonant or an anti-resonant transition
   integer                       :: ik                         ! ik index (IBZ)
   integer                       :: first_cv                   ! Position in the cv transitions @ k
   integer                       :: size
   integer,     allocatable      :: table(:,:)                 ! (/ i_Tr;5/) 5->(/ikbz,iv,ic,i_sp_v,i_sp_c/)
   real(SP)   , allocatable      :: E(:)                       ! (/ i_Tr  /)
   real(SP)   , allocatable      :: f(:)
   real(SP)   , allocatable      :: f_RES(:)
   real(SP)   , allocatable      :: f_PL(:,:)                    ! (/ i_Tr ; 2 /) This is always of size 2 since fv(1-fc) and fc(1-fv) are independent
   real(SP)   , allocatable      :: W(:)
   real(SP)   , allocatable      :: E_SOC_corr(:,:)            ! (/2/n_sp_pol; i_Tr /)
   complex(SP), allocatable      :: Z(:)
   ! For dipoles the anti-resonant component is always stored
   complex(SP), allocatable      :: dipoles_opt(:,:,:)         ! (/x,y,z ; i_Tr ;   2)
   complex(SP), allocatable      :: dipoles_dic(:,:,:)         ! (/x,y,z ; i_Tr ;   2) 
   complex(SP), allocatable      :: dipoles_mag(:,:,:)         ! (/S+,S-,Sz ; i_Tr; 2)
   !
   ! Exchange Oscillators
   !
   complex(SP), allocatable      :: O_x(:,:)
#ifdef _CUDA
   complex(SP), allocatable, device :: O_x_d(:,:)
#endif
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
   integer                       :: iT_k_st       ! Index to start counting for H_pos @ k
   integer                       :: iT_p_st       ! Index to start counting for H_pos @ p
   integer                       :: ira_k         ! Index if transition is resonant or aresonant @ k
   integer                       :: ira_p         ! Index if transition is resonant or aresonant @ p
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
#ifdef _CUDA
   complex(SP), allocatable, device :: O_c_d(:,:)
#endif
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
#ifdef _CUDA
 attributes(device)      ::BS_W
#endif
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
     logical, intent(in) :: EXCHANGE,CORRELATION
     integer, intent(in) :: iG,iB
     !
     if (EXCHANGE) then
       if (BS_T_grp(iG)%size==0) return
       YAMBO_ALLOC(BS_T_grp(iG)%O_x,(BS_n_g_exch,BS_T_grp(iG)%size))
#ifdef _CUDA
       allocate(BS_T_grp(iG)%O_x_d,mold=BS_T_grp(iG)%O_x)
#endif

     endif
     !
     if (CORRELATION) then
       if (BS_blk(iB)%N_oscillators==0) return
       YAMBO_ALLOC(BS_blk(iB)%O_c,(O_ng,BS_blk(iB)%N_oscillators))
#ifdef _CUDA
       allocate(BS_blk(iB)%O_c_d,mold=BS_blk(iB)%O_c)
#endif
     endif
     !
   end subroutine
   !
   subroutine BS_oscillators_free(iG_ref,iB_ref)
     use stderr,     ONLY:intc
     integer, intent(in) :: iG_ref,iB_ref
     integer :: iG,iB,ik_loop,ip_loop,ik_now,ip_now
     do iG=iG_ref,1,-1
       YAMBO_FREE(BS_T_grp(iG)%O_x)
#ifdef _CUDA
       if (allocated(BS_T_grp(iG)%O_x_d)) deallocate(BS_T_grp(iG)%O_x_d)
#endif
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
       YAMBO_FREE(BS_blk(iB)%O_c)
       YAMBO_FREE(BS_blk(iB)%O_table)
       YAMBO_FREE(BS_blk(iB)%kp_table)
#ifdef _CUDA
       if (allocated(BS_blk(iB)%O_c_d)) deallocate(BS_blk(iB)%O_c_d)
#endif
     enddo
     !
   end subroutine
   !
   subroutine BS_Blocks_and_Transitions_alloc(E,iT)
     !
     use pars,       ONLY:IP,cZERO
     use stderr,     ONLY:intc
     use electrons,  ONLY:levels,n_sp_pol
     integer,      intent(in) :: iT
     type(levels), intent(in) :: E
     integer :: nT,nDIP
     !
     nT=BS_T_grp(iT)%size
     nDIP=2/BS_res_ares_dim
     !
     YAMBO_ALLOC(BS_T_grp(iT)%dipoles_opt,(BS_dip_size,nT,nDIP))
     BS_T_grp(iT)%dipoles_opt=cZERO
     if(l_BS_magnons) then
       YAMBO_ALLOC(BS_T_grp(iT)%dipoles_mag,(2,nT,nDIP))
     endif
     if(l_BS_dichroism) then
       YAMBO_ALLOC(BS_T_grp(iT)%dipoles_dic,(BS_dip_size,nT,nDIP))
     endif
     YAMBO_ALLOC(BS_T_grp(iT)%table,(nT,5))
     YAMBO_ALLOC(BS_T_grp(iT)%E,(nT))
     YAMBO_ALLOC(BS_T_grp(iT)%f,(nT))
     YAMBO_ALLOC(BS_T_grp(iT)%f_RES,(nT))
     YAMBO_ALLOC(BS_T_grp(iT)%f_PL,(nT,2))
     if (allocated(E%Z)) then
       YAMBO_ALLOC(BS_T_grp(iT)%Z,(nT))
     endif
     if (allocated(E%W)) then
       YAMBO_ALLOC(BS_T_grp(iT)%W,(nT))
     endif
     if (allocated(E%E_SOC)) then
       YAMBO_ALLOC(BS_T_grp(iT)%E_SOC_corr,(2/n_sp_pol,nT))
     endif
     !
   end subroutine
   !
   subroutine BS_blks_free(i_BS_mat)
     integer, intent(in) :: i_BS_mat
     integer :: iB
     do iB=1,n_BS_blks
       if(i_BS_mat/=BS_blk(iB)%ira_k) cycle
       if(.not.allocated(BS_blk(iB)%mat)) cycle
       YAMBO_FREE(BS_blk(iB)%mat)
       deallocate(BS_blk(iB)%done)
     enddo
   end subroutine
   !
   subroutine BS_Blocks_and_Transitions_free()
     use stderr,     ONLY:intc
     use parallel_m, ONLY:PAR_BS_T_grps_index
     integer :: iT
     !
     YAMBO_FREE(BS_nT_at_k)
     YAMBO_FREE(PAR_BS_T_grps_index)
     YAMBO_FREE(BS_T_grp_1st_el)
     !
     do iT=1,BS_nT_grps
       !
       if (.not.allocated(BS_T_grp(iT)%table)) cycle
       !
       YAMBO_FREE(BS_T_grp(iT)%dipoles_opt)
       if (l_BS_magnons) then
         YAMBO_FREE(BS_T_grp(iT)%dipoles_mag)
       endif
       if (l_BS_dichroism) then
         YAMBO_FREE(BS_T_grp(iT)%dipoles_dic)
       endif
       YAMBO_FREE(BS_T_grp(iT)%table)
       YAMBO_FREE(BS_T_grp(iT)%E)
       YAMBO_FREE(BS_T_grp(iT)%f)
       YAMBO_FREE(BS_T_grp(iT)%f_RES)
       YAMBO_FREE(BS_T_grp(iT)%f_PL)
       YAMBO_FREE(BS_T_grp(iT)%Z)
       YAMBO_FREE(BS_T_grp(iT)%W)
       YAMBO_FREE(BS_T_grp(iT)%E_SOC_corr)
       !
     enddo
     !
     YAMBO_FREE(BS_T_grp)
     YAMBO_FREE(BS_blk)
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
