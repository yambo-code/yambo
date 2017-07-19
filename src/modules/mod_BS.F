!
!        Copyright (C) 2000-2017 the YAMBO team
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
 use pars,       ONLY:IP,SP,schlen,lchlen
 use collision,  ONLY:elemental_collision
 use memory_m,   ONLY:mem_est
 !
 implicit none
 !
 ! Modes and logicals
 !====================
 logical :: l_BSE_minimize_memory
 logical :: BS_W_is_diagonal
 logical :: BS_K_coupling
 logical :: BS_res_K_exchange
 logical :: BS_res_K_corr
 logical :: BS_cpl_K_exchange
 logical :: BS_cpl_K_corr
 logical :: BS_K_is_ALDA
 logical :: BS_not_const_eh_f
 logical :: BS_K_has_been_calculated_loaded=.FALSE.
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
 ! Live Timing
 !=============
 integer     :: BS_LiveTiming_prefactor = 1
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
   complex(SP), allocatable      :: dipoles_R_PL(:,:)
   complex(SP), allocatable      :: dipoles_A_PL(:,:)
   complex(SP), allocatable      :: Z(:)
   real(SP)   , allocatable      :: E(:)
   real(SP)   , allocatable      :: f(:)
   real(SP)   , allocatable      :: f_RES(:)
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
 type(BS_T_group), allocatable  :: BS_T_grp(:)
 integer         , allocatable  :: BS_T_grp_1st_el(:)
 integer         , allocatable  :: BS_nT_at_k(:)
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
   integer,     allocatable      :: O_table(:,:,:,:,:) ! 2,sym,n,n',spin @ k = (iOnn,i_block_ref)
   complex(SP), allocatable      :: O_c(:,:)
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
     use memory_m,   ONLY:mem_est
     !
     logical :: EXCHANGE,CORRELATION
     integer :: iG,iB
     !
     if (EXCHANGE) then
       allocate(BS_T_grp(iG)%O_x(BS_n_g_exch,BS_T_grp(iG)%size))
       if(iG==1) call mem_est("BS_T_group_X_oscillators",(/BS_n_g_exch*sum(BS_T_grp(:)%size)/))
     endif
     !
     if (CORRELATION) then
       allocate(BS_blk(iB)%O_c(O_ng,BS_blk(iB)%N_oscillators))
     endif
     !
   end subroutine
   !
   subroutine BS_oscillators_free(iG,iB)
     use stderr,     ONLY:intc
     use memory_m,   ONLY:mem_est
     integer :: iG,iB
     integer :: i_g,i_b,ik_loop,ip_loop,ik_now,ip_now
     if (iG==BS_nT_grps) call mem_est("BS_T_group_X_oscillators")
     do i_g=iG,1,-1
       if (.not.allocated(BS_T_grp(i_g)%O_x)) exit
       if (     allocated(BS_T_grp(i_g)%O_x)) deallocate(BS_T_grp(i_g)%O_x)
     enddo
     if(iB==0) return
     ik_now=BS_blk(iB)%ik
     ip_now=BS_blk(iB)%ip
     if(iB==n_BS_blks) then
       ik_now=0
       ip_now=0
     endif
     do i_b=iB,1,-1
       ik_loop=BS_blk(i_b)%ik
       ip_loop=BS_blk(i_b)%ip
       if ( ik_now==ik_loop .and. ip_now==ip_loop .and. .not.l_BSE_minimize_memory ) cycle
       if (.not.allocated(BS_blk(i_b)%O_c)) exit
       if (     allocated(BS_blk(i_b)%O_c)) then
         deallocate(BS_blk(i_b)%O_c)
         deallocate(BS_blk(i_b)%O_table)
       endif
     enddo
     !
   end subroutine
   !
   subroutine BS_Blocks_and_Transitions_alloc(E,iT)
     !
     use pars,       ONLY:IP
     use drivers,    ONLY:l_kerr,l_photolum
     use stderr,     ONLY:intc
     use memory_m,   ONLY:mem_est
     use electrons,  ONLY:levels,E_SOC_nbands,n_sp_pol
     integer      :: iT,nT
     type(levels) :: E
     !
     if(iT==0) then
       nT=sum(BS_T_grp(:)%size)
       call mem_est("BS_T_grp_dipoles_R_N",(/nT/))
       if (BS_K_coupling) call mem_est("BS_T_grp_dipoles_A_N",(/nT/))
       if(l_kerr) then
         call mem_est("BS_T_grp_dipoles_KERR_R_N",(/nT/))
         if (BS_K_coupling) call mem_est("BS_T_grp_dipoles_KERR_A_N",(/nT/))
       endif
       if(l_photolum) then
         call mem_est("BS_T_grp_dipoles_PL_R_N",(/4*nT/))
         if (BS_K_coupling) call mem_est("BS_T_grp_dipoles_PL_A_N",(/4*nT/))
         call mem_est("BS_T_grp_f_PL",(/2*nT/),(/SP/))
       endif
       call mem_est("BS_T_grp_table_N",(/nT/),(/IP/))
       call mem_est("BS_T_grp_E_N",(/nT/),(/SP/))
       call mem_est("BS_T_grp_f_N",(/2*nT/),(/SP/))
       if (associated(E%Z)) call mem_est("BS_T_grp_Z_N",(/nT/))
       if (associated(E%W)) call mem_est("BS_T_grp_W_N",(/nT/),(/SP/))
       return
     endif
     !
     nT=BS_T_grp(iT)%size
     !
     allocate(BS_T_grp(iT)%dipoles_R(nT))
     if (BS_K_coupling) allocate(BS_T_grp(iT)%dipoles_A(nT))
     if(l_kerr) then
       allocate(BS_T_grp(iT)%dipoles_R_kerr(nT))
       if (BS_K_coupling) allocate(BS_T_grp(iT)%dipoles_A_kerr(nT))
     endif
     if(l_photolum) then
       allocate(BS_T_grp(iT)%dipoles_R_PL(nT,3))
       if (BS_K_coupling) allocate(BS_T_grp(iT)%dipoles_A_PL(nT,3))
     endif
     allocate(BS_T_grp(iT)%table(nT,4))
     allocate(BS_T_grp(iT)%E(nT))
     allocate(BS_T_grp(iT)%f(nT))
     allocate(BS_T_grp(iT)%f_RES(nT))
     if(l_photolum) allocate(BS_T_grp(iT)%f_PL(2,nT))
     if (associated(E%Z)) allocate(BS_T_grp(iT)%Z(nT))
     if (associated(E%W)) allocate(BS_T_grp(iT)%W(nT))
     if (E_SOC_nbands/=0) allocate(BS_T_grp(iT)%E_SOC_corr(2/n_sp_pol,nT))
     !
   end subroutine
   !
   subroutine BS_Blocks_and_Transitions_free()
     use stderr,     ONLY:intc
     use memory_m,   ONLY:mem_est
     use drivers,    ONLY:l_kerr,l_photolum
     use parallel_m, ONLY:PAR_BS_T_grps_index
     use electrons,  ONLY:E_SOC_nbands
     integer :: i_g
     !
     if (allocated(BS_nT_at_k))          deallocate(BS_nT_at_k)
     if (allocated(PAR_BS_T_grps_index)) deallocate(PAR_BS_T_grps_index)
     if (allocated(BS_T_grp_1st_el))     deallocate(BS_T_grp_1st_el)  
     call mem_est("BS_nT_at_k PAR_BS_T_grps_index BS_T_grp_1st_el")
     !
     do i_g=1,BS_nT_grps
       !
       if (.not.allocated(BS_T_grp(i_g)%table)) cycle
       !
       deallocate(BS_T_grp(i_g)%dipoles_R)
       if (BS_K_coupling) deallocate(BS_T_grp(i_g)%dipoles_A)
       if(l_kerr) then
         deallocate(BS_T_grp(i_g)%dipoles_R_kerr)
         if (BS_K_coupling) deallocate(BS_T_grp(i_g)%dipoles_A_kerr)
       endif
       deallocate(BS_T_grp(i_g)%table)
       deallocate(BS_T_grp(i_g)%E)
       deallocate(BS_T_grp(i_g)%f)
       deallocate(BS_T_grp(i_g)%f_RES)
       if (allocated(BS_T_grp(i_g)%f_PL)) deallocate(BS_T_grp(i_g)%f_PL)
       if (allocated(BS_T_grp(i_g)%Z))    deallocate(BS_T_grp(i_g)%Z)
       if (allocated(BS_T_grp(i_g)%W))    deallocate(BS_T_grp(i_g)%W)
       if (E_SOC_nbands/=0)               deallocate(BS_T_grp(i_g)%E_SOC_corr)
       !
     enddo
     !
     call mem_est("BS_T_grp_dipoles_R_N")
     call mem_est("BS_T_grp_dipoles_A_N")
     if(l_photolum) then
       call mem_est("BS_T_grp_dipoles_PL_R_N BS_T_grp_f_PL")
       if (BS_K_coupling) call mem_est("BS_T_grp_dipoles_PL_A_N")
     endif
     if(l_kerr) then
       call mem_est("BS_T_grp_dipoles_R_N")
       if (BS_K_coupling) call mem_est("BS_T_grp_dipoles_A_N")
     endif
     call mem_est("BS_T_grp_table_N")
     call mem_est("BS_T_grp_E_N")
     call mem_est("BS_T_grp_f_N")
     call mem_est("BS_T_grp_Z_N")
     call mem_est("BS_T_grp_W_N")
     !
   end subroutine
   !
   function BS_Block_size(i_block)
     integer              :: i_block
     integer(IP)          :: BS_Block_size
     !
     if (BS_blk(i_block)%iT_k==BS_blk(i_block)%iT_p.and.BS_blk(i_block)%mode=="R") then
       BS_Block_size= (BS_blk(i_block)%size(1)*(BS_blk(i_block)%size(1)+1))/2
     else
       BS_Block_size=BS_blk(i_block)%size(1)*BS_blk(i_block)%size(2)
     endif
     !
     BS_Block_size=BS_Block_size/BS_LiveTiming_prefactor
     !
   end function BS_Block_size
   !
   subroutine BS_Blocks_symmetrize_K()
     integer :: i_B,i_Tk,i_Tp,i_r,i_c
     do i_B=1,n_BS_blks
       !
       i_Tk  =BS_blk(i_B)%iT_k
       i_Tp  =BS_blk(i_B)%iT_p
       !
       ! Resonant part (mode ==R) is hermitian
       ! Coupling part (mode ==A) is symmetric
       !
       if (.not.(i_Tk.eq.i_Tp)) cycle
       if (BS_blk(i_B)%mode=="R") then
         forall(i_r=1:BS_blk(i_B)%size(1),&
&             i_c=1:BS_blk(i_B)%size(2),i_r.lt.i_c) BS_blk(i_B)%mat(i_c,i_r) = conjg(BS_blk(i_B)%mat(i_r,i_c))
       else
         forall(i_r=1:BS_blk(i_B)%size(1),&
&             i_c=1:BS_blk(i_B)%size(2),i_r.lt.i_c) BS_blk(i_B)%mat(i_c,i_r) = BS_blk(i_B)%mat(i_r,i_c)
       end if
     end do
     !
   end subroutine BS_Blocks_symmetrize_K
   !
end module BS
