!
!        Copyright (C) 2000-2021 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM, DS
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
subroutine K_Transitions_setup(mode,iq,Ken,Xk,Dip)
 !
 ! mode=K
 ! ======
 ! Out: 
 !
 !   BS_K_dim(1)
 !   BS_H_dim
 !   BS_nT_at_k
 !
 ! mode=T
 ! ======
 ! Filling of Transition groups
 !
 !type BS_T_group
 !  integer                       :: ik         ! ik index (\in IBZ)
 !  integer                       :: first_cv   ! Position in the cv transitions @ k
 !  integer                       :: size       
 !  complex(SP), allocatable      :: table(size,5,R/A) ; 5 --> (/ikbz,iv,ic,i_sp_c,i_sp_v/)
 !  complex(SP), allocatable      :: Z(size,R/A)
 !  real(SP)   , allocatable      :: E(size,R/A)
 !  real(SP)   , allocatable      :: f(size,R/A)
 !  real(SP)   , allocatable      :: W(size,R/A)
 !end type BS_T_group
 !
 use pars,         ONLY:SP
 use stderr,       ONLY:intc
 use com,          ONLY:isec
 use BS_solvers,   ONLY:BSS_mode
 ! mode ="T"
 use BS,           ONLY:T_group,BS_bands,BS_T_grp,BS_nT_grps,BS_n_eh_spaces,&
&                       BS_Blocks_and_Transitions_alloc,BS_perturbative_SOC
 ! mode ="K"
 use BS,           ONLY:BS_K_dim,BS_H_dim,BS_nT_at_k,BS_K_coupling,  &
&                       l_BS_ares_from_res,BS_n_eh_spaces,BS_res_ares_n_mat
 use DIPOLES,      ONLY:DIPOLE_t
 use parallel_m,   ONLY:PAR_IND_WF_b_and_k,PAR_IND_T_ordered,PAR_IND_T_groups,&
&                       PAR_IND_T_Haydock,PP_indexes_reset
 use electrons,    ONLY:levels,n_sp_pol,n_full_bands,n_met_bands
 use com,          ONLY:msg
 use R_lattice,    ONLY:qindx_X,bz_samp
 use BS,           ONLY:l_BS_magnons,l_BS_photolum
#if defined _RT
 use drivers,      ONLY:l_rt_carriers_in_use
 use RT_control,   ONLY:EQ_Transitions,EQ_NoOcc
#endif
 !
#include<memory.h>
 !
 character(1)   :: mode ! "K" for k-resolved, "T" for transitions groups
 type(levels)   :: Ken
 type(bz_samp)  :: Xk
 type(DIPOLE_t) :: Dip
 integer        :: iq
 !
 ! Work Space
 !
 character(1) :: sec
 real(SP)     :: E_eh,E_eh_SOC_corr(2/n_sp_pol),f_eh,f_eh_RES,f_eh_PL
 logical      :: l_trans_in_group,l_grp_to_grp
 integer      :: ikbz,ikpbz,iv,ic,ik,ikp,i_sp_v,i_sp_c,i_star,ib1,ib2,isp1,isp2,  &
&                i_T_now,i_T_group_now,i_T_group_last,i_T_group_start,i_g,n_loops,i_loop,i_T_in_the_grp,i_res_ares,&
&                BS_nT_at_k_tmp(Xk%nibz*2),V_bands(2),C_bands(2)
 !
 logical, external :: K_Transition_check
 !
 if (isec(3)/=0) sec="-"
 if (isec(3)==0) sec="+"
 !
 if (mode=="K") call section(sec,'Transition pre-analysis @q'//trim(intc(iq)))
 if (mode=="T") call section(sec,'Transition Groups build-up @q'//trim(intc(iq)))
 !
 call k_build_up_BZ_tables(Xk)
 !
 ! BS_nT_grps is first computed in PARALLEL_Transition_grouping
 !
 if (mode=="T") then
   allocate(BS_T_grp(BS_nT_grps))
   do i_g=1,BS_nT_grps
     BS_T_grp(i_g)%size=0
   enddo
   n_loops=2
   call PP_indexes_reset(PAR_IND_WF_b_and_k)
   YAMBO_ALLOC(PAR_IND_WF_b_and_k%element_2D,(BS_bands(2),Xk%nibz))
   PAR_IND_WF_b_and_k%element_2D=.FALSE.
 else
   BS_K_dim=0
   BS_nT_at_k_tmp=0
   n_loops=1
 endif
 !
 V_bands=(/BS_bands(1),maxval(Ken%nbm)/)
 C_bands=(/minval(Ken%nbf)+1,BS_bands(2)/)
#if defined _RT
 ! Bands range. In the _RT case %nbf and %nbm turn metallic when carriers are used.
 if (l_rt_carriers_in_use.and.EQ_Transitions) then
   V_bands=(/BS_bands(1),maxval(n_met_bands)/)
   C_bands=(/minval(n_full_bands)+1,BS_bands(2)/)
 endif
 if (EQ_NoOcc) then
   V_bands=BS_bands
   C_bands=BS_bands
 endif
#endif
 !
 do i_loop=1,n_loops
   !
   if (mode=="T") then
     i_T_now=0
     i_T_group_now=0
     i_T_group_last=0
   endif
   !
   ! DS: WARNING
   ! I cannot revert ik and i_sp_pol loop here ...
   !
   do i_res_ares=1,BS_n_eh_spaces
     !
     if (mode=="T") i_T_group_start=i_T_group_now+1
     !
     do ik=1,Xk%nibz
       !
       ! Here I cannot invert the ik and the i_sp loop since 
       ! BS_T_grp and BS_nT_at_k are defined according to kpts while they
       ! should be defined according to both i_sp_pol and kpts
       !
       ! Moreover I cannot move iv,ic loops insiede i_sp_pol and i_star loops
       ! Otherwise runs with parallelization over eh fail
       !
       do iv=V_bands(1),V_bands(2)
         do ic=C_bands(1),C_bands(2)
           !
           do i_sp_c=1,n_sp_pol
             i_sp_v=i_sp_c
             !
             if (l_BS_magnons.and.n_sp_pol==2) then
               if(i_res_ares==1.and.i_sp_c==1) cycle
               if(i_res_ares==2.and.i_sp_c==2) cycle
               i_sp_v=mod(i_sp_c,n_sp_pol)+1
             endif
             !
             do i_star=1,Xk%nstar(ik)
               !
               ikbz=Xk%k_table(ik,Xk%star(ik,i_star))
               !
               ikpbz=qindx_X(iq,ikbz,1)
               ikp  =Xk%sstar(ikpbz,1)
               !
               if(i_res_ares==1) then; ib1=iv; ib2=ic; isp1=i_sp_v; isp2=i_sp_c; endif
               if(i_res_ares==2) then; ib1=ic; ib2=iv; isp1=i_sp_c; isp2=i_sp_v; endif
               !
               if (.not.K_Transition_check(Ken,Dip,iq,ik,ikp,ib1,ib2,isp2,i_res_ares,&
&                                          E_eh,E_eh_SOC_corr,f_eh,f_eh_RES,f_eh_PL)) cycle
               !
               if (mode=="K") then
                 BS_K_dim(i_res_ares)=BS_K_dim(i_res_ares)+1
                 BS_nT_at_k_tmp(ik+(i_res_ares-1)*Xk%nibz)=BS_nT_at_k_tmp(ik+(i_res_ares-1)*Xk%nibz)+1
                 cycle
               endif
               !
               i_T_now=i_T_now+1
               i_T_group_now=T_group(i_T_now)
               !
               if (i_loop==1) BS_T_grp(i_T_group_now)%size=BS_T_grp(i_T_group_now)%size+1
               !
               l_trans_in_group=PAR_IND_T_groups%element_1D(i_T_group_now)
               !
               l_grp_to_grp= any( PAR_IND_T_ordered%element_2D(i_T_group_now,:)) .or. &
&                            any( PAR_IND_T_ordered%element_2D(:,i_T_group_now))
               !
               if ( .not. (l_trans_in_group .or. l_grp_to_grp) ) cycle
               !
               if (i_T_group_now/=i_T_group_last) then
                 !
                 i_T_group_last=i_T_group_now
                 i_T_in_the_grp=0
                 !
                 if (i_loop==2) then
                   !
                   call BS_Blocks_and_Transitions_alloc(Ken,i_T_group_now)
                   !
                   BS_T_grp(i_T_group_now)%i_T_ref   =i_T_group_start 
                   BS_T_grp(i_T_group_now)%i_res_ares=i_res_ares
                   BS_T_grp(i_T_group_now)%ik        =ik
                   BS_T_grp(i_T_group_now)%first_cv  =i_T_now
                   !
                 endif
                 !
               endif
               !
               if (i_loop==1) cycle
               !
               i_T_in_the_grp=i_T_in_the_grp+1
               !
               BS_T_grp(i_T_group_now)%table(i_T_in_the_grp,:)=(/ikbz,ib1,ib2,isp2,isp1/)
               !
               PAR_IND_WF_b_and_k%element_2D(ib2,ik )=.TRUE.
               PAR_IND_WF_b_and_k%element_2D(ib1,ikp)=.TRUE.
               !
               if ((index(BSS_mode,'h')/=0) .or. (index(BSS_mode,'s')/=0)) then
                  PAR_IND_T_Haydock%element_1D(i_T_group_now)=.TRUE.
               endif
               !
               BS_T_grp(i_T_group_now)%E(i_T_in_the_grp) =E_eh
               BS_T_grp(i_T_group_now)%f(i_T_in_the_grp) =f_eh
               !
               ! To be fixed
               if (l_BS_photolum) BS_T_grp(i_T_group_now)%f_PL(i_T_in_the_grp,1) =f_eh_PL
               !
               BS_T_grp(i_T_group_now)%f_RES(i_T_in_the_grp)=f_eh_RES
               !
               if (allocated(Ken%Z)) BS_T_grp(i_T_group_now)%Z(i_T_in_the_grp)=&
&                                                            Ken%Z(ib2,ik,isp2)*Ken%Z(ib1,ikp,isp1)
               !
               ! Note that W is negative as the linewidth is negative(positive) above(below) the Fermi level.
               ! Presently there is the abs value here and a minus in K_IP.F
               !
               if (allocated(Ken%W)) BS_T_grp(i_T_group_now)%W(i_T_in_the_grp)= &
&                                             (-1._SP)**(i_res_ares-1)*(abs(Ken%W(ib2,ik,isp2))+abs(Ken%W(ib1,ikp,isp1)))
               !
               if ( BS_perturbative_SOC ) BS_T_grp(i_T_group_now)%E_SOC_corr(:,i_T_in_the_grp)=E_eh_SOC_corr(:)
               !
             enddo ! conduction 
           enddo ! valence
           !
         enddo ! symmetries
       enddo ! spin 
     enddo ! k (ibz)
     !
   enddo ! i_res_ares
   !
 enddo ! loop
 !
 ! REPORT
 !
 if (mode=="T") then
   call msg('r','[BSK] Transition groups (total)',BS_nT_grps)
   return
 endif
 !
 YAMBO_ALLOC(BS_nT_at_k,(Xk%nibz*BS_n_eh_spaces))
 BS_nT_at_k=BS_nT_at_k_tmp(:Xk%nibz*BS_n_eh_spaces)
 !
 if (any(BS_nT_at_k==0)) call warning(' Null BSE kernel block dimension(s) found.')
 if (sum(BS_nT_at_k)==0) call   error(' The whole BSE has zero dimension !')
 !
 BS_H_dim=BS_K_dim(1)
 if (l_BS_ares_from_res) then
   if (BS_K_coupling) BS_H_dim=2*BS_K_dim(1)
 else
   if (BS_res_ares_n_mat==2.or.BS_K_coupling) BS_H_dim=BS_K_dim(1)+BS_K_dim(2)
 endif
 !
end subroutine
