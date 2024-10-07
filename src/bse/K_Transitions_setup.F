!
! License-Identifier: GPL
!
! Copyright (C) 2014 The Yambo Team
!
! Authors (see AUTHORS file for details): AM DS IMA
!
subroutine K_Transitions_setup(iq,Ken,Xk,Dip,V_bands,C_bands)
 !
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
 use BS_solvers,   ONLY:BSS_mode,BSS_uses_DbGd
 use BS,           ONLY:T_group,BS_bands,BS_T_grp,BS_nT_grps,BS_n_eh_spaces,&
&                       BS_Blocks_and_Transitions_alloc,BS_perturbative_SOC,&
&                       BS_nkFGbz_in_Tgrp
 use DIPOLES,      ONLY:DIPOLE_t
 use parallel_m,   ONLY:PAR_IND_WF_b_and_k,PAR_IND_T_ordered,PAR_IND_T_groups,&
&                       PAR_IND_T_Haydock,PP_indexes_reset
 use electrons,    ONLY:levels,n_sp_pol
 use com,          ONLY:msg
 use R_lattice,    ONLY:qindx_X,bz_samp
 use BS,           ONLY:l_BS_magnons,l_BS_photolum
#if defined _RT
 use electrons,    ONLY:n_full_bands,n_met_bands
 use drivers,      ONLY:l_rt_carriers_in_use
 use RT_control,   ONLY:EQ_Transitions,EQ_NoOcc
#endif
 !
#include<memory.h>
 !
 type(levels),    intent(in)   :: Ken
 type(bz_samp),   intent(in) :: Xk
 type(DIPOLE_t),  intent(in) :: Dip
 integer,         intent(in) :: iq
 integer,         intent(in) :: V_bands(2),C_bands(2)
 !
 ! Work Space
 !
 real(SP)     :: E_eh,E_eh_SOC_corr(2/n_sp_pol),f_eh,f_eh_RES,f_eh_PL
 logical      :: l_trans_in_group,l_grp_to_grp
 integer      :: ikbz,ikpbz,iv,ic,ik,ikp,i_sp_v,i_sp_c,i_star,ib1,ib2,  &
&                i_T_now,i_T_group_now,i_T_group_last,i_T_group_start,i_g,n_loops,i_loop,i_T_in_the_grp,i_res_ares,&
&                nColFG_in_T_group_now,i_ColFG,ikFGbz,ikFG
 !
 logical, external :: K_Transition_check
 !
 call section("=",'Transition Groups build-up @q'//trim(intc(iq)))
 !
 ! BS_nT_grps is computed in PARALLEL_Transition_grouping
 !
 call msg('r','[BSK] Transition groups (total)',BS_nT_grps)
 !
 allocate(BS_T_grp(BS_nT_grps))
 do i_g=1,BS_nT_grps
   BS_T_grp(i_g)%size=0
 enddo
 n_loops=2
 !
 allocate(BS_nkFGbz_in_Tgrp(BS_nT_grps))
 !
 call PP_indexes_reset(PAR_IND_WF_b_and_k)
 YAMBO_ALLOC(PAR_IND_WF_b_and_k%element_2D,(BS_bands(2),Xk%nibz))
 PAR_IND_WF_b_and_k%element_2D=.FALSE.
 !
 do i_loop=1,n_loops
   !
   i_T_now=0
   i_T_group_now=0
   i_T_group_last=0
   !
   nColFG_in_T_group_now=1
   !
   ! DS: WARNING
   ! I cannot revert ik and i_sp_pol loop here ...
   !
   do i_res_ares=1,BS_n_eh_spaces
     !
     i_T_group_start=i_T_group_now+1
     !
     ! Here I cannot invert the ik and the i_sp loop since 
     ! BS_T_grp and BS_nT_at_k are defined according to kpts while they
     ! should be defined according to both i_sp_pol and kpts
     !
     do ik=1,Xk%nibz
       !
       if (BSS_uses_DbGd .and. (index(BSS_mode,'h')/=0)) then
         nColFG_in_T_group_now = maxval(Xk%FGbz%N_mapped(&
&                                Xk%k_table(ik,Xk%star(ik,:Xk%nstar(ik)))))
       end if
       !
       ! Moreover I cannot move iv,ic loops insiede i_sp_pol and i_star loops
       ! Otherwise runs with parallelization over eh fail
       ! Having i_star inside the loops is likely what makes the distribution
       ! of the algorithm with the qindx_B distributed in memory very slow
       ! since i_k_bz and i_p_bz change at each iteration of the loop
       !
       do iv=V_bands(1),V_bands(2)
         do ic=C_bands(1),C_bands(2)
           !
           do i_sp_c=1,n_sp_pol
             i_sp_v=i_sp_c
             !
             if (l_BS_magnons.and.n_sp_pol==2) then
               if(i_sp_c==1) cycle
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
               if (i_res_ares==1) then; ib1=iv; ib2=ic; endif
               if (i_res_ares==2) then; ib1=ic; ib2=iv; endif
               !
               if (.not.K_Transition_check(Ken,Dip,iq,ik,ikp,ib1,ib2,i_sp_c,i_res_ares,&
&                                          E_eh,E_eh_SOC_corr,f_eh,f_eh_RES,f_eh_PL)) cycle
               !
               i_T_now=i_T_now+1
               i_T_group_now=T_group(i_T_now)
               !
               if (i_loop==1) BS_T_grp(i_T_group_now)%size=BS_T_grp(i_T_group_now)%size+1
               !
               if (i_loop==2) BS_nkFGbz_in_Tgrp(i_T_group_now) = nColFG_in_T_group_now
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
                   call BS_Blocks_and_Transitions_alloc(Ken,i_T_group_now,nColFG_in_T_group_now)
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
               BS_T_grp(i_T_group_now)%table(i_T_in_the_grp,:)=(/ikbz,ib1,ib2,i_sp_c,i_sp_v/)
               !
               PAR_IND_WF_b_and_k%element_2D(ib2,ik )=.TRUE.
               PAR_IND_WF_b_and_k%element_2D(ib1,ikp)=.TRUE.
               !
               if ((index(BSS_mode,'h')/=0) .or. (index(BSS_mode,'s')/=0)) then
                  PAR_IND_T_Haydock%element_1D(i_T_group_now)=.TRUE.
               endif
               !
               BS_T_grp(i_T_group_now)%E(i_T_in_the_grp,1) =E_eh
               BS_T_grp(i_T_group_now)%f(i_T_in_the_grp) =f_eh
               !
               ! IAM (2022/01/19):
               ! Transition energies in fine grid only for Haydock + DbGd case
               !
               if ( BSS_uses_DbGd  .and. (index(BSS_mode,'h')/=0) ) then
                 if( Xk%FGbz%N_mapped(ikbz)>0 ) then
                   do ikFGbz=Xk%FGbz%k_range(ikbz,1)+1,Xk%FGbz%k_range(ikbz,2)
                     ikFG = Xk%FGbz%E_map(ikFGbz)
                     i_ColFG = ikFGbz - Xk%FGbz%k_range(ikbz,1) + 1
                     BS_T_grp(i_T_group_now)%E(i_T_in_the_grp,i_ColFG) = Ken%FG%E(ic,ikFG,1) - Ken%FG%E(iv,ikFG,1)
                   enddo
                 endif
               end if
               !
               ! To be fixed
               if (l_BS_photolum) BS_T_grp(i_T_group_now)%f_PL(i_T_in_the_grp,i_res_ares) =f_eh_PL
               !
               BS_T_grp(i_T_group_now)%f_RES(i_T_in_the_grp)=f_eh_RES
               !
               if (allocated(Ken%Z)) BS_T_grp(i_T_group_now)%Z(i_T_in_the_grp)=&
               &                      Ken%Z(ib2,ik,i_sp_c)*Ken%Z(ib1,ikp,i_sp_v)
               !
               ! Note that W is negative as the linewidth is negative(positive) above(below) the Fermi level.
               ! Presently there is the abs value here and a minus in K_IP.F
               !
               if (allocated(Ken%W)) BS_T_grp(i_T_group_now)%W(i_T_in_the_grp)= &
               &  (-1._SP)**(i_res_ares-1)*(abs(Ken%W(ib2,ik,i_sp_c))+abs(Ken%W(ib1,ikp,i_sp_v)))
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
end subroutine K_Transitions_setup
!
!
subroutine K_Transitions_dimensions(iq,Ken,Xk,Dip,V_bands,C_bands)
 !
 ! Out: 
 !
 !   BS_K_dim(1)
 !   BS_H_dim
 !   BS_nT_at_k
 !
 use pars,         ONLY:SP
 use stderr,       ONLY:intc
 use BS,           ONLY:BS_K_dim,BS_H_dim,BS_nT_at_k,BS_K_coupling,   &
&                       l_BS_ares_from_res,BS_res_ares_n_mat,BS_bands,&
&                       BS_n_eh_spaces
 use DIPOLES,      ONLY:DIPOLE_t
 use electrons,    ONLY:levels,n_sp_pol
 use com,          ONLY:msg
 use R_lattice,    ONLY:qindx_X,bz_samp
 use BS,           ONLY:l_BS_magnons
#if defined _RT
 use electrons,    ONLY:n_full_bands,n_met_bands
 use drivers,      ONLY:l_rt_carriers_in_use
 use RT_control,   ONLY:EQ_Transitions,EQ_NoOcc
#endif
 !
#include<memory.h>
 !
 type(levels),    intent(in) :: Ken
 type(bz_samp),   intent(in) :: Xk
 type(DIPOLE_t),  intent(in) :: Dip
 integer,         intent(in) :: iq
 integer,         intent(out):: V_bands(2),C_bands(2)
 !
 ! Work Space
 !
 real(SP)     :: E_eh,E_eh_SOC_corr(2/n_sp_pol),f_eh,f_eh_RES,f_eh_PL
 integer      :: ikbz,ikpbz,iv,ic,ik,ikp,i_sp_v,i_sp_c,i_star,ib1,ib2,  &
&                i_res_ares,BS_nT_at_k_tmp(Xk%nibz*2)
 !
 logical, external :: K_Transition_check
 !
 !
 call section("+P",'Transition pre-analysis @q'//trim(intc(iq)))
 !
 call k_build_up_BZ_tables(Xk)
 !  
 BS_K_dim=0
 BS_nT_at_k_tmp=0
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
 do i_res_ares=1,BS_n_eh_spaces
   !
   do i_sp_c=1,n_sp_pol
     !
     i_sp_v=i_sp_c
     !
     if (l_BS_magnons.and.n_sp_pol==2) then
       if(i_sp_c==1) cycle
       i_sp_v=mod(i_sp_c,n_sp_pol)+1
     endif
     !
     do ik=1,Xk%nibz
       do i_star=1,Xk%nstar(ik)
         !
         ikbz=Xk%k_table(ik,Xk%star(ik,i_star))
         ikpbz=qindx_X(iq,ikbz,1)
         ikp  =Xk%sstar(ikpbz,1)
         !
         do iv=V_bands(1),V_bands(2)
           do ic=C_bands(1),C_bands(2)
             !
             if (i_res_ares==1) then; ib1=iv; ib2=ic; endif
             if (i_res_ares==2) then; ib1=ic; ib2=iv; endif
             !
             if (.not.K_Transition_check(Ken,Dip,iq,ik,ikp,ib1,ib2,i_sp_c,i_res_ares,&
             &                            E_eh,E_eh_SOC_corr,f_eh,f_eh_RES,f_eh_PL)) cycle
             !
             BS_K_dim(i_res_ares)=BS_K_dim(i_res_ares)+1
             BS_nT_at_k_tmp(ik+(i_res_ares-1)*Xk%nibz)=BS_nT_at_k_tmp(ik+(i_res_ares-1)*Xk%nibz)+1
             !
           enddo ! conduction 
         enddo ! valence
         !
       enddo ! symmetries
     enddo ! k (ibz)
   enddo ! spin 
   !
 enddo ! i_res_ares
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
end subroutine K_Transitions_dimensions
