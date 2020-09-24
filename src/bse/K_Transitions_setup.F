!
!        Copyright (C) 2000-2020 the YAMBO team
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
subroutine K_Transitions_setup(iq,Ken,Xk,Dip,FILL_the_groups)
 !
 ! Out: 
 !
 !   BS_K_dim
 !   BS_H_dim
 !   BS_nT_at_k
 !
 ! Filling (FILL_the_groups=.TRUE.) of Transition groups
 !
 !type BS_T_group
 !  integer                       :: ik         ! ik index (\in IBZ)
 !  integer                       :: first_cv   ! Position in the cv transitions @ k
 !  integer                       :: size       
 !  complex(SP), allocatable      :: table(:,:) ! (/ikbz,iv,ic,i_sp_c,i_sp_v/)
 !  complex(SP), allocatable      :: Z(size)
 !  real(SP)   , allocatable      :: E(size)
 !  real(SP)   , allocatable      :: f(size)
 !  real(SP)   , allocatable      :: W(size)
 !end type BS_T_group
 !
 use pars,         ONLY:SP
 use stderr,       ONLY:intc
 use com,          ONLY:isec
 use BS_solvers,   ONLY:BSS_add_antiresonant,BSS_mode
 use BS,           ONLY:BS_K_dim,BS_H_dim,T_group,BS_eh_en,BS_K_coupling,&
&                       BS_bands,BS_nT_at_k,BSE_mode,BS_T_grp,BS_nT_grps,&
&                       BS_not_const_eh_f,BS_Blocks_and_Transitions_alloc,&
&                       BS_perturbative_SOC
 use DIPOLES,      ONLY:DIPOLE_t
 use X_m,          ONLY:global_gauge
 use parallel_m,   ONLY:PAR_IND_WF_b_and_k,PAR_IND_T_ordered,PAR_IND_T_groups,&
&                       PAR_IND_T_Haydock
 use electrons,    ONLY:levels,n_sp_pol,spin_occ,filled_tresh
 use com,          ONLY:msg
 use R_lattice,    ONLY:qindx_X,bz_samp
#if defined _KERR
 use BS,           ONLY:l_BS_magnons
#endif
#if defined _PL
 use drivers,      ONLY:l_photolum
#endif
 !
 ! GPL_EXCLUDE_START
 !
#if defined _RT
 use parser_m,     ONLY:parser
 use drivers,      ONLY:l_rt_carriers_in_use
 use RT_control,   ONLY:NEQ_Residuals,NEQ_Kernel
#endif
 !
 ! GPL_EXCLUDE_END
 !
#include<memory.h>
 !
 type(levels) :: Ken
 type(bz_samp):: Xk
 type(DIPOLE_t):: Dip
 integer      :: iq
 logical      :: FILL_the_groups,l_trans_in_group,l_grp_to_grp
 !
 ! Work Space
 !
 character(1) :: sec
 integer :: ikbz,ikpbz,iv,ic,iv_SOC(2/n_sp_pol),ic_SOC(2/n_sp_pol),ik,ikp,i_sp_c,i_sp_v,i_star,nb(2),&
&           i_T_group_now,i_T_group_last,i_g,n_loops,i_loop,i_T_in_the_grp,pert_range(2/n_sp_pol,2)
 real(SP):: E_eh,Eo_eh,E_eh_SOC_corr(2/n_sp_pol),E_v_SOC(2/n_sp_pol),E_c_SOC(2/n_sp_pol),f_eh,f_eh_RES,f_eh_REF,f_e,f_h,f_check
 integer, allocatable :: i_T_at_k(:)
#if defined _PL
 real(SP):: f_eh_PL(2)
#endif
 !
 ! GPL_EXCLUDE_START
 !
#if defined _RT
 logical :: FORCE_EQ_trans
 !
 call parser('ForceEqTrans',FORCE_EQ_trans)
 ! 
#endif
 !
 ! GPL_EXCLUDE_END
 !
 if (FILL_the_groups) then
   !
   if (isec(3)/=0) sec="-"
   if (isec(3)==0) sec="+"
   !
   call section(sec,'Transition Groups build-up @q'//trim(intc(iq)))
   !
   allocate(BS_T_grp(BS_nT_grps))
   do i_g=1,BS_nT_grps
     BS_T_grp(i_g)%size=0
   enddo
   n_loops=2
   if (any(BS_nT_at_k==0)) call warning(' Null BSE kernel block dimension(s) found.')
   !
   YAMBO_ALLOC(PAR_IND_WF_b_and_k%element_2D,(BS_bands(2),Xk%nibz))
   PAR_IND_WF_b_and_k%element_2D=.FALSE.
   !
   if ((index(BSS_mode,'h')/=0) .or. (index(BSS_mode,'s')/=0)) then
     YAMBO_ALLOC(PAR_IND_T_Haydock%element_1D,(BS_nT_grps))
     PAR_IND_T_Haydock%element_1D=.FALSE.
   endif
   !
 else
   !
   YAMBO_ALLOC(BS_nT_at_k,(Xk%nibz))
   BS_nT_at_k=0
   n_loops=1
   !
 endif 
 !
 YAMBO_ALLOC(i_T_at_k,(Xk%nibz))
 !
 call k_build_up_BZ_tables(Xk)
 !
 nb(1)=Ken%nbf+1
 nb(2)=Ken%nbm
!#if defined _RT
! if(all(Ken%nbc/=0)) nb=Ken%nbc
!#endif
 !
 do i_loop=1,n_loops
   !
   if (i_loop==2) call BS_Blocks_and_Transitions_alloc(Ken,0)
   !
   i_T_at_k=0
   BS_K_dim=0
   i_T_group_now=0
   i_T_group_last=0
   !
   do ik=1,Xk%nibz
     !
     do iv=BS_bands(1),nb(2)
       do ic=nb(1),BS_bands(2)
         !
         do i_star=1,Xk%nstar(ik)
           !
           ikbz=Xk%k_table(ik,Xk%star(ik,i_star))
           !
           ikpbz=qindx_X(iq,ikbz,1)
           ikp  =Xk%sstar(ikpbz,1)
           !
           do i_sp_c=1,n_sp_pol
             !
             if (.not.T_is_ok()) cycle
             !
             BS_K_dim=BS_K_dim+1
             i_T_at_k(ik)=i_T_at_k(ik)+1
             !
             if (.not. FILL_the_groups) then
               BS_nT_at_k(ik)=BS_nT_at_k(ik)+1
               cycle
             endif
             !
             i_T_group_now=T_group(BS_K_dim)
             !
             if (i_loop==1) BS_T_grp(i_T_group_now)%size=BS_T_grp(i_T_group_now)%size+1
             !
             l_trans_in_group=PAR_IND_T_groups%element_1D(i_T_group_now)
             !
             l_grp_to_grp= any( PAR_IND_T_ordered%element_2D(i_T_group_now,:)) .or. &
&                          any( PAR_IND_T_ordered%element_2D(:,i_T_group_now))
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
                 BS_T_grp(i_T_group_now)%ik      =ik
                 BS_T_grp(i_T_group_now)%first_cv=BS_K_dim
                 !
               endif
               !
             endif
             !
             if (i_loop==1) cycle
             !
             i_T_in_the_grp=i_T_in_the_grp+1
             !
             BS_T_grp(i_T_group_now)%table(i_T_in_the_grp,:)=(/ikbz,iv,ic,i_sp_c,i_sp_v/)
             !
             PAR_IND_WF_b_and_k%element_2D(ic,ik )=.TRUE.
             PAR_IND_WF_b_and_k%element_2D(iv,ikp)=.TRUE.
             !
             if ((index(BSS_mode,'h')/=0) .or. (index(BSS_mode,'s')/=0)) then
                PAR_IND_T_Haydock%element_1D(i_T_group_now)=.TRUE.
             endif
             !
             ! Energies
             !
             BS_T_grp(i_T_group_now)%E(i_T_in_the_grp) =E_eh
             !
             ! Occupations
             !
             BS_T_grp(i_T_group_now)%f(i_T_in_the_grp) =f_eh
#if defined _PL
             if (l_photolum) BS_T_grp(i_T_group_now)%f_PL(:,i_T_in_the_grp) =f_eh_PL
#endif
             BS_T_grp(i_T_group_now)%f_RES(i_T_in_the_grp)=f_eh_RES
             !
             if (allocated(Ken%Z)) BS_T_grp(i_T_group_now)%Z(i_T_in_the_grp)=Ken%Z(ic,ik,i_sp_c)*Ken%Z(iv,ikp,i_sp_v)
             !
             ! Note that W is negative as the linewidth is negative(positive) above(below) the Fermi level.
             ! Presently there is the abs value here and a minus in K_IP.F
             !
             if (allocated(Ken%W)) BS_T_grp(i_T_group_now)%W(i_T_in_the_grp)=abs(Ken%W(ic,ik,i_sp_c))+abs(Ken%W(iv,ikp,i_sp_v))
             !
             if ( BS_perturbative_SOC ) BS_T_grp(i_T_group_now)%E_SOC_corr(:,i_T_in_the_grp)=E_eh_SOC_corr(:)
             !
           enddo ! spin
         enddo ! symmetries
       enddo ! k (ibz)
       !
     enddo ! conduction
   enddo ! valence
   !
 enddo ! loop
 !
 BS_H_dim=BS_K_dim
 !
 if (BS_K_coupling) BS_H_dim=2*BS_K_dim
 !
 if (.not.FILL_the_groups) then
   !
   if (any(BS_nT_at_k==0)) call warning(' Null BSE kernel block dimension(s) found.')
   if (sum(BS_nT_at_k)==0) call   error(' The whole BSE has zero dimension !')
   !
   if (.not.BSS_add_antiresonant.and.BS_not_const_eh_f.and..not.BS_K_coupling) then
     call warning('Fractional e/h occupations. Retarded BSEmode forced.')
     BSE_mode='retarded'
     BSS_add_antiresonant=.true.
   endif
   !
 endif
 !
 ! REPORT
 !
 if (FILL_the_groups) call msg('r','[BSK] Transition groups (total)',BS_nT_grps)
 !
 ! CLEAN UP
 !
 YAMBO_FREE(i_T_at_k)
 !
 contains
   !
   logical function T_is_ok( )
     !
     T_is_ok=.TRUE.
     !
     i_sp_v=i_sp_c
#if defined _KERR
     if(l_BS_magnons.and.n_sp_pol==2) then
       if (i_sp_c==1) i_sp_v=2
       if (i_sp_c==2) i_sp_v=1
     endif   
#endif
     !
     E_eh= Ken%E(ic,ik ,i_sp_c)-Ken%E(iv,ikp,i_sp_v)
     if (     allocated(Ken%Eo)) Eo_eh= Ken%Eo(ic,ik ,i_sp_c)-Ken%Eo(iv,ikp,i_sp_v)
     if (.not.allocated(Ken%Eo)) Eo_eh= E_eh
     !
     ! Small energy transitions. Dipoles in length gauge are not defined
     !if ( abs(Eo_eh) < Dip%Energy_treshold .and. trim(global_gauge)=="length" .and. iq==1 ) T_is_ok=.FALSE.
     !
     if( BS_perturbative_SOC ) then
       if(n_sp_pol==1) pert_range=reshape((/1,2,1,2/),(/2,2/))
       if(n_sp_pol==2) pert_range=reshape((/i_sp_c,1 /),(/1,2/))
       iv_SOC(pert_range(:,2))=Ken%table_SOC(pert_range(:,1),iv,ikp)
       ic_SOC(pert_range(:,2))=Ken%table_SOC(pert_range(:,1),ic,ik )
       if(     all(iv_SOC/=0)) E_v_SOC(:)=Ken%E_SOC(iv_SOC(:),ikp)
       if(     all(ic_SOC/=0)) E_c_SOC(:)=Ken%E_SOC(ic_SOC(:),ik )
       if(.not.all(iv_SOC/=0)) E_v_SOC(:)=Ken%E(iv,ikp,i_sp_v)
       if(.not.all(ic_SOC/=0)) E_c_SOC(:)=Ken%E(ic,ik ,i_sp_c)
       E_eh_SOC_corr(:)=E_c_SOC(:)-E_v_SOC(:)-Eo_eh
     endif
     !
     ! Occupations Factors
     !=====================
     !
     f_eh_REF =(Ken%f(iv,ikp,i_sp_v)-Ken%f(ic,ik ,i_sp_c))/spin_occ
     f_eh     =f_eh_REF
     f_eh_RES =f_eh_REF
     f_check  =f_eh_REF
     !
     ! f_eh  enter the excitonic Hamiltonian
     ! f_eh_RES enter the residual of the dielectric constant
     !
     ! GPL_EXCLUDE_START
     !
#if defined _RT
     if (l_rt_carriers_in_use.and.allocated(Ken%fo)) then
       if (     FORCE_EQ_trans) f_eh_REF=(Ken%fo(iv,ikp,i_sp_v)-Ken%fo(ic,ik ,i_sp_c))/spin_occ
       if (.not.FORCE_EQ_trans) f_eh_REF=(Ken%f (iv,ikp,i_sp_v)-Ken%f (ic,ik ,i_sp_c))/spin_occ
       if (     NEQ_Kernel    ) f_eh =(Ken%f (iv,ikp,i_sp_v)-Ken%f (ic,ik ,i_sp_c))/spin_occ
       if (.not.NEQ_Kernel    ) f_eh =(Ken%fo(iv,ikp,i_sp_v)-Ken%fo(ic,ik ,i_sp_c))/spin_occ
       if (     NEQ_Residuals ) f_eh_RES=(Ken%f (iv,ikp,i_sp_v)-Ken%f (ic,ik ,i_sp_c))/spin_occ
       if (.not.NEQ_Residuals ) f_eh_RES=(Ken%fo(iv,ikp,i_sp_v)-Ken%fo(ic,ik ,i_sp_c))/spin_occ
     endif
#endif
     !
     ! GPL_EXCLUDE_END
     !
     f_e = (         Ken%f(iv,ikp,i_sp_v))/spin_occ
     f_h = (spin_occ-Ken%f(ic,ik ,i_sp_c))/spin_occ
     !
#if defined _PL
     !
     ! Resonant
     f_eh_PL(1) = Ken%f(ic,ikp,i_sp_c)*(spin_occ-Ken%f(iv,ik ,i_sp_v))/spin_occ/spin_occ
     !
     ! Anti-Resonant
     f_eh_PL(2) = Ken%f(iv,ik ,i_sp_v)*(spin_occ-Ken%f(ic,ikp,i_sp_c))/spin_occ/spin_occ
     !
#endif
     !
     ! A 0.001 % fractional occupation swiches on the particular treatment of finite temp. BSE 
     if ( abs(abs(f_eh_REF)-1._SP)> filled_tresh ) BS_not_const_eh_f=.TRUE.
     !
     ! f_eh<0 transitions are included in the anti-resonant part
     ! f_eh=0 transitions give no contribution to absorption and are not considered
     if ( f_check<=0._SP ) T_is_ok=.FALSE.
     !
     ! GPL_EXCLUDE_START
     !
#if defined _RT
     !
     ! In case of population inversion, it may be problematic if f_eh>0. and f_eh_RES<0.
     if ( f_eh_REF*f_eh<0._SP .or. f_eh_REF*f_eh_RES<0._SP ) &
&      call error(' I cannot split the R and K BSE contribution.'//&
&                 ' ACTION: Use the same occupations in K and R.')
     !
#endif
     !
#if defined _RT
     if (.not.l_rt_carriers_in_use) then
#endif
       !
       ! GPL_EXCLUDE_END
       !
       ! If I've included in valence states that are almost empty
       ! or in conduction states that are almost full, those are neglected
       if ( (f_e<=filled_tresh) .or. (f_h<=filled_tresh) ) T_is_ok=.FALSE.
       !
       ! GPL_EXCLUDE_START
       !
#if defined _RT
     endif
#endif
     !
     ! GPL_EXCLUDE_END
     !
     if (any((/abs(E_eh)<BS_eh_en(1).and.BS_eh_en(1)>0._SP,&
&              abs(E_eh)>BS_eh_en(2).and.BS_eh_en(2)>0._SP/))) T_is_ok=.FALSE.
     !
   end function
   !
end subroutine
