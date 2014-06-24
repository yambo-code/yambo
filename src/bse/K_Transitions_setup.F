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
subroutine K_Transitions_setup(iq,Ken,Xk,Wd,FILL_the_groups)
 !
 ! Out: 
 !
 !   BS_K_dim
 !
 !   PAR_BS_T_grps_index(i_Group_par) = i_Group_global ! with i_Group_par \in PAR_BS_nT_grps and
 !                                                            i_Group_global \in BS_nT_grps
 !
 !
 ! Filling (FILL_the_groups=.TRUE.) of Transition groups
 !
 !type BS_T_group
 !  integer                       :: ik         ! ik index
 !  integer                       :: first_cv   ! Position in the cv transitions @ k
 !  integer                       :: size       
 !  complex(SP), allocatable      :: table(:,:) ! (/iv,ic,i_sp/)
 !  complex(SP), allocatable      :: Z(size)
 !  real(SP)   , allocatable      :: E(size)
 !  real(SP)   , allocatable      :: f(size)
 !  real(SP)   , allocatable      :: W(size)
 !end type BS_T_group
 !
 use pars,         ONLY:SP,IP
 use memory_m,     ONLY:mem_est
 use com,          ONLY:warning,error
 use stderr,       ONLY:intc
 use BS,           ONLY:BS_K_dim,O_v_state,O_c_state,T_group,&
&                       O_n_v_states,O_n_c_states,BS_eh_en,&
&                       BS_bands,BS_nT_at_k,BS_anti_res,&
&                       BS_not_const_eh_f,BS_drude_f_eh,BS_Blocks_and_Transitions_alloc,&
&                       BSE_mode,BS_T_grp,BS_K_coupling,BS_nT_grps
 use parallel_m,   ONLY:PAR_BS_nT_grps,PAR_BS_T_grps_index,PAR_IND_Kk_ibz,PAR_IND_eh,&
&                       PAR_IND_WF_b_and_k,PAR_COM_eh_INDEX,myid
 use electrons,    ONLY:levels,n_sp_pol,spin_occ,filled_tresh,n_spin
 use vec_operate,  ONLY:i_bz
 use com,          ONLY:msg
 use R_lattice,    ONLY:qindx_X,bz_samp
 use interfaces,   ONLY:PARALLEL_live_message
 !
 implicit none
 type(levels) :: Ken
 type(bz_samp):: Xk
 complex(SP)  :: Wd
 integer      :: iq
 logical      :: FILL_the_groups
 !
 ! Work Space
 !
 integer :: Iic(BS_bands(2),n_sp_pol),Iiv(BS_bands(2),n_sp_pol),ikbz,iv,ic,ik,ikp,i_sp,&
&           i_T_group_now,i_T_group_last,i_g,n_loops,i_loop,i_PAR_nT_grps,i_T_in_the_grp,nT,&
&           i_PAR_nT_grps_mem,is
 real(SP):: E_eh,f_eh
 integer, allocatable :: i_T_at_k(:)
 !
 if (FILL_the_groups) call section('+','Transition Groups build-up')
 !
 if (FILL_the_groups) then
   !
   allocate(BS_T_grp(PAR_BS_nT_grps))
   allocate(PAR_BS_T_grps_index(PAR_BS_nT_grps))
   call mem_est("PAR_BS_T_grps_index",(/PAR_BS_nT_grps/),(/IP/))
   do i_g=1,PAR_BS_nT_grps
     PAR_BS_T_grps_index(i_g)=0
     BS_T_grp(i_g)%size=0
   enddo
   n_loops=2
   if (any(BS_nT_at_k==0)) then
     call error(' Null BSE kernel block dimension(s) found. Increase e/h range')
   endif
   !
   allocate(PAR_IND_WF_b_and_k%element_2D(BS_bands(2),Xk%nibz))
   PAR_IND_WF_b_and_k%element_2D=.FALSE.
   !
 else
   allocate(BS_nT_at_k(Xk%nibz))
   call mem_est("BS_nT_at_k",(/Xk%nibz/),(/IP/))
   BS_nT_at_k=0
   n_loops=1
 endif 
 !
 allocate(i_T_at_k(Xk%nibz))
 !
 do i_loop=1,n_loops
   !
   i_T_at_k=0
   BS_K_dim=0
   i_T_group_now=0
   i_T_group_last=0
   i_PAR_nT_grps=0
   !
   do iv=BS_bands(1),Ken%nbm
     do ic=Ken%nbf+1,BS_bands(2)
       !
       do ik=1,Xk%nibz
         !
         if (FILL_the_groups) then
           if (.not.PAR_IND_Kk_ibz%element_1D(ik)) cycle
           if (.not.PAR_IND_eh(ik)%element_1D(i_T_at_k(ik))) cycle
         endif
         !
         do is=1,Xk%nstar(ik)
           !
           ikbz=i_bz(ik,is,Xk)
           ikp =Xk%sstar(qindx_X(iq,ikbz,1),1)
           !
           do i_sp=1,n_sp_pol
             !
             if (.not.T_is_ok()) cycle
             !
             BS_K_dim=BS_K_dim+1
             i_T_at_k(ik)=i_T_at_k(ik)+1
             !
             if (FILL_the_groups) then
               !
               i_T_group_now=T_group(BS_K_dim)
               !
               if (i_T_group_now/=i_T_group_last) then
                 !
                 i_PAR_nT_grps=i_PAR_nT_grps+1
                 i_T_group_last=i_T_group_now
                 i_T_in_the_grp=0
                 !
                 if (i_loop==1) then
                   !
                   PAR_BS_T_grps_index(i_PAR_nT_grps)=i_T_group_now
                   !
                 else if (i_loop==2) then
                   !
                   call BS_Blocks_and_Transitions_alloc(Ken,i_PAR_nT_grps)
                   !
                   BS_T_grp(i_PAR_nT_grps)%ik      =ik
                   BS_T_grp(i_PAR_nT_grps)%first_cv=BS_K_dim
                   !
                 endif
                 !
               endif
               !
               if (i_loop==1) then
                 !
                 BS_T_grp(i_PAR_nT_grps)%size=BS_T_grp(i_PAR_nT_grps)%size+1
                 !
               else if (i_loop==2) then
                 !
                 i_T_in_the_grp=i_T_in_the_grp+1
                 !
                 BS_T_grp(i_PAR_nT_grps)%table(i_T_in_the_grp,:)=(/is,iv,ic,i_sp/)
                 !
                 PAR_IND_WF_b_and_k%element_2D(ic, ik)=.TRUE.
                 PAR_IND_WF_b_and_k%element_2D(iv,ikp)=.TRUE.
                 !
                 BS_T_grp(i_PAR_nT_grps)%E(i_T_in_the_grp)=E_eh
                 BS_T_grp(i_PAR_nT_grps)%f(i_T_in_the_grp)=f_eh
                 if (associated(Ken%Z)) then
                   BS_T_grp(i_PAR_nT_grps)%Z(i_T_in_the_grp)=Ken%Z(ic,ik,i_sp)*Ken%Z(iv,ikp,i_sp)
                 endif
                 if (associated(Ken%W)) then
                   BS_T_grp(i_PAR_nT_grps)%W(i_T_in_the_grp)=Ken%W(ic,ik,i_sp)-Ken%W(iv,ikp,i_sp)
                 endif
                 !
               endif
               !
             else
               !
               BS_nT_at_k(ik)=BS_nT_at_k(ik)+1
               !
             endif
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
 if (.not.FILL_the_groups.and..not.BS_anti_res.and.BS_not_const_eh_f) then
   call warning('Fractional e/h occupations. Causal BSEmode forced.')
   BSE_mode='causal'
   BS_anti_res=.true.
 endif
 !
 if (FILL_the_groups) then
   call PARALLEL_live_message("WF's",LOADED=count(PAR_IND_WF_b_and_k%element_2D),TOTAL=Xk%nibz*(BS_bands(2)-BS_bands(1)+1))
 endif
 !
 ! REPORT
 !
 if (FILL_the_groups) then
   call msg('r','[BSK] Transition groups (total)',BS_nT_grps)
 endif
 !
 ! CLEAN UP
 !
 deallocate(i_T_at_k)
 !
 contains
   !
   logical function T_is_ok( )
     !
     T_is_ok=.TRUE.
     !
     E_eh=Ken%E(ic,ik,i_sp)-Ken%E(iv,ikp,i_sp)
     !
     if (abs(E_eh)<1.E-5) then
       if(any((/ic/=Ken%bf,iv/=Ken%bf,ik/=Ken%kf,i_sp/=Ken%sf/)) ) T_is_ok=.FALSE.
       if(real(Wd)==0..and.aimag(Wd)==0.) T_is_ok=.FALSE.
     endif
     !
     ! Same ordering procedure of the Xo case (see X_eh_setup.F)
     !
     f_eh=Ken%f(iv,ikp,i_sp)*(spin_occ-Ken%f(ic,ik,i_sp))
     if (abs(f_eh)<epsilon(1._SP)) T_is_ok=.FALSE.
     !
     ! Additional (?) condition for spin-resolved calculations
     !
     if (n_spin==2.and. &
         (Ken%f(iv,ikp,i_sp)<=filled_tresh.or.&
          abs(spin_occ-Ken%f(ic,ik,i_sp))<=filled_tresh) ) T_is_ok=.FALSE.
     !
     ! A 5% fractional occupation swiches on the particular
     ! treatment of finite temp. BSE 
     !
     f_eh=f_eh/spin_occ/spin_occ
     !
     if (abs(E_eh)<1.E-5) then
       BS_drude_f_eh=f_eh
       T_is_ok=.FALSE.
     endif
     !
     if ( abs(f_eh-1.)> filled_tresh) BS_not_const_eh_f=.TRUE.
     !
     ! The e/h occ must be 1. in the T=0 case as the spin factor (spin_occ)
     ! would erronously increase the e-e scattering
     !
     if (any((/abs(E_eh)<BS_eh_en(1).and.BS_eh_en(1)>0.,&
&              abs(E_eh)>BS_eh_en(2).and.BS_eh_en(2)>0./))) T_is_ok=.FALSE.
     !
  end function
  !
end subroutine
