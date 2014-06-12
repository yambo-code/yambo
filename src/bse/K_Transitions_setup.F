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
 use BS,           ONLY:BS_K_dim,O_v_state,O_c_state,&
&                       O_n_v_states,O_n_c_states,BS_eh_en,&
&                       BS_bands,BS_nT_at_k,BS_anti_res,&
&                       BS_not_const_eh_f,BS_drude_f_eh,&
&                       BSE_mode,BS_T_grp,BS_K_coupling
 use parallel_m,   ONLY:PAR_BS_nT_grps,PAR_BS_T_grps_index,PAR_IND_Xk_bz,PAR_IND_eh,&
&                       PAR_IND_CON_BANDS_X,PAR_IND_VAL_BANDS_X,PAR_COM_eh_INDEX,&
&                       PAR_IND_CON_BANDS_X_ID,PAR_IND_VAL_BANDS_X_ID
 use electrons,    ONLY:levels,n_sp_pol,spin_occ,filled_tresh,n_spin
 use R_lattice,    ONLY:qindx_X,bz_samp
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
&           i_T_group_now,i_T_group_last,i_g,n_loops,i_loop,i_PAR_nT_grps,i_T_at_k,i_T_in_the_grp,nT
 integer, external :: K_T_group_index
 real(SP):: E_eh,f_eh
 !
 if (.not.FILL_the_groups) call section('+','Transition Space definition')
 if (FILL_the_groups)      call section('=','Transition Groups build-up')
 !
 if (FILL_the_groups) then
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
   allocate(PAR_IND_CON_BANDS_X(5)%n_of_elements(PAR_COM_eh_INDEX%n_CPU+1))
   allocate(PAR_IND_VAL_BANDS_X(5)%n_of_elements(PAR_COM_eh_INDEX%n_CPU+1))
   allocate(PAR_IND_CON_BANDS_X(5)%element_1D(BS_bands(2)))
   allocate(PAR_IND_VAL_BANDS_X(5)%element_1D(BS_bands(2)))
   PAR_IND_CON_BANDS_X(5)%n_of_elements=0
   PAR_IND_VAL_BANDS_X(5)%n_of_elements=0
   PAR_IND_CON_BANDS_X(5)%element_1D=.FALSE.
   PAR_IND_VAL_BANDS_X(5)%element_1D=.FALSE.
   PAR_IND_CON_BANDS_X_ID(5)=PAR_COM_eh_INDEX%CPU_id+1
   PAR_IND_VAL_BANDS_X_ID(5)=PAR_COM_eh_INDEX%CPU_id+1
   !
 else
   allocate(BS_nT_at_k(Xk%nbz))
   call mem_est("BS_nT_at_k",(/Xk%nbz/),(/IP/))
   BS_nT_at_k=0
   n_loops=1
 endif 
 !
 !if (BS_res_K_corr) then
 !  O_n_v_states=0
 !  O_n_c_states=0
 !endif
 !
 do i_loop=1,n_loops
   !
   BS_K_dim=0
   i_T_group_now=0
   i_T_group_last=0
   i_PAR_nT_grps=0
   !
   do ikbz=1,Xk%nbz
     !
     i_T_at_k=0
     !
     !Iic=0
     !Iiv=0
     !
     do iv=BS_bands(1),Ken%nbm
       do ic=Ken%nbf+1,BS_bands(2)
         !
         ik =Xk%sstar(ikbz,1)
         ikp=Xk%sstar(qindx_X(iq,ikbz,1),1)
         !
         do i_sp=1,n_sp_pol
           !
           if (.not.T_is_ok()) cycle
           !
           BS_K_dim=BS_K_dim+1
           i_T_at_k=i_T_at_k+1
           !
           if (FILL_the_groups) then
             !
             if (.not.PAR_IND_Xk_bz%element_1D(ikbz)) cycle
             if (.not.PAR_IND_eh(ikbz)%element_1D(i_T_at_k)) cycle
             !
             i_T_group_now=K_T_group_index(BS_K_dim)
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
               else
                 !
                 nT=BS_T_grp(i_PAR_nT_grps)%size
                 !
                 allocate(BS_T_grp(i_PAR_nT_grps)%dipoles_R(nT))
                 call mem_est("BS_T_grp_dipoles_R_N"//trim(intc(i_PAR_nT_grps)),(/nT/))
                 if (BS_K_coupling) then
                   allocate(BS_T_grp(i_PAR_nT_grps)%dipoles_A(nT))
                   call mem_est("BS_T_grp_dipoles_A_N"//trim(intc(i_PAR_nT_grps)),(/nT/))
                 endif
                 allocate(BS_T_grp(i_PAR_nT_grps)%table(nT,3))
                 call mem_est("BS_T_grp_table_N"//trim(intc(i_PAR_nT_grps)),(/nT/),(/IP/))
                 allocate(BS_T_grp(i_PAR_nT_grps)%E(nT))
                 call mem_est("BS_T_grp_E_N"//trim(intc(i_PAR_nT_grps)),(/nT/),(/SP/))
                 allocate(BS_T_grp(i_PAR_nT_grps)%f(nT))
                 call mem_est("BS_T_grp_f_N"//trim(intc(i_PAR_nT_grps)),(/nT/),(/SP/))
                 if (associated(Ken%Z)) then
                   allocate(BS_T_grp(i_PAR_nT_grps)%Z(nT))
                   call mem_est("BS_T_grp_Z_N"//trim(intc(i_PAR_nT_grps)),(/nT/))
                 endif
                 if (associated(Ken%W)) then
                   allocate(BS_T_grp(i_PAR_nT_grps)%W(nT))
                   call mem_est("BS_T_grp_W_N"//trim(intc(i_PAR_nT_grps)),(/nT/),(/SP/))
                 endif
                 !
                 BS_T_grp(i_PAR_nT_grps)%ik      =ikbz
                 BS_T_grp(i_PAR_nT_grps)%first_cv=BS_K_dim
                 !
               endif
               !
             endif
             !
             if (i_loop==1) BS_T_grp(i_PAR_nT_grps)%size=BS_T_grp(i_PAR_nT_grps)%size+1
             !
             if (i_loop==2) then
               !
               i_T_in_the_grp=i_T_in_the_grp+1
               !
               BS_T_grp(i_PAR_nT_grps)%table(i_T_in_the_grp,:)=(/iv,ic,i_sp/)
               !
               if (.not.PAR_IND_CON_BANDS_X(5)%element_1D(ic)) then
                 PAR_IND_CON_BANDS_X(5)%element_1D(ic)=.TRUE.
                 PAR_IND_CON_BANDS_X(5)%n_of_elements(PAR_COM_eh_INDEX%n_CPU+1)=&
&                                       PAR_IND_CON_BANDS_X(5)%n_of_elements(PAR_COM_eh_INDEX%n_CPU+1)+1
               endif
               !
               if (.not.PAR_IND_VAL_BANDS_X(5)%element_1D(iv)) then
                 PAR_IND_VAL_BANDS_X(5)%element_1D(iv)=.TRUE.
                 PAR_IND_VAL_BANDS_X(5)%n_of_elements(PAR_COM_eh_INDEX%n_CPU+1)=&
&                                       PAR_IND_VAL_BANDS_X(5)%n_of_elements(PAR_COM_eh_INDEX%n_CPU+1)+1
               endif
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
             BS_nT_at_k(ikbz)=BS_nT_at_k(ikbz)+1
             !
           endif
           !
!           Iiv(iv,i_sp)=1
!           Iic(ic,i_sp)=1
           !
         enddo
       enddo
     enddo
     !
   enddo
   !
 enddo
 !
 if (.not.FILL_the_groups.and..not.BS_anti_res.and.BS_not_const_eh_f) then
   call warning('Fractional e/h occupations. Causal BSEmode forced.')
   BSE_mode='causal'
   BS_anti_res=.true.
 endif
 !
 !
!     if (.not.BS_res_K_corr) cycle
     !
!     do iv=BS_bands(1),Ken%nbm
!       do i_sp=1,n_sp_pol
!         !
!         if (Iiv(iv,i_sp)==1) then
!           O_n_v_states(ikbz,i_sp)=O_n_v_states(ikbz,i_sp)+1
!           if (allocated(O_v_state)) O_v_state(ikbz,O_n_v_states(ikbz,i_sp),i_sp)=iv
!         endif
!         !
!       enddo
!     enddo
!     !
!     do ic=Ken%nbf+1,BS_bands(2)
!       do i_sp=1,n_sp_pol
!         !
!         if (Iic(ic,i_sp)==1) then
!            O_n_c_states(ikbz,i_sp)=O_n_c_states(ikbz,i_sp)+1
!            if (allocated(O_c_state)) O_c_state(ikbz,O_n_c_states(ikbz,i_sp),i_sp)=ic
!         endif
!         !
!       enddo
!     enddo
!   enddo
   !
!   if (allocated(BS_eh_table)) return
   !
   ! Allocations
   !
!   allocate(BS_eh_table(BS_K_dim,3+n_sp_pol-1),BS_eh_E(BS_K_dim),BS_eh_f(BS_K_dim))
!   call mem_est("BS_eh_table BS_eh_E BS_eh_f",&
!&               (/(3+n_sp_pol-1)*BS_K_dim,BS_K_dim,BS_K_dim/),(/IP,SP,SP/))
!   !
!   if (associated(Ken%W)) then
!     allocate(BS_eh_W(BS_K_dim))
!     call mem_est("BS_eh_W",(/BS_K_dim/),(/SP/))
!   endif
!   if (associated(Ken%Z)) then
!     allocate(BS_eh_Z(BS_K_dim))
!     call mem_est("BS_eh_Z",(/BS_K_dim/))
!   endif
!   !
!   if (.not.BS_res_K_corr) cycle
!   !
!   allocate(O_v_state(Xk%nbz,maxval(O_n_v_states),n_sp_pol),&
!&           O_c_state(Xk%nbz,maxval(O_n_c_states),n_sp_pol))
!   call mem_est("O_v_state O_c_state",(/size(O_v_state),size(O_c_state)/),(/IP,IP/))
!   !
! enddo
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
