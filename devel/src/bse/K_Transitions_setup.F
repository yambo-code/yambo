!
!        Copyright (C) 2000-2015 the YAMBO team
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
 !   BS_nT_at_k
 !
 ! Filling (FILL_the_groups=.TRUE.) of Transition groups
 !
 !type BS_T_group
 !  integer                       :: ik         ! ik index (\in IBZ)
 !  integer                       :: first_cv   ! Position in the cv transitions @ k
 !  integer                       :: size       
 !  complex(SP), allocatable      :: table(:,:) ! (/is,iv,ic,i_sp/)
 !  complex(SP), allocatable      :: Z(size)
 !  real(SP)   , allocatable      :: E(size)
 !  real(SP)   , allocatable      :: f(size)
 !  real(SP)   , allocatable      :: W(size)
 !end type BS_T_group
 !
 use pars,         ONLY:SP,IP
 use memory_m,     ONLY:mem_est
 use com,          ONLY:warning,error
 use BS,           ONLY:BS_K_dim,T_group,BS_eh_en,BS_K_coupling,&
&                       BS_bands,BS_nT_at_k,BS_anti_res,&
&                       BS_not_const_eh_f,BS_drude_f_eh,BS_Blocks_and_Transitions_alloc,&
&                       BSE_mode,BS_T_grp,BS_nT_grps
 use parallel_m,   ONLY:PAR_IND_WF_b_and_k,PAR_IND_T_all,PAR_IND_T_ordered,PAR_IND_T_groups
 use electrons,    ONLY:levels,n_sp_pol,spin_occ,filled_tresh
 use com,          ONLY:msg
 use R_lattice,    ONLY:qindx_X,bz_samp
 !
 implicit none
 type(levels) :: Ken
 type(bz_samp):: Xk
 complex(SP)  :: Wd
 integer      :: iq
 logical      :: FILL_the_groups,l_check
 !
 ! Work Space
 !
 integer :: ikbz,iv,ic,ik,ikp,i_sp,i_star,nb(2),&
&           i_T_group_now,i_T_group_last,i_g,n_loops,i_loop,i_T_in_the_grp
 real(SP):: E_eh,f_eh,f_e,f_h
 integer, allocatable :: i_T_at_k(:)
 !
 if (FILL_the_groups) call section('+','Transition Groups build-up')
 !
 if (FILL_the_groups) then
   !
   allocate(BS_T_grp(BS_nT_grps))
   do i_g=1,BS_nT_grps
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
 call k_build_up_BZ_tables(Xk)
 !
 nb(1)=Ken%nbf+1
 nb(2)=Ken%nbm
#if defined _RT
 nb=Ken%nbc
#endif
 !
 do i_loop=1,n_loops
   !
   i_T_at_k=0
   BS_K_dim=0
   i_T_group_now=0
   i_T_group_last=0
   !
   do ik=1,Xk%nibz
     do iv=BS_bands(1),nb(2)
       do ic=nb(1),BS_bands(2)
         !
         do i_star=1,Xk%nstar(ik)
           !
           ikbz=Xk%k_table(ik,Xk%star(ik,i_star))
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
               if (i_loop==1) then
                 !
                 BS_T_grp(i_T_group_now)%size=BS_T_grp(i_T_group_now)%size+1
                 !
               endif
               !
               l_check=PAR_IND_T_groups%element_1D(i_T_group_now).or.&
&                      any( PAR_IND_T_ordered%element_2D(:,i_T_group_now))
               !
               if (BS_k_coupling) l_check=l_check .or. any( PAR_IND_T_all%element_2D(:,i_T_group_now) )
               !
               if ( .not. l_check ) cycle
               !
               if (i_T_group_now/=i_T_group_last) then
                 !
                 i_T_group_last=i_T_group_now
                 i_T_in_the_grp=0
                 !
                 if (i_loop==1) then
                   !
                 else if (i_loop==2) then
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
               if (i_loop==2) then
                 !
                 i_T_in_the_grp=i_T_in_the_grp+1
                 !
                 BS_T_grp(i_T_group_now)%table(i_T_in_the_grp,:)=(/ikbz,iv,ic,i_sp/)
                 !
                 PAR_IND_WF_b_and_k%element_2D(ic, ik)=.TRUE.
                 PAR_IND_WF_b_and_k%element_2D(iv,ikp)=.TRUE.
                 !
                 BS_T_grp(i_T_group_now)%E(i_T_in_the_grp)=E_eh
                 BS_T_grp(i_T_group_now)%f(i_T_in_the_grp)=f_eh
                 if (associated(Ken%Z)) then
                   BS_T_grp(i_T_group_now)%Z(i_T_in_the_grp)=Ken%Z(ic,ik,i_sp)*Ken%Z(iv,ikp,i_sp)
                 endif
                 if (associated(Ken%W)) then
                   !
                   ! Note that W is negative as the linewidth is positive(negative) above(below) the Fermi level.
                   !
                   BS_T_grp(i_T_group_now)%W(i_T_in_the_grp)=Ken%W(ic,ik,i_sp)-Ken%W(iv,ikp,i_sp)
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
 ! REPORT
 !
 if (FILL_the_groups) call msg('r','[BSK] Transition groups (total)',BS_nT_grps)
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
     E_eh= Ken%E(ic,ik ,i_sp)-Ken%E(iv,ikp,i_sp)
     f_eh=(Ken%f(iv,ikp,i_sp)-Ken%f(ic,ik ,i_sp))/spin_occ
     !
     f_e = (         Ken%f(iv,ikp,i_sp))/spin_occ
     f_h = (spin_occ-Ken%f(ic,ik ,i_sp))/spin_occ
     !
     ! A 0.001 % fractional occupation swiches on the particular treatment of finite temp. BSE 
     if ( abs(abs(f_eh)-1._SP)> filled_tresh) BS_not_const_eh_f=.TRUE.
     !
     ! f_eh<0 transitions are included in the anti-resonant part
     ! f_eh=0 transitions give no contribution to absorption and are not considered
     if ( f_eh<=0._SP ) T_is_ok=.FALSE.
     !
     ! Small energy transitions
     ! If I add a Drude term just the poles inside K_f are considered
     ! Remember that the Drude terms wants the f_e*f_h occupation factor
     if (abs(E_eh)<1.E-5 .and. all((/real(Wd)/=0.,aimag(Wd)/=0./)) ) then
       if(all((/ic==Ken%bf,iv==Ken%bf,ik==Ken%kf,i_sp==Ken%sf/))) BS_drude_f_eh=f_e*f_h
       T_is_ok=.FALSE.
     endif
     !
     ! If I've included in valence states that are almost empty
     ! or in conduction states that are almost full, those are neglected
     if ( (f_e<=filled_tresh) .or. (f_h<=filled_tresh) ) T_is_ok=.FALSE.
     !
     if (any((/abs(E_eh)<BS_eh_en(1).and.BS_eh_en(1)>0.,&
&              abs(E_eh)>BS_eh_en(2).and.BS_eh_en(2)>0./))) T_is_ok=.FALSE.
     !
  end function
  !
end subroutine
