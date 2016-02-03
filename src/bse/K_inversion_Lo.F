!
!        Copyright (C) 2000-2015 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM DS
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
subroutine K_inversion_Lo(iq,W,E,k,q,Lo_dim,Lo) 
 !
 use pars,           ONLY:SP,cZERO,cONE,cI
 use parser_m,       ONLY:parser
 use stderr,         ONLY:intc,real2ch
 use R_lattice,      ONLY:bz_samp,q_norm
 use X_m,            ONLY:Epsilon_ii,Epsilon_ij,Joint_DOS,X_drude_term,global_gauge,iw_ref,l_drude
 use electrons,      ONLY:levels,BZ_DbGd_tot_nkpts,spin
 use frequency,      ONLY:w_samp,W_reset,W_duplicate
 use BS,             ONLY:Co_factor,BSS_rhoq0,BS_H_dim,BS_K_dim,&
&                         BSE_mode,BSS_eh_table,BSS_eh_E,BSS_eh_f,BS_mat,&
&                         BSS_n_descs,BSS_description,BSS_eh_W,BSS_eh_Z
 use parallel_m,     ONLY:PP_redux_wait,PP_indexes,myid,PP_indexes_reset
 use interfaces,     ONLY:PARALLEL_index
 use LIVE_t,         ONLY:live_timing
 !
#if defined _ELPH
 use BS,             ONLY:BSS_uses_GreenF,BS_bands
#endif
 !
 implicit none
 !
 type(w_samp),  intent(in)  :: W
 type(levels),  intent(in)  :: E
 type(bz_samp), intent(in)  :: k,q
 integer,       intent(in)  :: iq,Lo_dim
 complex(SP),   intent(out) :: Lo(BS_H_dim,W%n_freqs,Lo_dim)
 !
 type(PP_indexes) :: px
 type(w_samp)     :: W_mins,W_plus
 !
 logical          :: K_diag_in_BS_mat,eval_anti_resonant
 integer          :: i1,i2,i3,i4,ik_bz,iv,ic,iw,i_sp,ik_ibz,i_loop,n_loop,i_eps,i_Lo,transition(4)
 complex(SP)      :: E_plus_W,residual
 !
 complex(SP), allocatable :: BS_mat_diagonal(:)
 !
 Epsilon_ii(:,3:4)=cZERO
 Joint_DOS(:,3:4) =cZERO
 !
 call parser('BSSInvKdiag',K_diag_in_BS_mat)
 !
 if (.not.K_diag_in_BS_mat) allocate(BS_mat_diagonal(BS_K_dim))
 !
 do i1=1,BS_K_dim
   i2=i1+BS_K_dim
   !
   ! I need to remove energies and width from the diagonal of the BS_mat
   ! These are now embodied in the Green's function.
   !
   if (.not.allocated(BSS_eh_Z)) E_plus_W=BSS_eh_E(i1)
   if (     allocated(BSS_eh_Z)) E_plus_W=BSS_eh_E(i1)-cI*BSS_eh_W(i1)
   !
   BS_mat(i1,i1)=BS_mat(i1,i1)-E_plus_W
   if (index(BSE_mode,'coupling')>0) BS_mat(i2,i2)= BS_mat(i2,i2)+conjg(E_plus_W)
   !
   ! I need to remove the occupation factors from the kernel
   ! These are now embodied in the Green's function
   !
   do i3=1,BS_K_dim
     i4=i3+BS_K_dim
     !
     ! I need to remove the occupation factors from the kernel
     ! These are now embodied in the Green's function
     !
     BS_mat(i3,i1)=BS_mat(i3,i1)/sqrt(BSS_eh_f(i3))/sqrt(BSS_eh_f(i1))
     if (index(BSE_mode,'coupling')>0) then
       BS_mat(i1,i4)= BS_mat(i1,i4)/sqrt(BSS_eh_f(i1))/sqrt(BSS_eh_f(i3))/cI
       BS_mat(i2,i3)= BS_mat(i2,i3)/sqrt(BSS_eh_f(i1))/sqrt(BSS_eh_f(i3))/cI
       BS_mat(i2,i4)=-BS_mat(i2,i4)/sqrt(BSS_eh_f(i1))/sqrt(BSS_eh_f(i3))
     endif
   enddo
   !
   if (.not.K_diag_in_BS_mat) then
     !
     ! I store the diagonal of the kernel only in BS_mat_diagonal,
     ! to be inserted in Lo
     !
     BS_mat_diagonal(i1)=BS_mat(i1,i1)
     !
     BS_mat(i1,i1)=cZERO
     if (index(BSE_mode,'coupling')>0) BS_mat(i2,i2)= cZERO
     !
   endif
   !
 enddo
 !
#if defined _ELPH
 !
 ! Green Functions must be all mapped to the Xw range
 ! to be easily convoluted
 !
 if (associated(E%GreenF)) then
   call X_GreenF_remap(BS_bands,E,W)
   BSS_uses_GreenF=.TRUE.
 endif
 !
#endif
 !
 eval_anti_resonant= (trim(BSE_mode)=='causal') .or. (trim(BSE_mode)=='coupling')
 !
 if (     K_diag_in_BS_mat) n_loop=1
 if (.not.K_diag_in_BS_mat) n_loop=2
 !
 do i_loop=1,n_loop
   !
   call PP_indexes_reset(px)
   call PARALLEL_index(px,(/BS_K_dim/))
   !
   i_eps=2+i_loop
   !
   if(n_loop==2) then
     if(i_loop==1) call live_timing('Compute Eps IP',px%n_of_elements(myid+1))
     if(i_loop==2) call live_timing('Compute Lo and Eps shifted',px%n_of_elements(myid+1))
   else
     call live_timing('Compute Eps IP and Lo',px%n_of_elements(myid+1))
   endif
   !
   Lo=cZERO
   !
   call W_reset(W_mins)
   call W_reset(W_plus)
   call W_duplicate(W,W_mins)
   call W_duplicate(W,W_plus)
   !
   do i1=1,BS_K_dim
     !
     if (.not.px%element_1D(i1)) cycle
     !
     ik_bz =BSS_eh_table(i1,1)
     iv    =BSS_eh_table(i1,2)
     ic    =BSS_eh_table(i1,3)
     i_sp  =spin(BSS_eh_table(i1,:))
     !
     ik_ibz=k%sstar(ik_bz,1)   
     !
     transition=(/ik_bz,iv,ic,i_sp/)
     !
     if ( trim(BSE_mode)=='causal') then
       i2=i1
       i_Lo=2
     else if( trim(BSE_mode)=="coupling") then
       i2=i1+BS_K_dim
       i_Lo=1
     endif
     !
     if (.not.allocated(BSS_eh_W)) E_plus_W=BSS_eh_E(i1)
     if (     allocated(BSS_eh_W)) E_plus_W=BSS_eh_E(i1)-cI*BSS_eh_W(i1)
     !
     if(i_loop==2) then
       forall(iw=1:W%n_freqs) W_mins%p(iw)=W%p(iw)-BS_mat_diagonal(i1)
       forall(iw=1:W%n_freqs) W_plus%p(iw)=W%p(iw)-BS_mat_diagonal(i1)
     endif
     !
#if defined _ELPH
     if ( .not.associated(E%GreenF) ) then
#endif
       !
       ! Analytical GFs
       !================
       call X_GreenF_analytical(1,transition,W_mins,E,k,Lo(i1,:,1),"r","eh")
       if (eval_anti_resonant) &
       &  call X_GreenF_analytical(1,transition,W_plus,E,k,Lo(i2,:,i_Lo),"ca","eh")
       !
#if defined _ELPH
     else 
       !
       ! GPL_EXCLUDE_START
       !
       ! Convoluted GFs
       !================
       call X_GreenF_convoluted(1,transition,W_mins,E,k,Lo(i1,:,1),"r")
       if (eval_anti_resonant) call X_GreenF_convoluted(1,transition,W_plus,E,k,Lo(i2,:,i_Lo),"ca")
       !
       ! GPL_EXCLUDE_END
       !
     endif
#endif
     !
     residual=BSS_rhoq0(i1)*conjg(BSS_rhoq0(i1))
     if (allocated(BSS_eh_W) .and. trim(global_gauge)=="velocity") residual=residual*E_plus_W/conjg(E_plus_W)
     if (trim(global_gauge)=='velocity') Lo(i1,:,1)=(Lo(i1,:,1)-Lo(i1,iw_ref,1))
     Epsilon_ii(:,i_eps)=Epsilon_ii(:,i_eps)-residual*Lo(i1,:,1)
     Joint_DOS(:,i_eps) =Joint_DOS(:,i_eps) -         Lo(i1,:,2)
     !
     if ( eval_anti_resonant ) then
       residual=conjg(BSS_rhoq0(i2))*BSS_rhoq0(i2)
       if (trim(global_gauge)=='velocity') Lo(i2,:,i_Lo)=(Lo(i2,:,i_Lo)-Lo(i2,iw_ref,i_Lo))
       Epsilon_ii(:,i_eps)=Epsilon_ii(:,i_eps)-conjg(residual)*Lo(i2,:,i_Lo)
       Joint_DOS(:,i_eps) =Joint_DOS(:,i_eps) -                Lo(i2,:,i_Lo)
     endif
     !
     call live_timing(steps=1)
     !
   enddo
   !
   call live_timing( )
   !
   if(i_loop==n_loop) call PP_redux_wait(Lo)
   !
   call PP_redux_wait(Epsilon_ii(:,i_eps))
   call PP_redux_wait(Joint_DOS(:,i_eps) )
   !
   if(trim(global_gauge)=="length")   Epsilon_ii(:,i_eps)=(1._SP+Epsilon_ii(:,i_eps)*Co_factor/q_norm(1)**2)
   if(trim(global_gauge)=="velocity") Epsilon_ii(:,i_eps)=(1._SP+Epsilon_ii(:,i_eps)*Co_factor/  W%p(:) **2)
   Joint_DOS(:,i_eps)=Joint_DOS(:,i_eps)*Co_factor
   !
   if (l_drude) Epsilon_ii(:,i_eps)=Epsilon_ii(:,i_eps)+X_drude_term(:)*Co_factor/q_norm(1)**2
   !
 enddo
 !
 if(.not.K_diag_in_BS_mat) deallocate(BS_mat_diagonal)
 !
 BSS_n_descs=BSS_n_descs+2
 BSS_description(BSS_n_descs-1)=' '
 BSS_description(BSS_n_descs)=' BSS|BZ Double Grid points      :'//intc(BZ_DbGd_tot_nkpts)
 !
end subroutine K_inversion_Lo
