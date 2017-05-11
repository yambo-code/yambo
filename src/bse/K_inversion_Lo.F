!
!        Copyright (C) 2000-2017 the YAMBO team
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
subroutine K_inversion_Lo(iq,W,E,k,q,Lo_dim,Lo_n_loop,Lo) 
 !
 use pars,           ONLY:SP,cZERO,cI,pi,rZERO
 use stderr,         ONLY:intc
 use R_lattice,      ONLY:bz_samp,q_norm
 use X_m,            ONLY:Epsilon_ii,X_drude_term,global_gauge,iw_ref,l_drude
 use electrons,      ONLY:levels,spin
 use frequency,      ONLY:w_samp,W_reset,W_duplicate
 use BS_solvers,     ONLY:Co_factor,BSS_dipoles,&
&                         BSS_eh_table,BSS_eh_E,BSS_eh_f,&
&                         BSS_n_descs,BSS_description,BSS_eh_W,&
&                         BSS_add_antiresonant,K_diago_in_BS_mat,BS_diagonal
 use BS,             ONLY:BS_H_dim,BS_K_dim,BS_K_coupling,BSE_mode,BS_pert_soc
 use parallel_m,     ONLY:PP_redux_wait,PP_indexes,myid,PP_indexes_reset
 use interfaces,     ONLY:PARALLEL_index
 use LIVE_t,         ONLY:live_timing
 !
#if defined _ELPH
 use BS_solvers,     ONLY:BSS_uses_GreenF
 use BS,             ONLY:BS_bands
#endif
#if defined _PL
 use drivers,        ONLY:l_photolum
 use PHOTOLUM,       ONLY:BSS_PL_f,BSS_dipoles_PL,PL,PL_prefactor,PL_init
#endif
 !
#include<memory.h>
 !
 type(w_samp),  intent(in)  :: W
 type(levels),  intent(in)  :: E
 type(bz_samp), intent(in)  :: k,q
 integer,       intent(in)  :: iq,Lo_dim,Lo_n_loop
 complex(SP),   intent(out) :: Lo(BS_H_dim,W%n_freqs,Lo_dim)
 !
 ! Work Space
 !
 type(PP_indexes) :: px
 type(w_samp)     :: W_mins,W_plus
 integer          :: i_res,i_cpl,i_Lo_trans,ik_bz,iv,ic,iw,i_sp,ik_ibz,i_loop,n_loop,i_eps,i_Lo,transition(4),i_c
 complex(SP)      :: E_plus_W,residual
#if defined _PL
 complex(SP)      :: SF(W%n_freqs,2)
#endif
 !
#if defined _PL
 !
 ! PL_0
 !=======
 call PL_init( )
 PL(:,3) = rZERO
 !
#endif
 !
 Epsilon_ii(:,3:4) = cZERO
 !
#if defined _ELPH
 !
 ! Green Functions must be all mapped to the Xw range
 ! to be easily convoluted
 !
 if (allocated(E%GreenF)) then
   call X_GreenF_remap(BS_bands,E,W)
   BSS_uses_GreenF=.TRUE.
 endif
 !
#endif
 !
 if (     K_diago_in_BS_mat) n_loop=1
 if (.not.K_diago_in_BS_mat) n_loop=2
 call PP_indexes_reset(px)
 call PARALLEL_index(px,(/BS_K_dim/))
 !
 do i_loop=1,n_loop
   !
   i_eps=2+i_loop
   !
   if(n_loop==2) then
#if defined _PL
     if(i_loop==1) call live_timing('IP PL',px%n_of_elements(myid+1))
     if(i_loop==2) call live_timing('Shifted GF and PL',px%n_of_elements(myid+1))
#else
     if(i_loop==1) call live_timing('IP Absorption',px%n_of_elements(myid+1))
     if(i_loop==2) call live_timing('Shifted GF and Absorption',px%n_of_elements(myid+1))
#endif
   else
     call live_timing('IP GF and Absorption',px%n_of_elements(myid+1))
   endif
   !
   Lo=cZERO
   !
   call W_reset(W_mins)
   call W_reset(W_plus)
   call W_duplicate(W,W_mins)
   call W_duplicate(W,W_plus)
   !
   do i_res=1,BS_K_dim
     !
     i_cpl=i_res+BS_K_dim
     !
     if (.not.px%element_1D(i_res)) cycle
     !
     ik_bz =BSS_eh_table(i_res,1)
     iv    =BSS_eh_table(i_res,2)
     ic    =BSS_eh_table(i_res,3)
     i_sp  =spin(BSS_eh_table(i_res,:))
     !
     ik_ibz=k%sstar(ik_bz,1)   
     !
     transition=(/ik_bz,iv,ic,i_sp/)
     !
     i_Lo_trans=i_res
     i_Lo      =1
     !
     if ( trim(BSE_mode)=='retarded') then
       i_Lo_trans=i_res
       i_Lo      =2
     else if( trim(BSE_mode)=="coupling") then
       i_Lo_trans=i_cpl
       i_Lo      =1
     endif
     !
     if (.not.allocated(BSS_eh_W)) E_plus_W=BSS_eh_E(i_res)
     if (     allocated(BSS_eh_W)) E_plus_W=BSS_eh_E(i_res)-cI*BSS_eh_W(i_res)
     !
     if(i_loop==2) then
       forall(iw=1:W%n_freqs) W_mins%p(iw)=W%p(iw)-BS_diagonal(i_res)*BSS_eh_f(i_res)
       forall(iw=1:W%n_freqs) W_plus%p(iw)=W%p(iw)+BS_diagonal(i_res)*BSS_eh_f(i_res)
     endif
     !
     if ( .not.allocated(E%GreenF) ) then
       !
       ! Analytical GFs
       !================
       call X_GreenF_analytical(1,transition,W_mins,E,k,Lo(i_res,:,1),"r","eh",.FALSE.)
#if defined _PL
       call X_GreenF_analytical(1,transition,W,E,k,SF(:,1),"r","eh",.TRUE.)
       Lo(i_res,:,1+Lo_n_loop)=BSS_PL_f(1,i_res)/BSS_eh_f(i_res)**2.*aimag(W%p(1))/pi
#endif
       !
       if (BSS_add_antiresonant.or.BS_K_coupling) then
         !
         call X_GreenF_analytical(1,transition,W_plus,E,k,Lo(i_Lo_trans,:,i_Lo),"Ra","eh",.FALSE.)
#if defined _PL
         !
         ! The -1 coming from SF(:,2) and -abs(Lo)...compensate each other
         !
         call X_GreenF_analytical(1,transition,W,E,k,SF(:,2),"Ra","eh",.TRUE.)
         Lo(i_Lo_trans,:,i_Lo+Lo_n_loop)=BSS_PL_f(2,i_res)/BSS_eh_f(i_res)**2.*aimag(W%p(1))/pi
#endif
         !
       endif
       !
     else 
       !
       ! GPL_EXCLUDE_START
       !
       ! Convoluted GFs
       !================
       call X_GreenF_convoluted(1,transition,W_mins,E,k,Lo(i_res,:,1),"r",.FALSE.)
       !
       if (BSS_add_antiresonant.or.BS_K_coupling) then
         !
         call X_GreenF_convoluted(1,transition,W_plus,E,k,Lo(i_Lo_trans,:,i_Lo),"Ra",.FALSE.)
         !
       endif
       !
       ! GPL_EXCLUDE_END
       !
     endif
     !
     residual=BSS_dipoles(i_res)*conjg(BSS_dipoles(i_res))
     if (allocated(BSS_eh_W) .and. trim(global_gauge)=="velocity") residual=residual*E_plus_W/conjg(E_plus_W)
     if (trim(global_gauge)=='velocity') Lo(i_res,:,1)=(Lo(i_res,:,1)-Lo(i_res,iw_ref,1))
     Epsilon_ii(:,i_eps)=Epsilon_ii(:,i_eps)-residual*Lo(i_res,:,1)
     !
     if (BSS_add_antiresonant.or.BS_K_coupling) then
       residual=conjg(BSS_dipoles(i_Lo_trans))*BSS_dipoles(i_Lo_trans)
       if (trim(global_gauge)=='velocity') Lo(i_Lo_trans,:,i_Lo)=(Lo(i_Lo_trans,:,i_Lo)-Lo(i_Lo_trans,iw_ref,i_Lo))
       Epsilon_ii(:,i_eps)=Epsilon_ii(:,i_eps)-conjg(residual)*Lo(i_Lo_trans,:,i_Lo)
     endif
     !
#if defined _PL
     if (l_photolum.and.i_loop==1) then
       !
       residual=dot_product(BSS_dipoles_PL(i_res,:),BSS_dipoles_PL(i_res,:))
       !
       PL(:,i_eps)=PL(:,i_eps)-PL_prefactor*residual*aimag(SF(:,1))/pi*BSS_PL_f(1,i_res)
       if (BSS_add_antiresonant.or.BS_K_coupling) then
         PL(:,i_eps)=PL(:,i_eps)+PL_prefactor*residual*aimag(SF(:,2))/pi*BSS_PL_f(2,i_res)
       endif
       !
     endif
#endif
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
#if defined _PL
   if (l_photolum.and.i_loop==1) call PP_redux_wait(PL(:,i_eps))
#endif
   !
   if(trim(global_gauge)=="length")   Epsilon_ii(:,i_eps)=(1._SP+Epsilon_ii(:,i_eps)*Co_factor/q_norm(1)**2)
   if(trim(global_gauge)=="velocity") Epsilon_ii(:,i_eps)=(1._SP+Epsilon_ii(:,i_eps)*Co_factor/  W%p(:) **2)
   !
   if (l_drude) Epsilon_ii(:,i_eps)=Epsilon_ii(:,i_eps)+X_drude_term(:)*Co_factor/q_norm(1)**2
   !
 enddo
 !
 if(.not.K_diago_in_BS_mat) then
   YAMBO_FREE(BS_diagonal)
 endif
 !
 if( BS_pert_soc) then
   BSS_n_descs=BSS_n_descs+2
   BSS_description(BSS_n_descs-1)=' '
   BSS_description(BSS_n_descs)=' BSS|BZ Double Grid points      :'//intc(k%nibz+k%nibz_FineGd)
 endif
 !
end subroutine K_inversion_Lo
