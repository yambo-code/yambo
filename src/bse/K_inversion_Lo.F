!
! License-Identifier: GPL
!
! Copyright (C) 2016 The Yambo Team
!
! Authors (see AUTHORS file for details): AM DS
!
subroutine K_inversion_Lo(iq,W,E,k,Lo_dim,Lo_n_loop,Lo) 
 !
 use pars,           ONLY:SP,cZERO,cI,pi,rZERO,schlen
 use R_lattice,      ONLY:bz_samp,bare_qpg
 use X_m,            ONLY:Resp_ii,global_gauge,X_Double_Grid_setup,&
&                         X_DbGd_nkpt_at_q,X_DbGd_nkpt_at_q_table,skip_cond_sum_rule,X_do_obs,&
&                         i_G_shift
 use electrons,      ONLY:levels
 use frequency,      ONLY:w_samp,W_reset,W_duplicate
 use BS_solvers,     ONLY:Co_factor,diam_term_exact,BSS_dipoles_opt,BSS_uses_DbGd,&
&                         BSS_eh_table,BSS_eh_E,BSS_eh_f,BSS_eh_W,K_diago_in_BS_mat,BS_diagonal,&
&                         para_term_w0
 use BS,             ONLY:BS_H_dim,BS_K_dim,BSE_mode,BS_q,BS_n_eh_spaces,&
&                         BS_K_anti_resonant,l_BS_ares_from_res
 use parallel_m,     ONLY:PAR_IND_Xk_ibz_ID,PAR_COM_Xk_ibz_INDEX,PAR_IND_Xk_ibz
 use parallel_int,   ONLY:PP_redux_wait
 use LIVE_t,         ONLY:live_timing
#if defined _ELPH
 use BS_solvers,     ONLY:BSS_uses_GreenF
 use BS,             ONLY:BS_bands
#endif
 use BS,             ONLY:l_BS_photolum
 use PHOTOLUM,       ONLY:BSS_PL_f,BSS_dipoles_PL,PL,PL_prefactor
 !
#include<memory.h>
 !
 type(w_samp),  intent(inout) :: W
 type(levels),  intent(in)    :: E
 type(bz_samp), intent(in)    :: k
 integer,       intent(in)    :: iq,Lo_dim,Lo_n_loop
 complex(SP),   intent(out)   :: Lo(BS_H_dim,W%n_freqs,Lo_dim)
 !
 ! Work Space
 !
 type(w_samp)     :: W_mins,W_plus
 integer          :: iw_ref,i_bsk,i_res,i_cpl,i_Lo_trans,ik_bz,iv,ic,iw,i_sp_c,i_sp_v,ik_ibz,&
 &                   i_res_ares,i_para,i_loop,n_loop,i_eps,i_Lo,transition(4)
 complex(SP)      :: E_plus_W,residual
 complex(SP)      :: SF(W%n_freqs,2)
 character(schlen):: LT_title
 !
 ! Zeroing
 !=======
 if (X_do_obs("pl"))  PL(:,3) = rZERO
 if (X_do_obs("eps")) Resp_ii(:,3:4) = cZERO
 !
 ! DbGd setup
 !
 call X_Double_Grid_setup(iq,k,k%FGbz)
 !
 ! Look for the W(iw_ref) closest 0
 !==================================
 call FREQUENCIES_set_reference_point(W%p,W%n_freqs,iw_ref)
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
 !
 do i_loop=1,n_loop
   !
   i_eps=2+i_loop
   if (i_loop==1) i_para=1
   if (i_loop==2) i_para=3
   !
   if(n_loop==2) then
     if(l_BS_photolum) then
       if(i_loop==1) LT_title='IP PL'
       if(i_loop==2) LT_title='Shifted GF and PL'
     else
       if(i_loop==1) LT_title='IP Absorption'
       if(i_loop==2) LT_title='Shifted GF and Absorption'
     endif
   else
     LT_title='IP GF and Absorption'
   endif
   call live_timing(trim(LT_title),PAR_IND_Xk_ibz%n_of_elements(PAR_IND_Xk_ibz_ID+1))
   !
   Lo=cZERO
   !
   call W_reset(W_mins)
   call W_reset(W_plus)
   call W_duplicate(W,W_mins)
   call W_duplicate(W,W_plus)
   !
   do i_res_ares=1,BS_n_eh_spaces
     do i_bsk=1,BS_K_dim(i_res_ares)
       !
       i_res=i_bsk
       i_cpl=i_res+BS_K_dim(1)
       if(BS_n_eh_spaces==2) i_res=i_res+BS_K_dim(1)*(i_res_ares-1)
       !
       ik_bz =BSS_eh_table(i_res,1)
       iv    =BSS_eh_table(i_res,2)
       ic    =BSS_eh_table(i_res,3)
       i_sp_c=BSS_eh_table(i_res,4)
       i_sp_v=BSS_eh_table(i_res,5)
       !
       ik_ibz=k%sstar(ik_bz,1)
       !
       if (.not.PAR_IND_Xk_ibz%element_1D(ik_ibz)) cycle
       !
       i_Lo_trans=i_res
       i_Lo      =1
       transition=(/ik_bz,iv,ic,i_sp_c/)
       !
       if (BS_n_eh_spaces==2) then
         if ( i_res_ares==1) i_Lo      =1
         if ( i_res_ares==2) i_Lo      =2
       else
         if ( trim(BSE_mode)=='retarded') then
           i_Lo_trans=i_res
           i_Lo      =2
         else if( trim(BSE_mode)=="coupling") then
           i_Lo_trans=i_cpl
           i_Lo      =1
         endif
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
         if(i_res_ares==1) call X_GreenF_analytical(iq,transition,W_mins,E,k,Lo(i_res,:,1),"r" ,"eh",.FALSE.,.FALSE.)
         if(i_res_ares==2) call X_GreenF_analytical(iq,transition,W_mins,E,k,Lo(i_res,:,i_Lo),"Ra","eh",.FALSE.,.FALSE.)
         if (BS_K_anti_resonant) then
           call X_GreenF_analytical(iq,transition,W_plus,E,k,Lo(i_Lo_trans,:,i_Lo),"Ra","eh",.FALSE.,.FALSE.)
         endif
         if (l_BS_photolum) then
           if(i_res_ares==1) call X_GreenF_analytical(iq,transition,W,E,k,SF(:,1),"r" ,"eh",.TRUE.,.FALSE.)
           if(i_res_ares==2) call X_GreenF_analytical(iq,transition,W,E,k,SF(:,2),"Ra","eh",.TRUE.,.FALSE.)
           if (BS_K_anti_resonant) then
             call X_GreenF_analytical(iq,transition,W,E,k,SF(:,2),"Ra","eh",.TRUE.,.FALSE.)
           endif
           Lo(i_Lo_trans,:,i_Lo+Lo_n_loop)=BSS_PL_f(i_res)/BSS_eh_f(i_res)**2*aimag(W%p(1))/pi
         endif
         !
       else 
         !
         ! Convoluted GFs
         !================
         if(i_res_ares==1) call X_GreenF_convoluted(iq,transition,W_mins,E,k,Lo(i_res,:, 1  ),"r" )
         if(i_res_ares==2) call X_GreenF_convoluted(iq,transition,W_mins,E,k,Lo(i_res,:,i_Lo),"Ra")
         if (BS_K_anti_resonant) then
           call X_GreenF_convoluted(iq,transition,W_plus,E,k,Lo(i_Lo_trans,:,i_Lo),"Ra")
         endif
         !
       endif
       !
       if(i_res_ares==1) then
         residual=BSS_dipoles_opt(1,i_res)*conjg(BSS_dipoles_opt(1,i_res))
         if (allocated(BSS_eh_W) .and. trim(global_gauge)=="velocity") residual=residual*E_plus_W/conjg(E_plus_W)
         para_term_w0(i_para)=para_term_w0(i_para)+Lo(i_res,iw_ref,1)*Co_factor
         Resp_ii(:,i_eps)=Resp_ii(:,i_eps)-residual*Lo(i_res,:,1)
       endif     
       !
       if (BS_K_anti_resonant.and.l_BS_ares_from_res) then
         residual=conjg(BSS_dipoles_opt(1,i_Lo_trans))*BSS_dipoles_opt(1,i_Lo_trans)
         para_term_w0(i_para)=para_term_w0(i_para)+Lo(i_Lo_trans,iw_ref,i_Lo)*Co_factor
         Resp_ii(:,i_eps)=Resp_ii(:,i_eps)-conjg(residual)*Lo(i_Lo_trans,:,i_Lo)
       endif
       !
       if (l_BS_photolum.and.i_loop==1) then
         residual=dot_product(BSS_dipoles_PL(:,i_res),BSS_dipoles_PL(:,i_res))
         PL(:,i_eps)=PL(:,i_eps)-PL_prefactor*residual*aimag(SF(:,1))/pi*BSS_PL_f(i_res)
         if (BS_K_anti_resonant.and.l_BS_ares_from_res) then
           PL(:,i_eps)=PL(:,i_eps)+PL_prefactor*residual*aimag(SF(:,2))/pi*BSS_PL_f(i_cpl)
         endif
       endif
       !
       if (k%sstar(ik_bz,2)==1) call live_timing(steps=1)
       !
     enddo
   enddo
   !
   call live_timing( )
   !
   if(i_loop==n_loop) call PP_redux_wait(Lo,COMM=PAR_COM_Xk_ibz_INDEX%COMM)
   !
   call PP_redux_wait(resp_ii(:,i_eps),    COMM=PAR_COM_Xk_ibz_INDEX%COMM)
   call PP_redux_wait(para_term_w0(i_para),COMM=PAR_COM_Xk_ibz_INDEX%COMM)
   if (l_BS_photolum.and.i_loop==1) call PP_redux_wait(PL(:,i_eps),COMM=PAR_COM_Xk_ibz_INDEX%COMM)
   !
   Resp_ii(:,i_eps)= Resp_ii(:,i_eps)*Co_factor
   !
 enddo
 !
 if(.not.K_diago_in_BS_mat) then
   YAMBO_FREE(BS_diagonal)
 endif
 !
 if (BSS_uses_DbGd) then
   YAMBO_FREE(X_DbGd_nkpt_at_q)
   YAMBO_FREE(X_DbGd_nkpt_at_q_table)
 endif  
 !
 if (iq>BS_q(1)) return
 !
end subroutine K_inversion_Lo
