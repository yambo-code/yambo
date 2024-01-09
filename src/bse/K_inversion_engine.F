!
! License-Identifier: GPL
!
! Copyright (C) 2016 The Yambo Team
!
! Authors (see AUTHORS file for details): AM DS
!
subroutine K_inversion_engine(what,iw_2start,iw_2do,iq,px,W,Lo,Lo_dim,Lo_n_loop,do_it_full)
 !
 use pars,           ONLY:SP,cZERO
 use parallel_m,     ONLY:PP_indexes
 use frequency,      ONLY:w_samp
 use X_m,            ONLY:Resp_ii,i_G_shift
 use LIVE_t,         ONLY:live_timing
 use R_lattice,      ONLY:bare_qpg
 use wrapper,        ONLY:Vstar_dot_V,M_by_V,V_dot_V
 use wrapper_omp,    ONLY:Vstar_dot_V_omp,M_by_V_omp,V_dot_V_omp
 use BS,             ONLY:BS_H_dim,l_BS_photolum
 use BS_solvers,     ONLY:Co_factor,K_INV_EPS,BSS_dipoles_opt,Co_factor,K_inv_slk,K_slk,K_INV_PL
 use PHOTOLUM,       ONLY:PL,PL_prefactor,BSS_dipoles_PL
 !
#include<memory.h>
 !
 character(3)     :: what
 integer          :: iw_2start,iw_2do,iq,Lo_dim,Lo_n_loop
 type(PP_indexes) :: px
 type(w_samp)     :: W
 complex(SP)      :: Lo(BS_H_dim,W%n_freqs,Lo_dim)
 logical          :: do_it_full
 !
 ! Work Space
 !
 integer      :: i2,iw,i_L
 character(1) :: Gr
 complex(SP), allocatable :: Mm1(:,:)
 integer,        external :: PL_via_perturbative_inversion
 integer,        external :: EPS_via_perturbative_inversion
 !
 if (do_it_full.and..not.K_slk%kind=="SLK") then
   YAMBO_ALLOC(Mm1,(BS_H_dim,BS_H_dim))
   Mm1=cZERO
 endif
 !
 do i2=1,iw_2do
   !
   iw=i2+iw_2start-1
   !
   ! The parallel loop is controlled in the manin body loop 
   ! when a full inversion is performed
   !
   if (.not.do_it_full) then
     !
     if(.not.px%element_1D(iw)) then
       K_INV_EPS%err(iw)=0
       if (l_BS_photolum) K_INV_PL%err(iw)=0
       cycle
     endif
     !
     if (K_INV_EPS%err(iw)/=0) then
       K_INV_EPS%err(iw)=EPS_via_perturbative_inversion(BS_H_dim,Lo_dim,Lo_n_loop,BSS_dipoles_opt(1,:),-Co_factor,&
&                                                       Resp_ii(iw,2),Lo(:,iw,:))
     endif
     !
     if(l_BS_photolum) then
       if (K_INV_PL%err(iw)/=0) then
         K_INV_PL%err(iw)=PL_via_perturbative_inversion(BS_H_dim,Lo_dim,Lo_n_loop,BSS_dipoles_PL,PL_prefactor,&
&                                                       PL(iw,2),Lo(:,iw,:))
       endif
     endif
     !
   else
     !
     do i_L=1,Lo_n_loop
       !
       if (i_L==1) Gr="r"
       if (i_L==2) Gr="a"
       !
#if defined _SCALAPACK
       if (     K_slk%kind=="SLK") call K_inversion_do_it_SLK (Lo(:,iw,i_L),Gr)
#endif
       if (.not.K_slk%kind=="SLK") call K_inversion_do_it_full(Lo(:,iw,i_L),Mm1,Gr)
       !
       if (what=="EPS".or.what=="any") then
         call EPS_compose(i_L)
         K_INV_EPS%err(iw)=0
       endif
       !
       if(l_BS_photolum) then
         if (what=="PLS".or.what=="any") then
           call PL_compose(i_L)
           K_INV_PL%err(iw)=0
         endif
       endif
       !
     enddo
     !
   endif
   !       
   if (.not.do_it_full) call live_timing(steps=1)
   !
 enddo
 !
 if (do_it_full) then
   YAMBO_FREE(Mm1)
 else
   call live_timing()
 endif
 !
 contains
   !
   subroutine PL_compose(i_L_)
     complex(SP) :: Vp(BS_H_dim),V(BS_H_dim)
     integer     :: i_L_,i_c,i_cv,i1,i2
     do i_c=1,3
       if (K_slk%kind=="SLK") then
#if defined _SCALAPACK
         if(i_L_==1) call PARALLEL_M_by_V('c',BS_H_dim,K_inv_slk,BSS_dipoles_PL(:,i_c),V)
         if(i_L_==2) call PARALLEL_M_by_V('n',BS_H_dim,K_inv_slk,conjg(BSS_dipoles_PL(:,i_c)),V)
#endif
       else
         if(i_L_==1) call M_by_V('c',BS_H_dim,Mm1,BSS_dipoles_PL(:,i_c),V)
         if(i_L_==2) call M_by_V('n',BS_H_dim,Mm1,conjg(BSS_dipoles_PL(:,i_c)),V)
       endif
       do i_cv=1,BS_H_dim
         Vp(i_cv)=V(i_cv)*Lo(i_cv,iw,i_L_+Lo_n_loop)
       enddo
       PL(iw,2)=PL(iw,2)+PL_prefactor*Vstar_dot_V(BS_H_dim,V,Vp)
    enddo
   end subroutine
   !
   subroutine EPS_compose(i_L_)
     integer     :: i_L_
     complex(SP) :: V(BS_H_dim)
     if(i_L_==1) then
       if (K_slk%kind=="SLK") then
#if defined _SCALAPACK
         call PARALLEL_M_by_V('n',BS_H_dim,K_inv_slk,BSS_dipoles_opt(1,:),V)
#endif
       else
         call M_by_V_omp('n',BS_H_dim,Mm1,BSS_dipoles_opt(1,:),V)
       endif
       Resp_ii(iw,2)=-Co_factor*Vstar_dot_V_omp(BS_H_dim,BSS_dipoles_opt(1,:),V)
     else if(i_L_==2) then
       if (K_slk%kind=="SLK") then
#if defined _SCALAPACK
         call PARALLEL_M_by_V('n',BS_H_dim,K_inv_slk,conjg(BSS_dipoles_opt(1,:)),V)
#endif
       else
         call M_by_V_omp('n',BS_H_dim,Mm1,conjg(BSS_dipoles_opt(1,:)),V)
       endif
       Resp_ii(iw,2)=Resp_ii(iw,2)-Co_factor*V_dot_V_omp(BS_H_dim,BSS_dipoles_opt(1,:),V)
     endif
   end subroutine
   !
end subroutine K_inversion_engine
