!
!        Copyright (C) 2000-2016 the YAMBO team
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
subroutine K_inversion_engine(iw_2start,iw_2do,px,W,Lo,Lo_dim,Lo_n_loop,do_it_full)
 !
 use pars,           ONLY:SP,cZERO
 use parallel_m,     ONLY:PP_indexes
 use frequency,      ONLY:w_samp
 use X_m,            ONLY:Epsilon_ii,global_gauge
 use LIVE_t,         ONLY:live_timing
 use R_lattice,      ONLY:q_norm
 use wrapper,        ONLY:Vstar_dot_V,M_by_V,V_dot_V
 use BS,             ONLY:BS_mat,BS_H_dim,Co_factor,BSS_INV_err,BSS_dipoles
#if defined _PL
 use PHOTOLUM,       ONLY:PL,PL_prefactor,BSS_dipoles_PL
#endif
 !
 implicit none
 !
 integer          :: iw_2start,iw_2do,Lo_dim,Lo_n_loop
 type(PP_indexes) :: px
 type(w_samp)     :: W
 complex(SP)      :: Lo(BS_H_dim,W%n_freqs,Lo_dim)
 logical          :: do_it_full
 !
 ! Work Space
 !
 integer      :: i2,iw,i_L
 logical      :: perform_inv
 character(1) :: Gr
 complex(SP)              :: gauge_factor
 complex(SP), allocatable :: Mm1(:,:),V(:)
#if defined _PL
 complex(SP), allocatable :: Vp(:)
#endif
 integer, external::MATRIX_resolvant_via_perturbative_inversion
 !
 if (do_it_full) then
   allocate(Mm1(BS_H_dim,BS_H_dim),V(BS_H_dim))
   Mm1=cZERO
   V  =cZERO
#if defined _PL
   allocate(Vp(BS_H_dim))
   Vp =cZERO
#endif
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
       BSS_inv_err(iw)=0
       cycle
     endif
     !
   endif
   !
   if (     do_it_full) perform_inv= BSS_INV_err(iw)/= 0
   if (.not.do_it_full) perform_inv= BSS_INV_err(iw)==-1
   !
   if (trim(global_gauge)=='length'  ) gauge_factor=-Co_factor/q_norm(1)**2
   if (trim(global_gauge)=='velocity') gauge_factor=-Co_factor/ W%p(iw) **2
   !       
   if (perform_inv) then
     !
     if (do_it_full) then
       !
       BSS_INV_err(iw)=0
       !
       do i_L=1,Lo_n_loop
         !
         if (i_L==1) Gr="r"
         if (i_L==2) Gr="a"
         call K_invertion_do_it_full(Lo(:,iw,i_L),Mm1,Gr)
         !
         call EPS_compose(i_L)
         !
#if defined _PL
         !
         call PL_compose(i_L)
         !
#endif
         !
       enddo
       !
     else
       !
       BSS_INV_err(iw)=MATRIX_resolvant_via_perturbative_inversion(BS_H_dim,Lo_dim,Lo_n_loop,BSS_dipoles,gauge_factor,&
&                                                                  Epsilon_ii(iw,2),Lo(:,iw,:Lo_dim),BS_mat)
       !
     endif
     !
   endif       
   !
   if (.not.do_it_full) call live_timing(steps=1)
   !
 enddo
 !
 if (do_it_full) then
   deallocate(Mm1,V)
#if defined _PL
   deallocate(Vp)
#endif
 else
   call live_timing()
 endif
 !
 contains
   !
#if defined _PL
   subroutine PL_compose(i_L_)
     integer :: i_L_,i_c,i_cv,i1,i2
     do i_c=1,3
       if(i_L_==1) then
         !
         call M_by_V('c',BS_H_dim,Mm1,BSS_dipoles_PL(:,i_c),V)
         do i_cv=1,BS_H_dim
           V(i_cv)=V(i_cv)*Lo(i_cv,iw,i_L_+Lo_n_loop)
         enddo
         call M_by_V('n',BS_H_dim,Mm1,V,Vp)
         PL(iw,2)=PL(iw,2)+PL_prefactor*Vstar_dot_V(BS_H_dim,BSS_dipoles_PL(:,i_c),Vp)
         !
       else if(i_L_==2) then
         !
         call M_by_V('n',BS_H_dim,Mm1,conjg(BSS_dipoles_PL(:,i_c)),V)
         do i_cv=1,BS_H_dim
           V(i_cv)=V(i_cv)*Lo(i_cv,iw,i_L_+Lo_n_loop)
         enddo
         call M_by_V('c',BS_H_dim,Mm1,V,Vp)
         PL(iw,2)=PL(iw,2)+PL_prefactor*V_dot_V(BS_H_dim,BSS_dipoles_PL(:,i_c),Vp)
         !
       endif
    enddo
   end subroutine
#endif
   !
   subroutine EPS_compose(i_L_)
     integer :: i_L_
     if(i_L_==1) then
       call M_by_V('n',BS_H_dim,Mm1,BSS_dipoles,V)
       Epsilon_ii(iw,2)=gauge_factor*Vstar_dot_V(BS_H_dim,BSS_dipoles,V)
     else if(i_L_==2) then
       call M_by_V('n',BS_H_dim,Mm1,conjg(BSS_dipoles),V)
       Epsilon_ii(iw,2)=Epsilon_ii(iw,2)+gauge_factor*Vstar_dot_V(BS_H_dim,conjg(BSS_dipoles),V)
     endif
   end subroutine
   !
end subroutine K_inversion_engine
