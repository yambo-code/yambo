!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine DEV_SUB(scatter_Gamp)(isc,mode)
 !
 ! INCLUDED in: scatter_Gamp_incl.F
 !
 !
 ! isc%gamp(G,G') = \int_p (region around q) 1/|p+G|/|p+G'|/(2*pi)**3
 !
 ! When mode=='x' the standard Coulomb integral is used. When mode='c' the 
 ! isc%gamp are used multiplied by eps^-1 so, if present, the anisotropy
 ! is incuded.
 !
 use pars,          ONLY:SP,pi
 use drivers,       ONLY:l_col_cut
 use collision_el,  ONLY:elemental_collision
 use R_lattice,     ONLY:d3q_factor,RIM_ng,RIM_qpg,RIM_is_diagonal,&
&                        bare_qpg,RIM_anisotropy,RIM_n_rand_pts,cut_is_slab,&
&                        DEV_VAR(RIM_qpg),DEV_VAR(bare_qpg)
 use devxlib,       ONLY:devxlib_mapped
 use gpu_m
 use timing_m
 !
#include<y_memory.h>
 !
 type(elemental_collision), target::isc
 character(1) ::mode
 !
 !Working Space
 !
 integer :: ig1,ig2,iq,i1,i2
 logical :: l_RIM
 real(SP):: reg_q_m2,q_weight,R_sphere
 !
 complex(SP), pointer DEV_ATTR :: gamp_p(:,:)
 integer :: ng_rows1,ng_rows2,ng_cols1,ng_cols2
 logical :: lpres
#ifdef _GPU_LOC
 character(32) :: subname="scatter_Gamp_gpu"
#else
 character(32) :: subname="scatter_Gamp"
#endif
 !
 call timing(subname,opr="start")
 !
 ! q_weight = 1./(DL_vol*q%nbz)
 !
 q_weight=d3q_factor/(2._SP*pi)**3
 !
 ! Note that
 !
 ! \int_q (region) 1/q^2 /(2*pi)**3 = CONSTANT / (2 pi)**3 (Omega_RL/NQBZ)^1/3
 !                                  = CONSTANT / (2 pi)**3 d3q_factor^1/3
 ! where
 !
 ! CONSTANT = 7.7956 (spherical region)
 ! CONSTANT = 7.44   ("Godby" region)
 !
 ! reg_q_m2 = \int_q (region) 1/q^2 /(2*pi)**3
 !
 reg_q_m2=7.44/(2._SP*pi)**3*d3q_factor**(1._SP/3._SP)
 !
 ! In the case of a spherical region the radius is
 !
 R_sphere=(3._SP/4._SP/pi)**(1._SP/3._SP)*d3q_factor**(1._SP/3._SP)
 !
 iq=isc%qs(2)
 isc%iqref=iq
 !
 ! RIM support ?
 !
 if (.not.allocated(RIM_qpg)) then
   RIM_ng=0
   RIM_n_rand_pts=0
 else
   reg_q_m2=RIM_qpg(iq,1,1)/2._SP
 endif
 !
#ifdef _GPU_LOC
 if (allocated(RIM_qpg).and..not.devxlib_mapped(DEV_VAR(RIM_qpg))) then
   YAMBO_ALLOC_GPU_SOURCE(DEV_VAR(RIM_qpg),RIM_qpg)
 endif
 if (.not.devxlib_mapped(DEV_VAR(bare_qpg))) then
   YAMBO_ALLOC_GPU_SOURCE(DEV_VAR(bare_qpg),bare_qpg)
 endif
#endif
 !
 ! desipite this information is (or should be) already present
 ! in isc%gamp_rows and isc%gamp_cols, here we recompute it
 ! in order to protect for gamp allocations done directly
 ! via YAMBO_ALLOC instead of using the proper isc allocator
 !
 ng_rows1=lbound(DEV_VAR(isc%gamp),1)
 ng_rows2=ubound(DEV_VAR(isc%gamp),1)
 ng_cols1=lbound(DEV_VAR(isc%gamp),2)
 ng_cols2=ubound(DEV_VAR(isc%gamp),2)
 !
 gamp_p => DEV_VAR(isc%gamp)
 !
 !DEV_ACC_DEBUG data present(gamp_p,RIM_qpg,bare_qpg)
 !DEV_ACC parallel loop collapse(2)
 !DEV_CUF kernel do(2)
 !DEV_OMPGPU target map(present,alloc:gamp_p,RIM_qpg,bare_qpg)
 !DEV_OMPGPU teams loop collapse(2)
 !DEV_OMP parallel do default(shared), private(i1,i2,ig1,ig2,l_RIM), collapse(2)
 !
 do i2=ng_cols1,ng_cols2
   do i1=ng_rows1,ng_rows2
     ig1=i1
     ig2=i2
     if (ng_rows1==1.and.ng_rows2==1) ig1=ig2
     if (ng_cols1==1.and.ng_cols2==1) ig2=ig1
     !
     ! RIM support (Both components)
     !
     l_RIM=(ig1<=RIM_ng .and. ig2<=RIM_ng)
     if (RIM_is_diagonal.and.l_RIM) l_RIM=(ig1==ig2)
     !
     if (l_RIM.and.(.not.l_col_cut.or.cut_is_slab)) then
       gamp_p(i1,i2)=DEV_VAR(RIM_qpg)(iq,ig1,ig2)/2._SP
       cycle
     else
       gamp_p(i1,i2)=q_weight/DEV_VAR(bare_qpg)(iq,ig1)/DEV_VAR(bare_qpg)(iq,ig2)
     endif
     !
     ! Head and wings point special treatment (with no RIM only Gamma is possible)
     !
     if ( (RIM_ng==0.and.iq>1) ) cycle
     !
     ! When using the CUTOFF all the components of the Col potential are
     ! already regolarized.
     !
     if (l_col_cut) cycle
     !
     ! Wings (0,G) & (G,0) components using the Sphere approx for the region around Gamma
     ! and the square root approx for other components:
     !
     ! \int_q (region) 1/q /(2*pi)**3 = R_sphere/2. * reg_q_m2
     ! 
     ! for q = 0
     ! 
     ! \int_q (region) 1/q /(2*pi)**3 \sim
     !                 sqrt(\int_q (region) 1/q^2 /(2*pi)**3 ) /
     ! 
     ! for q != 0
     !
     if (ig1==1.and.(ig2>RIM_ng.or.(RIM_is_diagonal.and.ig2>1))) then
       !
       if (iq==1) then
         gamp_p(i1,i2)=R_sphere/2._SP*reg_q_m2/DEV_VAR(bare_qpg)(iq,ig2)
       else
         !
         ! the sqrt(q_weight) is needed as the sqrt(reg_q_m2) contains
         ! implicitly the q_weight
         ! 
         gamp_p(i1,i2)=gamp_p(i1,i2)*&
&                      DEV_VAR(bare_qpg)(iq,ig1)*sqrt(reg_q_m2)/sqrt(q_weight)
       endif
     endif
     if (ig2==1.and.(ig1>RIM_ng.or.(RIM_is_diagonal.and.ig1>1))) then
       if (iq==1) then
         gamp_p(i1,i2)=R_sphere/2._SP*reg_q_m2/DEV_VAR(bare_qpg)(iq,ig1)
       else
         gamp_p(i1,i2)=gamp_p(i1,i2)*&
&                         DEV_VAR(bare_qpg)(iq,ig2)*sqrt(reg_q_m2)/sqrt(q_weight)
       endif
     endif
     !
     ! head component.
     !
     if (ig1==1.and.ig2==1) gamp_p(i1,i2)=reg_q_m2
     !
   enddo
 enddo
 !DEV_OMPGPU end target
 !DEV_ACC_DEBUG end data
 !
 ! Anisotropy correction
 !
 if (.not.l_col_cut.and.ng_rows1==1.and.ng_cols1==1) then
   !DEV_ACC_DEBUG data present(gamp_p)
   !DEV_ACC serial
   !DEV_OMPGPU target map(present,alloc:gamp_p)
   if (all((/iq==1,RIM_ng>0,mode=='c',RIM_anisotropy/=0._SP/))) gamp_p(1,1)=RIM_anisotropy/2._SP
   !DEV_OMPGPU end target
   !DEV_ACC end serial
   !DEV_ACC_DEBUG end data
 endif
 !
 call timing(subname,opr="stop")
 !
end subroutine

