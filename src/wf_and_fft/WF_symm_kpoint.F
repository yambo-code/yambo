!
! License-Identifier: GPL
!
! Copyright (C) 2015 The Yambo Team
!
! Authors (see AUTHORS file for details): MG CA DS AF
!
subroutine DEV_SUB(WF_symm_kpoint)(b_to_load,ikbz,i_sp_pol,Xk,WF_k_out)
 !
 ! INCLUDED in: WF_symm_kpoint_incl.F 
 !
 use pars,          ONLY:SP,cZERO
 use electrons,     ONLY:n_sp_pol,n_spinor
 use wave_func,     ONLY:WF,wf_ng,wf_ng_1st_BZ
 use R_lattice,     ONLY:bz_samp,DEV_VAR(g_rot)
 use D_lattice,     ONLY:idt_index,DEV_VAR(spin_sop),i_time_rev,nsym
 use devxlib,       ONLY:devxlib_conjg_d,devxlib_memcpy_d2d,devxlib_memset_d
 !
 implicit none
 !
 integer,       intent(in)  :: ikbz,i_sp_pol,b_to_load(2)
 type(bz_samp), intent(in)  :: Xk
 complex(SP),   intent(out) DEV_ATTR :: WF_k_out(wf_ng_1st_BZ,n_spinor,b_to_load(2)-b_to_load(1)+1)
 !
 ! Work space
 !
 complex(SP), pointer DEV_ATTR :: WF_p(:,:,:)
 integer :: ik,is,ib,ibl,iwf
 integer :: i_g,i_spinor
 !
 ik = Xk%sstar(ikbz,1)
 is = Xk%sstar(ikbz,2)
 !
#if defined _GPU_LOC
 call devxlib_memset_d(WF_k_out,cZERO)
#else
 WF_k_out=cZERO
#endif
 WF_p => DEV_VAR(WF%c)
 !
 if(is==idt_index) then
   !  
     do ib=b_to_load(1),b_to_load(2)
       !
       iwf=WF%index(ib,ik,i_sp_pol)
       ibl=ib-b_to_load(1)+1
       !
#if defined _GPU_LOC
       ! dev2dev
       call devxlib_memcpy_d2d(WF_k_out(:,:,ibl),DEV_VAR(WF%c)(:,:,iwf))
#else
       WF_k_out(:,:,ibl)=WF_p(:,:,iwf)
#endif
       !
     enddo
   !
 else
   !
     do ib=b_to_load(1),b_to_load(2)
       !
       iwf=WF%index(ib,ik,i_sp_pol)
       ibl=ib-b_to_load(1)+1
       !
       if (n_spinor==1) then
         !
         !DEV_ACC_DEBUG data present(WF_k_out,g_rot,WF_p)
         !DEV_ACC parallel loop collapse(2)
         !DEV_CUF kernel do(2) <<<*,*>>>
         !DEV_OMPGPU target map(present,alloc:WF_k_out,g_rot,WF_p)
         !DEV_OMPGPU teams loop collapse(2)
         do i_spinor=1,n_spinor
         do i_g=1,wf_ng
           WF_k_out(DEV_VAR(g_rot)(i_g,is),i_spinor,ibl)=WF_p(i_g,i_spinor,iwf)
         enddo
         enddo
         !DEV_OMPGPU end target
         !DEV_ACC_DEBUG end data
         !
       else
         !
         !DEV_ACC_DEBUG data present(WF_k_out,g_rot,WF_p,spin_sop)
         !DEV_ACC parallel loop
         !DEV_CUF kernel do(1) <<<*,*>>>
         !DEV_OMPGPU target map(present,alloc:WF_k_out,g_rot,WF_p,spin_sop)
         !DEV_OMPGPU teams loop
         do i_g=1,wf_ng
           WF_k_out(DEV_VAR(g_rot)(i_g,is),1,ibl)=DEV_VAR(spin_sop)(1,1,is)*WF_p(i_g,1,iwf)+ &
&                                                 DEV_VAR(spin_sop)(1,2,is)*WF_p(i_g,2,iwf)
           WF_k_out(DEV_VAR(g_rot)(i_g,is),2,ibl)=DEV_VAR(spin_sop)(2,1,is)*WF_p(i_g,1,iwf)+ &
&                                                 DEV_VAR(spin_sop)(2,2,is)*WF_p(i_g,2,iwf)
         enddo
         !DEV_OMPGPU end target
         !DEV_ACC_DEBUG end data
       endif
       !
     enddo
   !
#ifdef _GPU_LOC
   if(is>nsym/(1+i_time_rev)) call devxlib_conjg_d(WF_k_out)
#else
   if(is>nsym/(1+i_time_rev)) WF_k_out=conjg(WF_k_out)
#endif
   !
 endif
 !
end subroutine DEV_SUB(WF_symm_kpoint)

