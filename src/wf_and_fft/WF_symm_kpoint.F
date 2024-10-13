!
! License-Identifier: GPL
!
! Copyright (C) 2015 The Yambo Team
!
! Authors (see AUTHORS file for details): MG CA DS AF
!
subroutine DEV_SUB_ALT(WF_symm_kpoint)(b_to_load,ikbz,i_sp_pol,Xk,WF_k_out)
 !
 ! INCLUDED in: WF_symm_kpoint_incl.F 
 !
 use pars,          ONLY:SP,cZERO
 use electrons,     ONLY:n_sp_pol,n_spinor
 use wave_func,     ONLY:WF,wf_ng,wf_ng_1st_BZ
 use R_lattice,     ONLY:bz_samp,DEV_VAR(g_rot)
 use D_lattice,     ONLY:idt_index,DEV_VAR(spin_sop),i_time_rev,nsym
 use deviceXlib_m,  ONLY:dev_conjg,dev_memcpy
 !
 implicit none
 !
 integer,       intent(in)  :: ikbz,i_sp_pol,b_to_load(2)
 type(bz_samp), intent(in)  :: Xk
 complex(SP),   intent(out) DEV_ATTR :: WF_k_out(wf_ng_1st_BZ,n_spinor,b_to_load(1):b_to_load(2))
 !
 ! Work space
 !
 complex(SP), pointer DEV_ATTR :: WF_p(:,:,:)
 integer :: ik,is,ib,iwf
 integer :: i_g,i_spinor
 !
 ik = Xk%sstar(ikbz,1)
 is = Xk%sstar(ikbz,2)
 !
 WF_k_out=cZERO
 WF_p => DEV_VAR(WF%c)
 !
 if(is==idt_index) then
   !  
     do ib=b_to_load(1),b_to_load(2)
       !
       iwf=WF%index(ib,ik,i_sp_pol)
       call dev_memcpy(WF_k_out(:,:,ib),DEV_VAR(WF%c)(:,:,iwf))
       !
     enddo
   !
 else
   !
     do ib=b_to_load(1),b_to_load(2)
       !
       iwf=WF%index(ib,ik,i_sp_pol)
       !
       if (n_spinor==1) then
         !
         !DEV_CUF kernel do(1) <<<*,*>>>
         do i_g=1,wf_ng
           WF_k_out(DEV_VAR(g_rot)(i_g,is),1,ib)=WF_p(i_g,1,iwf)
         enddo
         !
       else
         !
         !DEV_CUF kernel do(1) <<<*,*>>>
         do i_g=1,wf_ng
           WF_k_out(DEV_VAR(g_rot)(i_g,is),1,ib)=DEV_VAR(spin_sop)(1,1,is)*WF_p(i_g,1,iwf)+ &
&                                                DEV_VAR(spin_sop)(1,2,is)*WF_p(i_g,2,iwf)
           WF_k_out(DEV_VAR(g_rot)(i_g,is),2,ib)=DEV_VAR(spin_sop)(2,1,is)*WF_p(i_g,1,iwf)+ &
&                                                DEV_VAR(spin_sop)(2,2,is)*WF_p(i_g,2,iwf)
         enddo
       endif
       !
     enddo
   !
   if(is>nsym/(1+i_time_rev)) call dev_conjg(WF_k_out)
   !
 endif
 !
end subroutine DEV_SUB_ALT(WF_symm_kpoint)

