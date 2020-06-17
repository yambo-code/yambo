!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): MG, CA, DS, AF
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
subroutine DEV_SUB_ALT(WF_symm_kpoint)(b_to_load,ikbz,Xk,WF_k_out)
 !
 use pars,          ONLY:SP,cZERO
 use electrons,     ONLY:n_sp_pol,n_spinor
 use wave_func,     ONLY:WF,wf_ng,wf_ng_1st_bz
 use R_lattice,     ONLY:bz_samp,DEV_VAR(g_rot)
 use D_lattice,     ONLY:idt_index,DEV_VAR(spin_sop),i_time_rev,nsym
 use deviceXlib_m,  ONLY:dev_conjg,dev_memcpy
 !
 implicit none
 !
 integer,       intent(in)  :: ikbz,b_to_load(2)
 type(bz_samp), intent(in)  :: Xk
 complex(SP),   intent(out) DEV_ATTR :: WF_k_out(wf_ng_1st_BZ,n_spinor,b_to_load(1):b_to_load(2),n_sp_pol)
 !
 ! Work space
 !
 complex(SP), pointer DEV_ATTR :: WF_p(:,:,:)
 integer :: ik,is,i_sp_pol,ib,iwf
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
   do i_sp_pol=1,n_sp_pol
     do ib=b_to_load(1),b_to_load(2)
       iwf=WF%index(ib,ik,i_sp_pol)
       call dev_memcpy(WF_k_out(:,:,ib,i_sp_pol),DEV_VAR(WF%c)(:,:,iwf))
     enddo
   enddo
   !
 else
   !
   do i_sp_pol=1,n_sp_pol
     do ib=b_to_load(1),b_to_load(2)
       !
       iwf=WF%index(ib,ik,i_sp_pol)
       !
       if (n_spinor==1) then
         !
#ifdef _CUDA_LOC
         !$cuf kernel do(2) <<<*,*>>>
#endif
         do i_spinor=1,n_spinor
         do i_g=1,wf_ng
           WF_k_out(DEV_VAR(g_rot)(i_g,is),i_spinor,ib,i_sp_pol)=WF_p(i_g,i_spinor,iwf)
         enddo
         enddo
         !
       else
         !
#ifdef _CUDA_LOC
         !$cuf kernel do(1) <<<*,*>>>
#endif
         do i_g=1,wf_ng
           WF_k_out(DEV_VAR(g_rot)(i_g,is),1,ib,i_sp_pol)=DEV_VAR(spin_sop)(1,1,is)*WF_p(i_g,1,iwf)+ &
&                                                         DEV_VAR(spin_sop)(1,2,is)*WF_p(i_g,2,iwf)
           WF_k_out(DEV_VAR(g_rot)(i_g,is),2,ib,i_sp_pol)=DEV_VAR(spin_sop)(2,1,is)*WF_p(i_g,1,iwf)+ &
&                                                         DEV_VAR(spin_sop)(2,2,is)*WF_p(i_g,2,iwf)
         enddo
       endif
       !
     enddo
   enddo
   !
   if(is>nsym/(1+i_time_rev)) call dev_conjg(WF_k_out)
   !
 endif
 !
end subroutine DEV_SUB_ALT(WF_symm_kpoint)

