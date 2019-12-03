!
!        Copyright (C) 2000-2019 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): MG, CA
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
#include<dev_defs.h>
!
subroutine DEV_SUBNAME_ALT(WF_shift_kpoint)(b_to_shift,ikbz,wf_shift,Xk,WF_k_in,WF_k_out)
 !
 use pars,          ONLY:SP,cZERO
 use electrons,     ONLY:n_spinor,n_sp_pol
 use R_lattice,     ONLY:G_m_G,k_map,bz_samp
#ifdef _CUDA
 use R_lattice,     ONLY:G_m_G_d
#endif
 use wave_func,     ONLY:wf_ng
 use interfaces,    ONLY:eval_G_minus_G
 use deviceXlib_m,  ONLY:dev_memcpy
 !
 implicit none
 !
 integer,       intent(in) :: wf_shift(3),ikbz,b_to_shift(2)
 type(bz_samp), intent(in) :: Xk
 complex(SP), intent(out)  :: WF_k_out(wf_ng,n_spinor,b_to_shift(1):b_to_shift(2),n_sp_pol)
 complex(SP), intent(in)   :: WF_k_in (wf_ng,n_spinor,b_to_shift(1):b_to_shift(2),n_sp_pol)
#ifdef _CUDA
 attributes(device) :: WF_k_out,WF_k_in
#endif
 !
 ! Work space
 !
 integer :: id,ik,is
 complex(SP) DEV_ATTRIBUTE :: WF_tmp(wf_ng,n_spinor,b_to_shift(1):b_to_shift(2),n_sp_pol)
 integer           :: ig,igp,iG0,ng_max
 integer           :: g0_idx(3,2)
 real(SP)          :: g0_length(3)
#ifdef _CUDA
 integer :: i_spinor,i_b,i_sp_pol
 integer :: g0_idx_val
#endif
 !
 ik = Xk%sstar(ikbz,1)
 is = Xk%sstar(ikbz,2)
 !
 call dev_memcpy(WF_tmp,WF_k_in)
 !
 if(all(k_map%g0_idx==-1)) then
   call k_find_smallest_g0(g0_idx,g0_length)
 else
   g0_idx   =k_map%g0_idx
   g0_length=k_map%g0_length
 endif
 !
 ! Generate G_m_G if it is not present
 !
 iG0=max(maxval(g0_idx(:,1)),maxval(g0_idx(:,2)))
 ng_max=eval_G_minus_G(wf_ng,iG0)
 !
 !
 do id=1,3
   !
   if(WF_shift(id)/=0) then
     !
#if defined _CUDA
     g0_idx_val=g0_idx(id,WF_shift(id))
     !
     !$cuf kernel do(4) <<<*,*>>>
     do i_sp_pol=1,n_sp_pol
     do i_b=b_to_shift(1),b_to_shift(2)
     do i_spinor=1,n_spinor
     do ig=1,min(wf_ng,ng_max)
       !
       igp=G_m_G_d(ig,g0_idx_val)
       if(igp>wf_ng.or.igp>ng_max) cycle
       WF_k_out(igp,i_spinor,i_b,i_sp_pol)=WF_tmp(ig,i_spinor,i_b,i_sp_pol)
       !
     enddo
     enddo
     enddo
     enddo
     !
#else
     do ig=1,min(wf_ng,ng_max)
       igp=G_m_G(ig,g0_idx(id,WF_shift(id)))
       if(igp>wf_ng.or.ig>wf_ng) cycle
       WF_k_out(igp,:,:,:)=WF_tmp(ig,:,:,:)
     enddo
#endif
   else
     call dev_memcpy(WF_k_out,WF_tmp)
   endif
   !
   call dev_memcpy(WF_tmp,WF_k_out)
   !
 enddo
 !
end subroutine DEV_SUBNAME_ALT(WF_shift_kpoint)

