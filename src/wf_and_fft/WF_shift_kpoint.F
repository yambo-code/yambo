!
!        Copyright (C) 2000-2020 the YAMBO team
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
 use R_lattice,     ONLY:G_m_G,G_m_G_d,k_map,bz_samp
 use wave_func,     ONLY:wf_ng_1st_BZ,wf_ng_overlaps
 use deviceXlib_m,  ONLY:dev_memcpy
 !
 implicit none
 !
 integer,       intent(in) :: wf_shift(3),ikbz,b_to_shift(2)
 type(bz_samp), intent(in) :: Xk
 complex(SP), intent(in)  DEV_ATTRIBUTE :: WF_k_in (wf_ng_1st_BZ,  n_spinor,b_to_shift(1):b_to_shift(2),n_sp_pol)
 complex(SP), intent(out) DEV_ATTRIBUTE :: WF_k_out(wf_ng_overlaps,n_spinor,b_to_shift(1):b_to_shift(2),n_sp_pol)
 !
 ! Work space
 !
 integer :: id,ik,is
 complex(SP) DEV_ATTRIBUTE :: WF_tmp(wf_ng_overlaps,n_spinor,b_to_shift(1):b_to_shift(2),n_sp_pol)
 integer :: ig,igp,i_b_ref
 integer :: g0_idx(3,2),g0_idx_val
 integer :: i_spinor,i_b,i_sp_pol
 !
 ik = Xk%sstar(ikbz,1)
 is = Xk%sstar(ikbz,2)
 !
 i_b_ref=b_to_shift(1)
 !
 WF_tmp=cZERO
 call dev_memcpy(WF_tmp,WF_k_in, range1=(/1,wf_ng_1st_BZ/))
 !
 do id=1,3
   !
   WF_k_out=cZERO
   !
   if(WF_shift(id)/=0) then
     !
     g0_idx_val=k_map%g0_idx(id,WF_shift(id))
     !
#if defined _CUDA
     !$cuf kernel do(4) <<<*,*>>>
#endif
     do i_sp_pol=1,n_sp_pol
     do i_b=b_to_shift(1),b_to_shift(2)
     do i_spinor=1,n_spinor
     do ig=1,wf_ng_1st_BZ
       !
       if(WF_tmp(ig,1,i_b_ref,1)==cZERO) cycle
       igp=DEV_VARNAME(G_m_G)(ig,g0_idx_val)
       WF_k_out(igp,i_spinor,i_b,i_sp_pol)=WF_tmp(ig,i_spinor,i_b,i_sp_pol)
       !
     enddo
     enddo
     enddo
     enddo
     !
   else
     call dev_memcpy(WF_k_out,WF_tmp)
   endif
   !
   WF_tmp  =cZERO
   call dev_memcpy(WF_tmp,WF_k_out)
   !
 enddo
 !
end subroutine DEV_SUBNAME_ALT(WF_shift_kpoint)

