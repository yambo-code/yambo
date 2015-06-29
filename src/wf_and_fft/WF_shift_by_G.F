!
! Copyright (C) 2000-2013 C. Attaccalite and the YAMBO team 
!              http://www.yambo-code.org
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
subroutine WF_shift_by_G(isc,WF_shifted,shift_vec)
 !
 use pars,          ONLY:SP,cZERO
 use electrons,     ONLY:n_spinor
 use R_lattice,     ONLY:ng_closed,G_m_G,k_map
 use wave_func,     ONLY:WF,wf_ng,wf_nc_k,wf_igk
 !
 implicit none
 !
 integer,     intent(in)  :: isc(3),shift_vec(3)
 complex(SP), intent(out) :: WF_shifted(wf_ng,n_spinor)
 !
 ! Work Space
 !
 integer           :: i_wf(n_spinor),ic,ig,igp,i_spinor,iG0,wf_ng_eff,ik
 integer, external :: eval_G_minus_G
 !
 ik     =isc(2)
 i_wf(1)=WF%index(isc(1),isc(2),isc(3))
 if(n_spinor==2)  i_wf(n_spinor)=WF%index(isc(1),isc(2),n_spinor)
 !
 if(all(shift_vec==0)) then
   do i_spinor=1,n_spinor
     WF_shifted(:,i_spinor)=WF%c(:,i_wf(i_spinor))
   enddo
   return
 else
   WF_shifted=cZERO
 endif
 !
 if(all(k_map%g0_plus_idx==-1)) call k_find_smallest_g0(k_map)
 !
 if(allocated(G_m_G).and.size(G_m_G)==1) deallocate(G_m_G)
 !
 if(.not.allocated(G_m_G)) then
   iG0=max(maxval(k_map%g0_plus_idx(:,2)),maxval(k_map%g0_minus_idx(:,2)))
   wf_ng_eff=eval_G_minus_G(minval((/ng_closed,wf_ng/)),iG0)
 else
   wf_ng_eff=size(G_m_G(:,1))
 endif
 !
 do ic=1,wf_nc_k(ik)
   ig=wf_igk(ic,ik)
   if(ig>wf_ng_eff) cycle
   igp=G_m_G(ig,k_map%g_idx(shift_vec(1),shift_vec(2),shift_vec(3)))
   if(igp>wf_ng.or.ig>wf_ng) cycle
   do i_spinor=1,n_spinor
     WF_shifted(igp,i_spinor)=WF%c(ig,i_wf(i_spinor))
   enddo
 enddo
 !
 return
 !
end subroutine WF_shift_by_G
