!
! Copyright (C) 2000-2008 A. Marini and the YAMBO team 
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
subroutine WF_rotate(ik,WFo,wf_size)
 !
 ! <r|n'>= <m|n'><r|m>
 !
 ! with <:|n'>=H_sc(:,n')
 !
 ! wf(:,n')= sum_m WFo(:,m) H_sc(m,n')
 !
 ! Extended only to collinear case
 !
 use electrons,      ONLY:n_sp_pol
 use pars,           ONLY:SP
 use wave_func,      ONLY:wf_state,wf_n_states,wf,wf_b
 use SC,             ONLY:SC_R,SC_bands
 implicit none
 !
 integer     :: ik,wf_size
 complex(SP) :: WFo(wf_size,wf_n_states)
 !
 ! Work Space
 !
 integer     :: ib,ibp,ifft,ifftp,i_spin
 complex(SP) :: WS_wf(wf_size,wf_b(2))
 !
 do i_spin=1,n_sp_pol
   WS_wf=(0.,0.)
   !
   do ib  = wf_b(1),min(wf_b(2),SC_bands)
     ifft = wf_state(ib,ik,i_spin)
     do ibp = wf_b(1),min(wf_b(2),SC_bands)
       ifftp = wf_state(ibp,ik,i_spin)
       !
       if (ifft==0.or.ifftp==0) cycle
       !
#if defined _DOUBLE
       call zaxpy(wf_size,SC_R(ibp,ib,ik,i_spin),WFo(:,ifftp),1,WS_wf(:,ib),1)
#else
       call caxpy(wf_size,SC_R(ibp,ib,ik,i_spin),WFo(:,ifftp),1,WS_wf(:,ib),1)
#endif
     end do
   end do
   !
   do ib=wf_b(1),min(wf_b(2),SC_bands)
     wf(:,wf_state(ib,ik,i_spin))=WS_wf(:,ib)
   enddo
   !
 enddo
 !
end subroutine
