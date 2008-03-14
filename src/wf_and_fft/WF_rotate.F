!
! Copyright (C) 2000-2005 A. Marini and the SELF team 
!         http://www.fisica.uniroma2.it/~self
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
 use pars,           ONLY:SP
 use wave_func,      ONLY:wf_state,wf_n_states,wf,wf_b
 use SC,             ONLY:SC_R
 implicit none
 !
 integer     :: ik,wf_size
 complex(SP) :: WFo(wf_size,wf_n_states)
 !
 ! Work Space
 !
 integer     :: ib,ibp,ifft,ifftp
 complex(SP) :: WS_wf(wf_size,wf_b(2))
 !
 WS_wf=(0.,0.)
 !
 do ib  = wf_b(1),wf_b(2)
   ifft = wf_state(ib,ik,1)
   do ibp = wf_b(1),wf_b(2)
     ifftp = wf_state(ibp,ik,1)
     !
     if (ifft==0.or.ifftp==0) cycle
     !
     call caxpy(wf_size,SC_R(ibp,ib,ik),WFo(:,ifftp),1,WS_wf(:,ib),1)
   end do
 end do
 !
 do ib=wf_b(1),wf_b(2)
   wf(:,wf_state(ib,ik,1))=WS_wf(:,ib)
 enddo
 !
end subroutine
