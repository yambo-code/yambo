!
! Copyright (C) 2000-2013 A. Marini and the YAMBO team 
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
 ! with <m|n'>=SC_R(m,n')
 !      <:|m>=WFo(:,m)
 !
 ! wf(:,n')= sum_m WFo(:,m) SC_R(m,n')
 !
 ! Extended only to collinear case
 !
 use wrapper,        ONLY:V_by_V_plus_V
#if defined _OPENMP
 use wrapper,        ONLY:V_by_V_plus_V_omp
#endif
 use electrons,      ONLY:n_sp_pol
 use pars,           ONLY:SP
 use wave_func,      ONLY:WAVEs,WF
 use SC,             ONLY:SC_R,SC_bands
 implicit none
 !
 integer     :: ik,wf_size,ig
 type(WAVEs) :: WFo
 !
 ! Work Space
 !
 integer     :: ib,ibp,ifft,ifftp,i_spin
 complex(SP) :: WS_wf(wf_size,SC_bands(2))
 !
 do i_spin=1,n_sp_pol
   WS_wf=(0.,0.)
   !
   do ib  = 1,SC_bands(2)
     ifft = WFo%index(ib,ik,i_spin)
     do ibp = 1,SC_bands(2)
       ifftp = WFo%index(ibp,ik,i_spin)
       !
       if (ifft==0.or.ifftp==0) cycle
       !
#if defined _OPENMP
       call V_by_V_plus_V_omp(wf_size,SC_R(ibp,ib,ik,i_spin),WFo%c(:,ifftp),WS_wf(:,ib))
#else
       call V_by_V_plus_V(wf_size,SC_R(ibp,ib,ik,i_spin),WFo%c(:,ifftp),WS_wf(:,ib))
#endif
       !
     end do
   end do
   !
!$omp parallel default(shared), private(ig,ib)
   do ib=1,SC_bands(2)
!$omp do
     do ig=1,wf_size
        WF%c(ig,WF%index(ib,ik,i_spin))=WS_wf(ig,ib)
     enddo
!$omp end do
   enddo
!$omp end parallel
   !
 enddo
 !
end subroutine
