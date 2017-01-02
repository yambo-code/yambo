!
!        Copyright (C) 2000-2017 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM
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
subroutine WF_rotate(ik,i_sp_pol,WFo,wf_size)
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
 use pars,           ONLY:cZERO
 use wrapper_omp,    ONLY:V_by_V_plus_V_omp
 use electrons,      ONLY:n_sp_pol,n_spinor
 use pars,           ONLY:SP
 use wave_func,      ONLY:WAVEs,WF
 use SC,             ONLY:SC_R,SC_bands
 !
 implicit none
 !
 integer     :: ik,i_sp_pol,wf_size,ig
 type(WAVEs) :: WFo
 !
 ! Work Space
 !
 integer     :: ib,ibp,i_wf(n_spinor),i_wfp(n_spinor),i_spin,i_spinor
 complex(SP) :: WS_wf(wf_size,n_spinor,SC_bands(2))
 !
   WS_wf=cZERO
   !
   do ib = 1,SC_bands(2)
     !
     do i_spinor=1,n_spinor
       i_spin=i_sp_pol+i_spinor-1
       i_wf(i_spinor) = WFo%index(ib,ik,i_spin)
     enddo
     !
     if (any(i_wf(:)==0)) cycle
     !
     do ibp = 1,SC_bands(2)
       !
       do i_spinor=1,n_spinor
         i_spin=i_sp_pol+i_spinor-1
         i_wfp(i_spinor) = WFo%index(ibp,ik,i_spin)
         !
         if (i_wfp(i_spinor)==0) cycle
         !
         call V_by_V_plus_V_omp(wf_size,SC_R(ibp,ib,ik,i_sp_pol),WFo%c(:,i_wfp(i_spinor)),WS_wf(:,i_spinor,ib))
         !
       end do
     end do
     !
   enddo
   !
#if defined _OPENMP
!$omp parallel default(shared), private(ig,ib)
#endif
   do ib=1,SC_bands(2)
#if defined _OPENMP
!$omp do
#endif
     !
     do i_spinor=1,n_spinor
       i_spin=i_sp_pol+i_spinor-1
       i_wf(i_spinor) = WF%index(ib,ik,i_spin)
       !
       do ig=1,wf_size
         WF%c(ig,i_wf(i_spinor))=WS_wf(ig,i_spinor,ib)
       enddo
     enddo
#if defined _OPENMP
!$omp end do
#endif
   enddo
#if defined _OPENMP
!$omp end parallel
#endif
 !
end subroutine
