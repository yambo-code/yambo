!
!        Copyright (C) 2000-2018 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM, DS
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
 use hamiltonian,    ONLY:SC_R,H_ref_bands
 !
 implicit none
 !
 integer     :: i_sp_pol,ik,wf_size
 type(WAVEs) :: WFo
 !
 ! Work Space
 !
 integer     :: ib,ibp,i_wf,i_wfp,i_spinor,ig
 complex(SP) :: WS_wf(wf_size,n_spinor,H_ref_bands(1):H_ref_bands(2))
 !
 WS_wf=cZERO
 !
 do ib = H_ref_bands(1),H_ref_bands(2)
   !
   i_wf = WFo%index(ib,ik,i_sp_pol)
   if (i_wf==0) cycle
   !
   do ibp = H_ref_bands(1),H_ref_bands(2)
     i_wfp = WFo%index(ibp,ik,i_sp_pol)
     if (i_wfp==0) cycle
     !call V_by_V_plus_V_omp(wf_size*n_spinor,SC_R(ibp,ib,ik,i_sp_pol),WFo%c(:,:,i_wfp),WS_wf(:,:,ib))
     WS_wf(:,:,ib)=WS_wf(:,:,ib)+SC_R(ibp,ib,ik,i_sp_pol)*WFo%c(:,:,i_wfp)
   end do
   !
 enddo
 !
 do ib = H_ref_bands(1),H_ref_bands(2)
   !
   i_wf = WFo%index(ib,ik,i_sp_pol)
   !
   !$omp parallel do default(shared), private(ig,i_spinor)
   do i_spinor=1,n_spinor
     do ig=1,wf_size
       WF%c(ig,i_spinor,i_wf)=WS_wf(ig,i_spinor,ib)
     enddo
   enddo
   !$omp end parallel do
   !
 enddo
 !
end subroutine
