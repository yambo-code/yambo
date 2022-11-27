!
!        Copyright (C) 2000-2022 the YAMBO team
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
#if defined _SC
subroutine WF_rotate(ik,i_sp_pol,WFo,wf_size,space)
 !
 ! <r|n'>= <m|n'><r|m>
 !
 ! with <m|n'>=SC_R(m,n')
 !      <:|m>=WFo(:,m)
 !
 ! wf(:,n')= sum_m WFo(:,m) SC_R(m,n')
 !
 use pars,           ONLY:cZERO
 use electrons,      ONLY:n_spinor
 use pars,           ONLY:SP
 use wave_func,      ONLY:WAVEs,WF
 use hamiltonian,    ONLY:H_rotation,H_ref_bands
 use cuda_m,         ONLY:have_cuda
 !
 implicit none
 !
 integer,      intent(in)    :: i_sp_pol,ik,wf_size
 type(WAVEs),  intent(inout) :: WFo
 character(1), intent(in)    :: space
 !
 ! Work Space
 !
 integer     :: ib,ibp,i_wf,i_wfp,i_spinor,i_c
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
     if (space=="G") WS_wf(:,:,ib)=WS_wf(:,:,ib)+H_rotation(ibp,ib,ik,i_sp_pol)*WFo%c(:,:,i_wfp)
     if (space=="R") WS_wf(:,:,ib)=WS_wf(:,:,ib)+H_rotation(ibp,ib,ik,i_sp_pol)*WFo%r(:,:,i_wfp)
   end do
   !
 enddo
 !
 do ib = H_ref_bands(1),H_ref_bands(2)
   !
   i_wf = WFo%index(ib,ik,i_sp_pol)
   if (i_wf==0) cycle
   !
   if (space=="G") then
     !
     !$omp parallel do default(shared), private(i_c,i_spinor)
     do i_spinor=1,n_spinor
       do i_c=1,wf_size
         WF%c(i_c,i_spinor,i_wf)=WS_wf(i_c,i_spinor,ib)
       enddo
     enddo
     !$omp end parallel do
     !
     if (have_cuda) WF%c_d(:,:,i_wf) = WF%c(:,:,i_wf)
     !
   else if(space=="R") then
     !
     !$omp parallel do default(shared), private(i_c,i_spinor)
     do i_spinor=1,n_spinor
       do i_c=1,wf_size
         WF%r(i_c,i_spinor,i_wf)=WS_wf(i_c,i_spinor,ib)
       enddo
     enddo
     !$omp end parallel do
     !
     if (have_cuda) WF%r_d(:,:,i_wf) = WF%r(:,:,i_wf)
     !
   endif
   !
 enddo
 !
end subroutine
#endif
