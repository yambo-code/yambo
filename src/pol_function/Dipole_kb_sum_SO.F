!
! Copyright (C) 2000-2010 D. Sangalli, A. Marini and the YAMBO team 
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
subroutine Dipole_kb_sum_SO(iv,ic,ik,i_spin,rho,kbv)
 !
 use pars,          ONLY:SP,DP
 use wrapper,       ONLY:Vstar_dot_V,V_dot_V,V_by_V_plus_V
 use wave_func,     ONLY:wf,wf_ng,wf_state
 use pseudo,        ONLY:pp_kbv_dim,kbv_spin
 implicit none
 !
 integer    :: iv,ic,ik,i_spin
 complex(SP):: rho(3),kbv(wf_ng,pp_kbv_dim,7)
 ! 
 ! Work Space
 !
 integer     :: i_pp,i_dir
 integer     :: ivfft_up,ivfft_dw,icfft_up,icfft_dw
 integer     :: ivfft1, icfft1, icfft2
 !
 ! [r,\Delta V^{SO}] L*S
 complex(SP) :: A1(wf_ng),A2(wf_ng)
 complex(SP) :: A1_c     ,A2_c
 complex(SP) :: K_c(wf_ng,3)
 !
 ! \Delta V^{SO} [r,L*S] = \Delta V^{SO} * r x S
 complex(SP) :: B_v
 complex(SP) :: C1  (wf_ng,3), C2  (wf_ng,3)
 complex(SP) :: K1_v(wf_ng,3), K2_v(wf_ng,3)
 !
 complex(SP) :: drho(3)
 !
 K_c  = (0.,0.)
 K1_v = (0.,0.)
 K2_v = (0.,0.)
 !
 ivfft_up=wf_state(iv,ik,1)
 ivfft_dw=wf_state(iv,ik,2)
 icfft_up=wf_state(ic,ik,1)
 icfft_dw=wf_state(ic,ik,2)
 !
 if(i_spin==1) then
   ivfft1=ivfft_up
   !
   icfft1=icfft_up
   icfft2=icfft_dw
 else
   ivfft1=ivfft_dw
   !
   icfft2=icfft_up
   icfft1=icfft_dw
 endif
 !
 do i_pp=1,pp_kbv_dim
   !
   if(kbv_spin(i_pp)==1) cycle
   !
   ! [r,\Delta V^{SO}] L*S
   !
   A1(:)=kbv(:,i_pp,7)*(-1)**(i_spin+1)                     ! kbv_z*S_z
   A2(:)=kbv(:,i_pp,5)+(0.,1.)*kbv(:,i_pp,6)*(-1)**i_spin   ! kbv_x*S_x + kbv_y*Sy
   !
   A1_c =V_dot_V(wf_ng,wf(:,icfft1),A1(:))
   A2_c =V_dot_V(wf_ng,wf(:,icfft2),A2(:))
   !
   if (.not. (A1_c==(0.,0.) .and. A2_c==(0.,0.)) ) then
     do i_dir=1,3
       call V_by_V_plus_V(wf_ng,A1_c,conjg(kbv(:,i_pp,1+i_dir)),K_c(:,i_dir))
       call V_by_V_plus_V(wf_ng,A2_c,conjg(kbv(:,i_pp,1+i_dir)),K_c(:,i_dir))
     enddo
   endif
   !
   ! \Delta V^{SO} [r,L*S]=\Delta V^{SO} * r x S
   !
   B_v  =V_dot_V(wf_ng,wf(:,ivfft1),kbv(:,i_pp,1))
   ! 
   if(B_v==(0.,0.)) cycle
   !
   C1(:,1) = kbv(:,i_pp,3)        *(-1)**(i_spin+1) !  KB_y*S_z
   C2(:,1) =-kbv(:,i_pp,4)*(0.,1.)*(-1)**i_spin     ! -KB_z*S_y
   C1(:,2) =-kbv(:,i_pp,2)        *(-1)**(i_spin+1) ! -KB_x*S_z
   C2(:,2) = kbv(:,i_pp,4)                          !  KB_z*S_x
   C1(:,3) = 0                                      !
   C2(:,3) = kbv(:,i_pp,2)*(0.,1.)*(-1)**i_spin &   !  KB_x*S_y
&           -kbv(:,i_pp,3)                          ! -KB_y*S_x  
   !
   do i_dir=1,3
     call V_by_V_plus_V(wf_ng,conjg(B_v),C1(:,i_dir),K1_v(:,i_dir))
     call V_by_V_plus_V(wf_ng,conjg(B_v),C2(:,i_dir),K2_v(:,i_dir))
   enddo
   !
 enddo
 !
 do i_dir=1,3
   drho(i_dir)=Vstar_dot_V(wf_ng,wf(:,ivfft1),K_c(:,i_dir)) &
&                 -V_dot_V(wf_ng,K1_v(:,i_dir),wf(:,icfft1))-V_dot_V(wf_ng,K2_v(:,i_dir),wf(:,icfft2))
 enddo
 !
 ! DEBUG <
 !if( any(abs(drho(:)) >0.01) ) then
 !  write(*,*) '******warning SO******'
 !endif
 !write(*,*) 'SO:   ,iv,ic,drho/rho',ivfft1,icfft1,abs(drho(:))/abs(rho(:)),abs(drho(:))
 ! DEBUG >
 !
 rho=rho+drho
 !
end subroutine
