!
!        Copyright (C) 2000-2019 the YAMBO team
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
subroutine scatter_Bamp_using_the_gradient(isc,which)
 !
 ! It computs
 !
 ! rhotw(G)=<ib ik_is i_sp| e^[I(RsG-G0).r] (\grad | ob ok_os o_sp> ) ! which ="o"
 !
 ! or
 !
 ! rhotw(G)=(\grad <ib ik_is i_sp|) e^[I(RsG-G0).r] | ob ok_os o_sp>  ! which ="i"
 !
 !  is(3)=(ib,ik,is,i_sp) --<--:--<-- os(3)=(ob,ok,os,o_sp)
 !                             :
 !                            /:\ iqs=(ig0,iq,qs)
 !                             :
 !
 !  iq_is = ik_is-ok_os-Go
 !
 use pars,           ONLY:SP
 use electrons,      ONLY:n_spinor
 use collision_el,   ONLY:elemental_collision
 use D_lattice,      ONLY:idt_index
 use R_lattice,      ONLY:g_rot
 use FFT_m,          ONLY:fft_size,fft_dim,fftw_plan,fft_g_table
 !
 implicit none
 !
 type(elemental_collision)::isc
 character(1)             ::which
 !
 ! Work Space
 !
 complex(SP) :: WF_der(fft_size,n_spinor,3)
 !
 integer     :: qs,iGo,ig, ir,ic,ig_
 !
 ! |ib ik_is i_sp>
 call WF_apply_symm(isc%is,isc%WF_symm_i)
 !
 ! | ob ok_os o_sp>   
 call WF_apply_symm(isc%os,isc%WF_symm_o)
 !
 ! \grad <ib ik_is i_sp|
 if (which=="i") call eval_Gradient(isc%WF_symm_i,WF_der,n_spinor,"wave")
 !
 ! \grad | ob ok_os o_sp>   
 if (which=="o") call eval_Gradient(isc%WF_symm_o,WF_der,n_spinor,"wave")
 !
 ! Chartesian components of the gradient loop
 !
 do ic=1,3
   !
   ! \tilde{\rho} in Real Space
   !
#if defined _OPENMP
   !$omp parallel default(shared), private(ir)
   !$omp do
#endif
   do ir = 1, fft_size
     if (which=="i")  isc%rho_tw_rs(ir) = conjg(WF_der(ir,1,ic))*isc%WF_symm_o(ir,1)
     if (which=="o")  isc%rho_tw_rs(ir) = conjg(isc%WF_symm_i(ir,1))*WF_der(ir,1,ic)
   enddo
#if defined _OPENMP
   !$omp end do
#endif
   !
   if (n_spinor==2) then
   !$omp do
     do ir = 1, fft_size
       if (which=="i") isc%rho_tw_rs(ir) = isc%rho_tw_rs(ir) + conjg(WF_der(ir,2,ic))*isc%WF_symm_o(ir,2)
       if (which=="o") isc%rho_tw_rs(ir) = isc%rho_tw_rs(ir) + conjg(isc%WF_symm_i(ir,2))*WF_der(ir,2,ic)
     enddo
   !$omp end do
   endif
   !$omp end parallel
   !
   ! perform the actual FFT
   !
#if defined _FFTW
   call fft_3d(isc%rho_tw_rs,fft_dim,+1,fftw_plan)
#else
   call fft_3d(isc%rho_tw_rs,fft_dim,+1)
#endif
   !
   ! e^[I(Rs(G-G0)).r]
   !
   iGo=isc%qs(1)
   qs =isc%qs(3)
   !
#if defined _OPENMP
   if (qs==idt_index) then 
     !$omp parallel do default(shared), private(ig,ig_)
     do ig = 1, isc%ngrho
       ig_ =isc%ngrho*(ic-1)+ig
       isc%rhotw(ig_)= isc%rho_tw_rs(fft_g_table(ig,iGo))
     enddo
     !$omp end parallel do
   else
     !$omp parallel do default(shared), private(ig,ig_)
     do ig = 1, isc%ngrho
       ig_ =isc%ngrho*(ic-1)+ig
       isc%rhotw(ig_)= isc%rho_tw_rs(fft_g_table(g_rot(qs,ig),iGo))
     enddo
     !$omp end parallel do
   endif
#else
   do ig = 1,isc%ngrho
     ig_ =isc%ngrho*(ic-1)+ig
     if (qs==idt_index) isc%rhotw(ig_)= isc%rho_tw_rs(fft_g_table(ig,iGo))
     if (qs/=idt_index) isc%rhotw(ig_)= isc%rho_tw_rs(fft_g_table(g_rot(qs,ig),iGo))
   enddo
#endif
   !
 enddo
 !
end subroutine
