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
 subroutine rim_integrate(iq,qr,em1_anis,N_out,N_out_G,G_sph_radii)
!==================================================================
 use pars,          ONLY:SP,pi
 use vec_operate,   ONLY:iku_v_norm,v_is_zero
 use R_lattice,     ONLY:g_vec,RIM_n_rand_pts,k_grid_uc_vol,RIM_qpg,&
&                        RIM_is_diagonal,RIM_ng,q_pt,RIM_anisotropy
 use D_lattice,     ONLY:alat
 implicit none
 integer  :: iq,N_out,N_out_G
 real(SP) :: qr(RIM_n_rand_pts,3),em1_anis(3),G_sph_radii
!ws
 integer  :: i1,i1min,i2,i2max,i3
 real(SP) :: r1,rfac,v1(3),v2(3)
!------------------------------------------------------------------------
 rfac=8.*k_grid_uc_vol/real(N_out)/(2.*pi)**3.
!     ----------------
!     MonteCarlo volume

 RIM_qpg(iq,:,:)=0.

! All points
!======================
 i1min=1
 if (iq==1) i1min=2
 do i1=i1min,RIM_ng 
   i2max=RIM_ng 
   if (RIM_is_diagonal) i2max=i1
   do i2=i1,i2max
     do i3=1,RIM_n_rand_pts
       v1(:)=g_vec(i1,:)+q_pt(iq,:)+qr(i3,:)
       v2(:)=g_vec(i2,:)+q_pt(iq,:)+qr(i3,:)
       r1=iku_v_norm(v1)*iku_v_norm(v2)
       RIM_qpg(iq,i1,i2)=RIM_qpg(iq,i1,i2)+2.*rfac/r1
     enddo
     RIM_qpg(iq,i2,i1)=RIM_qpg(iq,i1,i2)
   enddo
 enddo
 if (iq>1) return

! Gamma point (1,I) elements
!===========================

 G_sph_radii=(3.*.2/(4.*pi)*k_grid_uc_vol)**(1./3.)
 RIM_qpg(iq,1,1)=2.*4.*pi*G_sph_radii/(2.*pi)**3.
 RIM_anisotropy=2.*4.*pi/3.*G_sph_radii/(2.*pi)**3.*sum(em1_anis)

 N_out_G=0
 i2max=RIM_ng 
 if (RIM_is_diagonal) i2max=1
 do i1=1,RIM_n_rand_pts
   r1=iku_v_norm(qr(i1,:))
   v1=4.*pi**2.*(/qr(i1,1)**2.,qr(i1,2)**2,qr(i1,3)**2/)
   v1(:)=v1(:)/alat(:)**2./r1**4.
   if(r1>=G_sph_radii) then
     N_out_G=N_out_G+1
     RIM_qpg(1,1,1)=RIM_qpg(1,1,1)+2.*rfac/(r1**2.)
     RIM_anisotropy=RIM_anisotropy+2.*rfac*dot_product(em1_anis,v1)
     do i2=2,i2max,1
       r1=r1*iku_v_norm(g_vec(i2,:)+qr(i1,:))
       RIM_qpg(1,1,i2)=RIM_qpg(1,1,i2)+rfac/r1
     enddo
   endif
 enddo
!
 do i1=2,i2max
   RIM_qpg(1,i1,1)=RIM_qpg(1,1,i1)
 enddo

 end subroutine
