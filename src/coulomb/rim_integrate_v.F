!
!        Copyright (C) 2000-2023 the YAMBO team
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
subroutine rim_integrate_v(iq,qr,em1_anis,N_out,N_out_G,G_radii,G_circ)
 !
 use pars,          ONLY:SP,DP,pi,pi_DP
 use vec_operate,   ONLY:iku_v_norm
 use R_lattice,     ONLY:g_vec,RIM_n_rand_pts,k_grid_uc_vol,RIM_qpg,q0_def_norm,&
&                        RIM_is_diagonal,RIM_ng,q_pt,RIM_anisotropy,b,cut_is_slab,idir
 use D_lattice,     ONLY:alat
 !
 implicit none
 integer  :: iq,N_out,N_out_G
 real(SP) :: qr(3,RIM_n_rand_pts),em1_anis(3),G_radii
 ! 
 ! Work Space
 !
 integer  :: i1,i1min,i2,i2max,i3,n_idx,n_max
 real(SP) :: v1(3),v2(3),factorial,G_circ
 real(DP) :: r1,rfac,RIM_acc(RIM_ng),RIM_acc_anis,pre_factor
 real(SP) :: slab_vz,slab_vplane,lcut
 !
 rfac=8._DP*real(k_grid_uc_vol,DP)/real(N_out,DP)/(2._DP*pi_DP)**3
 !
 if (cut_is_slab) lcut=alat(idir(1))/2._SP
 !
 !    ----------------
 !    MonteCarlo volume
 !
 RIM_qpg(iq,:,:)=0._SP
 !
 ! All points
 !
 i1min=1
 if (iq==1) i1min=2
 !
 do i1=i1min,RIM_ng
   i2max=RIM_ng
   if (RIM_is_diagonal) i2max=i1
   !
   do i2=i1,i2max
     RIM_acc(1)=0._DP
     if (cut_is_slab) then
       do i3=1,RIM_n_rand_pts
         !
         !Evaluate v_slab
         !kz
         slab_vz=(q_pt(iq,idir(1))+qr(idir(1),i3)+g_vec(i1,idir(1)))*2._SP*pi/alat(idir(1))
         !kxy
         slab_vplane=sqrt(((q_pt(iq,idir(2))+qr(idir(2),i3)+g_vec(i1,idir(2)))*2._SP*pi/alat(idir(2)))**2 &
&                     +((q_pt(iq,idir(3))+qr(idir(3),i3)+g_vec(i1,idir(3)))*2._SP*pi/alat(idir(3)))**2)
         !
         r1 = (slab_vplane**2+slab_vz**2)
         pre_factor=1._DP-exp(-real(slab_vplane*lcut,DP))*cos(real(slab_vz*lcut,DP))
         RIM_acc(1)=RIM_acc(1)+2._DP*real(pre_factor,DP)*rfac/r1
         !
       end do
       RIM_qpg(iq,i1,i2)=RIM_acc(1)
       RIM_qpg(iq,i2,i1)=RIM_qpg(iq,i1,i2)
     else
       do i3=1,RIM_n_rand_pts
         !
         v1(:)=g_vec(i1,:)+q_pt(iq,:)+qr(:,i3)
         v2(:)=g_vec(i2,:)+q_pt(iq,:)+qr(:,i3)
         r1=iku_v_norm(v1)*iku_v_norm(v2)
         RIM_acc(1)=RIM_acc(1)+2._DP*rfac/r1
       enddo
       RIM_qpg(iq,i1,i2)=RIM_acc(1)
       RIM_qpg(iq,i2,i1)=RIM_qpg(iq,i1,i2)
     endif
     !
   enddo
 enddo
 if (iq>1) return
 !
 ! Gamma point (1,I) elements
 !
 RIM_acc=0._DP
 !
 if (cut_is_slab) then
   !
   ! Here put the integral over the circle
   G_circ = 0.2_SP*k_grid_uc_vol/((2.0_SP*pi)/alat(idir(1)))
   G_radii = sqrt(G_circ/pi)
   n_max = 15
   factorial = 1.0_SP
   pre_factor = 0.0_SP
   do n_idx=1,n_max
     factorial = factorial*n_idx
     pre_factor = pre_factor+(-1.0_DP)**(n_idx+1)/real(n_idx,DP) &
     &            /real(factorial,DP)*real(G_radii*lcut,DP)**n_idx
   enddo
   RIM_acc(1)   = pre_factor/(2.0_DP*pi_DP*lcut)
   !
 else
   !
   G_radii=(3.0_DP*0.2_DP/(4.0_DP*pi)*k_grid_uc_vol)**(1.0_DP/3.0_DP)
   RIM_acc(1)  =2.0_DP*4.0_DP*pi_DP*real(G_radii,DP)/(2.0_DP*pi_DP)**3
   !
 endif
 !
 RIM_acc_anis = RIM_acc(1)/3.0_DP*sum(real(em1_anis,DP))
 !
 N_out_G=0
 i2max=RIM_ng 
 if (RIM_is_diagonal) i2max=1
 !
 do i1=1,RIM_n_rand_pts
   !
   r1=iku_v_norm(qr(:,i1))
   v1=4._DP*pi**2*qr(:,i1)**2
   v1(:)=v1(:)/alat(:)**2/r1**4
   !
   if(r1>=G_radii) then
     !
     ! Integrate the Coulomb term outside the sphere of radius G_radii
     !
     N_out_G=N_out_G+1
     if (cut_is_slab) then
       !Evaluate v_slab
       !kz
       slab_vz=qr(idir(1),i1)*2.*pi/alat(idir(1))
       !kxy
       slab_vplane=sqrt((qr(idir(2),i1)*2._SP*pi/alat(idir(2)))**2+&
&                       (qr(idir(3),i1)*2._SP*pi/alat(idir(3)))**2)
       if (slab_vplane < q0_def_norm) slab_vplane = q0_def_norm
       !
       pre_factor=1.0_SP-exp(-slab_vplane*lcut)*cos(slab_vz*lcut)
       RIM_acc(1)=RIM_acc(1)+2._DP*pre_factor*rfac/(r1**2)
       !
     else
       !
       RIM_acc(1)=RIM_acc(1)+2._DP*rfac/(r1**2)
       RIM_acc_anis=RIM_acc_anis+2._DP*rfac*real(dot_product(em1_anis,v1),DP)
       !
     endif
     !
     ! Integrate the 1/|q||q+Go| term outside the sphere of radius G_radii as well
     ! because the intergral in the sphere gose to zero as NQ->\infty
     !
     do i2=2,i2max,1
       r1=iku_v_norm(qr(:,i1))*iku_v_norm(g_vec(i2,:)+qr(:,i1))
       RIM_acc(i2)=RIM_acc(i2)+rfac/r1
     enddo
     !
   endif
   !
 enddo
 !
 RIM_qpg(1,1,:)=RIM_acc(:)
 RIM_anisotropy=RIM_acc_anis
 !
 do i1=2,i2max
   RIM_qpg(1,i1,1)=RIM_qpg(1,1,i1)
 enddo
 !
end subroutine
