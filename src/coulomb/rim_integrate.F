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
subroutine rim_integrate(iq,qr,em1_anis,N_out,N_out_G,G_radii,G_circ,cut_is_slab,iort,ipar1,ipar2,mode,f_coeff)
 !
 use pars,          ONLY:SP,DP,pi
 use vec_operate,   ONLY:iku_v_norm,c2a
 use R_lattice,     ONLY:g_vec,RIM_n_rand_pts,k_grid_uc_vol,RIM_qpg,q0_def_norm,&
&                        RIM_is_diagonal,RIM_ng,q_pt,RIM_anisotropy,nqibz,RIM_W,b
 use D_lattice,     ONLY:alat
 !
 implicit none
 integer  :: iq,N_out,N_out_G
 real(SP) :: qr(RIM_n_rand_pts,3),em1_anis(3),G_radii
 character(1)                :: mode
 real, optional :: f_coeff(5,nqibz,RIM_ng,RIM_ng)
 ! 
 ! Work Space
 !
 integer  :: i1,i1min,i2,i2max,i3,iort,ipar1,ipar2,n_idx,n_max
 real(SP) :: v1(3),v2(3),factorial,G_circ,vslab,func, g1_2D_mod, g2_2D_mod
 real(DP) :: r1,rfac,RIM_acc(RIM_ng),RIM_acc_anis
 logical  :: cut_is_slab
 real(SP) :: slab_vz,slab_vplane,slab_vz1,slab_vplane1,slab_vz2,slab_vplane2,lcut,pre_factor,v_2D_mod
 !
 if (cut_is_slab) then
   lcut=alat(iort)/2.
   rfac=4.*k_grid_uc_vol/real(N_out)/(2.*pi)**3.
 else
   rfac=8.*k_grid_uc_vol/real(N_out)/(2.*pi)**3.
 endif
 if (mode == "c") rfac = rfac/(2.*pi)
 !
 !    ----------------
 !    MonteCarlo volume
 !
 if (mode == 'x') then
   RIM_qpg(iq,:,:)=0.
 else if (mode == 'c') then
   RIM_W(iq,:,:)=0.
 end if
 !
 ! All points
 !
 i1min=1
 if (iq==1 .and. mode == "x") i1min=2
 do i1=i1min,RIM_ng 
   i2max=RIM_ng 
   if (RIM_is_diagonal .and. mode == "x") i2max=i1
   do i2=i1,i2max
     RIM_acc(1)=0._DP
     if (cut_is_slab .and. mode == 'x') then
       do i3=1,RIM_n_rand_pts
         ! 
         v1(:)=g_vec(i1,:)+q_pt(iq,:)+qr(i3,:)
         v2(:)=g_vec(i2,:)+q_pt(iq,:)+qr(i3,:)
         r1=iku_v_norm(v1)*iku_v_norm(v2)
         !
         !Evaluate v_slab
         !kz
         slab_vz=(q_pt(iq,iort)+qr(i3,iort)+g_vec(i1,iort) )*2.*pi/alat(iort)
         !kxy
         slab_vplane=sqrt(((q_pt(iq,ipar1)+qr(i3,ipar1)+g_vec(i1,ipar1))*2.*pi/alat(ipar1))**2+&
&                 ((q_pt(iq,ipar2)+qr(i3,ipar2)+g_vec(i1,ipar2))*2.*pi/alat(ipar2))**2)
         !     
         pre_factor=1.-exp(-slab_vplane*lcut)*cos(slab_vz*lcut)
         if (abs(pre_factor)<1.E-5_SP) pre_factor=1.E-5_SP        
         RIM_acc(1)=RIM_acc(1)+2._DP*pre_factor*rfac/r1
         !
       end do
       RIM_qpg(iq,i1,i2)=RIM_acc(1)
       RIM_qpg(iq,i2,i1)=RIM_qpg(iq,i1,i2)
     else if (cut_is_slab .and. mode == 'c') then
       !
       do i3=1,RIM_n_rand_pts
         ! 
         v1(:)=g_vec(i1,:)+q_pt(iq,:)+qr(i3,:)
         v2(:)=g_vec(i2,:)+q_pt(iq,:)+qr(i3,:)
         r1=iku_v_norm(v1)*iku_v_norm(v2)
         !
         !Evaluate v_slab
         !kz
         slab_vz1=(q_pt(iq,iort)+qr(i3,iort)+g_vec(i1,iort) )*2.*pi/alat(iort)
         slab_vz2=(q_pt(iq,iort)+qr(i3,iort)+g_vec(i2,iort) )*2.*pi/alat(iort)
         !kxy
         slab_vplane1=sqrt(((q_pt(iq,ipar1)+qr(i3,ipar1)+g_vec(i1,ipar1))*2.*pi/alat(ipar1))**2+&
&                 ((q_pt(iq,ipar2)+qr(i3,ipar2)+g_vec(i1,ipar2))*2.*pi/alat(ipar2))**2)
         slab_vplane2=sqrt(((q_pt(iq,ipar1)+qr(i3,ipar1)+g_vec(i2,ipar1))*2.*pi/alat(ipar1))**2+&
&                 ((q_pt(iq,ipar2)+qr(i3,ipar2)+g_vec(i2,ipar2))*2.*pi/alat(ipar2))**2)
         !
         if (slab_vplane1 < q0_def_norm) slab_vplane1 = q0_def_norm
         if (slab_vplane2 < q0_def_norm) slab_vplane2 = q0_def_norm
         !     
         pre_factor=sqrt(1.-exp(-slab_vplane1*lcut)*cos(slab_vz1*lcut))*sqrt(1.-exp(-slab_vplane2*lcut)*cos(slab_vz2*lcut))
         !
         vslab=4._DP*pi*pre_factor/r1
         !
         !Evaluate func
         !
         if (iq == 1) then
           g1_2D_mod = sqrt(g_vec(i1,1)**2+g_vec(i1,2)**2)
           g2_2D_mod = sqrt(g_vec(i2,1)**2+g_vec(i2,2)**2)
           call c2a(b,qr(i3,:),v1,'ki2c')
           v_2D_mod = sqrt(v1(1)**2+v1(2)**2)
           !
           if ((i1 == 1 .and. g2_2D_mod < 1.e-5) .or. (g1_2D_mod < 1.e-5 .and. i2 == 1)) then
             !
             func = f_coeff(2,iq,i1,i2)*v_2D_mod**2
             !
           else
             !
             !func = f_coeff(1,iq,i1,i2)+f_coeff(2,iq,i1,i2)*v_2D_mod
             !
             call c2a(b,qr(i3,:),v1,'ki2a')
             func = f_coeff(1,iq,i1,i2)+v1(1)*f_coeff(2,iq,i1,i2)+v1(1)*f_coeff(4,iq,i1,i2)*v1(1)+&
&                                    +v1(2)*f_coeff(3,iq,i1,i2)+v1(2)*f_coeff(5,iq,i1,i2)*v1(2)
           end if
           !
         else
           !
           call c2a(b,qr(i3,:),v1,'ki2a')
           func = f_coeff(1,iq,i1,i2)+v1(1)*f_coeff(2,iq,i1,i2)+v1(1)*f_coeff(4,iq,i1,i2)*v1(1)+&
&                                    +v1(2)*f_coeff(3,iq,i1,i2)+v1(2)*f_coeff(5,iq,i1,i2)*v1(2)
           !
         end if
         !
         !Evaluate W
         !
         RIM_acc(1) = RIM_acc(1) + rfac*vslab*func*vslab/(1-vslab*func)
       end do
       RIM_W(iq,i1,i2)=RIM_acc(1)
       RIM_W(iq,i2,i1)=RIM_W(iq,i1,i2)
     else
       do i3=1,RIM_n_rand_pts
         ! 
         v1(:)=g_vec(i1,:)+q_pt(iq,:)+qr(i3,:)
         v2(:)=g_vec(i2,:)+q_pt(iq,:)+qr(i3,:)
         r1=iku_v_norm(v1)*iku_v_norm(v2)
         RIM_acc(1)=RIM_acc(1)+2._DP*rfac/r1
       enddo
       RIM_qpg(iq,i1,i2)=RIM_acc(1)
       RIM_qpg(iq,i2,i1)=RIM_qpg(iq,i1,i2)
     endif


   enddo
 enddo
 if (iq>1) return
 !
 ! Gamma point (1,I) elements
 !
 RIM_acc=0._DP
 !
 if (cut_is_slab .and. mode == "x") then
   ! Here put the integral over the circle
   G_circ = .2_SP*k_grid_uc_vol/((2._SP*pi)/alat(iort))
   G_radii = sqrt(G_circ/pi)
   n_max = 15
   factorial = 1._SP
   pre_factor = 0._SP
   do n_idx=1,n_max
     factorial = factorial*n_idx
     pre_factor = pre_factor+(-1._SP)**(n_idx+1)/n_idx/factorial*(G_radii*lcut)**n_idx
   enddo
   RIM_acc(1) = pre_factor/(2*pi*lcut)
 else if (.not. cut_is_slab) then
   G_radii=(3.*.2/(4.*pi)*k_grid_uc_vol)**(1./3.)
   RIM_acc(1)  =2._DP*4._DP*pi*G_radii/(2._DP*pi)**3._DP
   RIM_acc_anis=2._DP*4._DP*pi/3._DP*G_radii/(2._DP*pi)**3._DP*sum(em1_anis)
 endif
 !
 N_out_G=0
 i2max=RIM_ng 
 if (RIM_is_diagonal .and. mode == "x") i2max=1
 RIM_acc_anis=0._DP
 !
 if (mode == "x") then
   do i1=1,RIM_n_rand_pts
     r1=iku_v_norm(qr(i1,:))
     v1=4.*pi**2.*(/qr(i1,1)**2.,qr(i1,2)**2,qr(i1,3)**2/)
     v1(:)=v1(:)/alat(:)**2./r1**4.
     !
     if(r1>=G_radii) then
       !
       ! Integrate the Coulomb term outside the sphere of radius G_radii
       !
       N_out_G=N_out_G+1
       if (cut_is_slab) then
         !Evaluate v_slab
         !kz
         slab_vz=qr(i1,iort)*2.*pi/alat(iort)
         !kxy
         slab_vplane=sqrt((qr(i1,ipar1)*2.*pi/alat(ipar1))**2+&
&                 (qr(i1,ipar2)*2.*pi/alat(ipar2))**2)
         !     
         pre_factor=1.-exp(-slab_vplane*lcut)*cos(slab_vz*lcut)
         if (abs(pre_factor)<1.E-5_SP) pre_factor=1.E-5_SP        
         RIM_acc(1)=RIM_acc(1)+2._DP*pre_factor*rfac/(r1**2._DP)
       else
         RIM_acc(1)=RIM_acc(1)+2._DP*rfac/(r1**2._DP)
         RIM_acc_anis=RIM_acc_anis+2._DP*rfac*dot_product(em1_anis,v1)
       endif
       !
       ! Integrate the 1/|q||q+Go| term outside the sphere of radius G_radii as well
       ! because the intergral in the sphere gose to zero as NQ->\infty
       !
       do i2=2,i2max,1
         r1=r1*iku_v_norm(g_vec(i2,:)+qr(i1,:))
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
 else if (mode == "c") then
   !
   do i1=1,RIM_n_rand_pts
     !
     r1=iku_v_norm(qr(i1,:))
     if (r1 < 1.e-5) then
       RIM_acc(1) = RIM_acc(1) + rfac*f_coeff(1,1,1,1)*(4*pi*lcut)**2
       cycle
     end if
     !
     !Evaluate v_slab
     !kz
     slab_vz=qr(i1,iort)*2.*pi/alat(iort)
     !kxy
     slab_vplane=sqrt((qr(i1,ipar1)*2.*pi/alat(ipar1))**2+&
&                 (qr(i1,ipar2)*2.*pi/alat(ipar2))**2)
     !     
     pre_factor=1.-exp(-slab_vplane*lcut)      
     vslab=4._DP*pi*pre_factor
     !
     !Evaluate interpolation function
     func = f_coeff(1,1,1,1)*exp(-f_coeff(2,1,1,1)*slab_vplane)
     !Evaluate W
     RIM_acc(1) = RIM_acc(1) + rfac*vslab*func*vslab/r1**2/(1-vslab*func)
     !
   enddo
   !
   RIM_W(1,1,1)=RIM_acc(1)
   !
 end if
 !
end subroutine
