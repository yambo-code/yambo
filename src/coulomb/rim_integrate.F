!
!        Copyright (C) 2000-2020 the YAMBO team
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
 use pars,          ONLY:SP,DP,pi
 use vec_operate,   ONLY:iku_v_norm
 use R_lattice,     ONLY:g_vec,RIM_n_rand_pts,k_grid_uc_vol,RIM_qpg,q0_def_norm,&
&                        RIM_is_diagonal,RIM_ng,q_pt,RIM_anisotropy,nqibz,b,cut_is_slab,idir
 use D_lattice,     ONLY:alat
 !
 implicit none
 integer  :: iq,N_out,N_out_G
 real(SP) :: qr(3,RIM_n_rand_pts),em1_anis(3),G_radii
 ! 
 ! Work Space
 !
 integer  :: i1,i1min,i2,i2max,i3,n_idx,n_max
 real(SP) :: v1(3),v2(3),factorial,G_circ,vslab,func
 real(DP) :: r1,rfac,RIM_acc(RIM_ng),RIM_acc_anis
 real(SP) :: slab_vz,slab_vplane,lcut,pre_factor
 !
 rfac=8.*k_grid_uc_vol/real(N_out)/(2.*pi)**3. 
 !
 if (cut_is_slab) lcut=alat(idir(1))/2.
 !
 !    ----------------
 !    MonteCarlo volume
 !
 RIM_qpg(iq,:,:)=0.
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
         slab_vz=(q_pt(iq,idir(1))+qr(idir(1),i3)+g_vec(i1,idir(1)) )*2.*pi/alat(idir(1))
         !kxy
         slab_vplane=sqrt(((q_pt(iq,idir(2))+qr(idir(2),i3)+g_vec(i1,idir(2)))*2.*pi/alat(idir(2)))**2+&
&                         ((q_pt(iq,idir(3))+qr(idir(3),i3)+g_vec(i1,idir(3)))*2.*pi/alat(idir(3)))**2)
         !    
         r1 = (slab_vplane**2+slab_vz**2) 
         pre_factor=1.-exp(-slab_vplane*lcut)*cos(slab_vz*lcut)       
         RIM_acc(1)=RIM_acc(1)+2._DP*pre_factor*rfac/r1
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
     pre_factor = pre_factor+(-1.0_SP)**(n_idx+1)/n_idx/factorial*(G_radii*lcut)**n_idx
   enddo
   RIM_acc(1) = pre_factor/(2.0_SP*pi*lcut)
   !
 else
   !
   G_radii=(3.0_DP*0.2_DP/(4.0_DP*pi)*k_grid_uc_vol)**(1.0_DP/3.0_DP)
   RIM_acc(1)  =2.0_DP*4.0_DP*pi*G_radii/(2.0_DP*pi)**3.0_DP
   RIM_acc_anis=2.0_DP*4.0_DP*pi/3.0_DP*G_radii/(2.0_DP*pi)**3*sum(em1_anis)
   !
 endif
 !
 N_out_G=0
 i2max=RIM_ng 
 if (RIM_is_diagonal) i2max=1
 RIM_acc_anis=0._DP
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
       slab_vplane=sqrt((qr(idir(2),i1)*2.*pi/alat(idir(2)))**2+&
&                       (qr(idir(3),i1)*2.*pi/alat(idir(3)))**2)
       if (slab_vplane < q0_def_norm) slab_vplane = q0_def_norm  
       !     
       pre_factor=1.0_SP-exp(-slab_vplane*lcut)*cos(slab_vz*lcut)       
       RIM_acc(1)=RIM_acc(1)+2._DP*pre_factor*rfac/(r1**2)
       !
     else
       !
       RIM_acc(1)=RIM_acc(1)+2._DP*rfac/(r1**2)
       RIM_acc_anis=RIM_acc_anis+2._DP*rfac*dot_product(em1_anis,v1)
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
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
subroutine rim_integrate_w(iq,qr,N_out)
 !
 use pars,          ONLY:SP,DP,pi
 use vec_operate,   ONLY:iku_v_norm
 use R_lattice,     ONLY:g_vec,RIM_n_rand_pts,k_grid_uc_vol,q0_def_norm,&
&                        RIM_ng,q_pt,nqibz,b,RIM_W_is_diagonal,&
&                        RIM_W,f_coeff,cut_is_slab,idir
 use D_lattice,     ONLY:alat
 !
 implicit none
 integer  :: iq,N_out
 real(SP) :: qr(3,RIM_n_rand_pts)
 ! 
 ! Work Space
 !
 integer  :: i1,i1min,i2,i2max,i3
 real(DP) :: func,vslab(RIM_n_rand_pts,RIM_ng)
 real(DP) :: r1,rfac,RIM_acc
 real(SP) :: slab_vz1,slab_vplane1,slab_vz2,slab_vplane2,lcut,pre_factor
 !
 rfac=8._SP*k_grid_uc_vol/real(N_out)/(2._SP*pi)**4
 !
 if (cut_is_slab) then
  lcut=alat(idir(1))/2._SP
 else
  call error('RIM-W without cutoff slab has not been implemented')
 end if
 !
 RIM_W(iq,:,:)=0.
 !
 !Evaluate vslab
 !
 i1min=1
 if (iq==1) i1min=2
 do i1=i1min,RIM_ng
   !
   do i2=1,RIM_n_rand_pts
     !
     slab_vz1 = (qr(idir(1),i2)+q_pt(iq,idir(1))+g_vec(i1,idir(1)))*2.*pi/alat(idir(1))
     slab_vplane1 = 2*pi*sqrt(((qr(idir(2),i2)+q_pt(iq,idir(2))+g_vec(i1,idir(2)))/alat(idir(2)))**2+&
&                             ((qr(idir(3),i2)+q_pt(iq,idir(3))+g_vec(i1,idir(3)))/alat(idir(3)))**2)
     pre_factor=(1.-exp(-slab_vplane1*lcut)*cos(slab_vz1*lcut))
     !
     vslab(i2,i1) = sqrt(4._SP*pi*pre_factor/(slab_vplane1**2+slab_vz1**2))
     !
   enddo
   !
 enddo
 !
 ! q /= 0 all terms
 !
 !$omp parallel do default(shared), private(i1,i2,i3,func,RIM_acc)
 do i1=i1min,RIM_ng 
   i2max=RIM_ng
   if (RIM_W_is_diagonal) i2max=i1
   do i2=i1,i2max
     !
     RIM_acc=0._DP
     !
     do i3=1,RIM_n_rand_pts
       !
       func = f_coeff(1,i1,i2,iq,1)+qr(1,i3)*(f_coeff(2,i1,i2,iq,1)+qr(1,i3)*f_coeff(4,i1,i2,iq,1))&
&                                    +qr(2,i3)*(f_coeff(3,i1,i2,iq,1)+qr(2,i3)*f_coeff(6,i1,i2,iq,1)&
&                                    +2_SP*qr(1,i3)*f_coeff(5,i1,i2,iq,1))
       ! Accumulate W
       RIM_acc = RIM_acc + rfac*vslab(i3,i1)**2*func*vslab(i3,i2)**2/(1-vslab(i3,i1)*func*vslab(i3,i2))
     enddo
     !
     RIM_W(iq,i1,i2)=RIM_acc
     RIM_W(iq,i2,i1)=RIM_W(iq,i1,i2)
     !
   enddo
 enddo
 !$omp end parallel do
 if (iq>1) return
 !
 ! head q == 0
 !
 RIM_acc=0._DP
 !
 do i1=1,RIM_n_rand_pts
   !
   slab_vplane1=sqrt((qr(idir(2),i1)*2.*pi/alat(idir(2)))**2+&
&                    (qr(idir(3),i1)*2.*pi/alat(idir(3)))**2)
   !Regularization
   if (slab_vplane1 < 1.e-5) then
     vslab(i1,1) = sqrt(4._DP*pi*(1.-exp(-q0_def_norm*lcut))/q0_def_norm**2)
     RIM_acc = RIM_acc + rfac*f_coeff(1,1,1,1,1)*(4*pi*lcut)**2
     cycle
   end if
   !
   !Evaluate v_slab
   !kxy
   r1=iku_v_norm(qr(:,i1))
   !     
   vslab(i1,1)=4._DP*pi*(1.-exp(-slab_vplane1*lcut))
   !
   !Evaluate interpolation function
   func = f_coeff(1,1,1,1,1)*exp(-sqrt(f_coeff(2,1,1,1,1)**2*qr(1,i1)**2+f_coeff(3,1,1,1,1)**2*qr(2,i1)**2))
   !Evaluate W
   RIM_acc = RIM_acc + rfac*vslab(i1,1)*func*vslab(i1,1)/(r1**2*(1-vslab(i1,1)*func))
   !
   !Store the square root of vslab
   vslab(i1,1) = sqrt(vslab(i1,1))/r1
 enddo
 !
 RIM_W(1,1,1)=RIM_acc
 !
 ! wings q == 0
 !
 if (RIM_W_is_diagonal) return
 do i2=2,RIM_ng
   RIM_acc=0._DP
   !
   do i3=1,RIM_n_rand_pts
     !
     !Evaluate func
     !
     func = f_coeff(1,1,i2,iq,1)+qr(1,i3)*(f_coeff(2,1,i2,iq,1)+qr(1,i3)*f_coeff(4,1,i2,iq,1))&
&                             +qr(2,i3)*(f_coeff(3,1,i2,iq,1)+qr(2,i3)*f_coeff(6,1,i2,iq,1)&
&                        +2_SP*qr(1,i3)*f_coeff(5,1,i2,iq,1))
     !
     RIM_acc = RIM_acc + rfac*vslab(i3,1)**2*func*vslab(i3,i2)**2/(1-vslab(i3,1)*func*vslab(i3,i2))
   enddo
   RIM_W(iq,1,i2)=RIM_acc
   RIM_W(iq,i2,1)=RIM_W(iq,1,i2)
 enddo
 !
end subroutine
