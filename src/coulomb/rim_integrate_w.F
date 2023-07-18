!
!        Copyright (C) 2000-2022 the YAMBO team
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
subroutine rim_integrate_w(iq,qr,N_out,em1_anis)
 !
 use pars,          ONLY:SP,DP,pi
 use vec_operate,   ONLY:iku_v_norm
 use R_lattice,     ONLY:g_vec,RIM_n_rand_pts,k_grid_uc_vol,q0_def_norm,&
&                        RIM_W_ng,q_pt,b,RIM_W_is_diagonal,RIM_W_for_graph,&
&                        RIM_W,f_coeff,cut_is_slab,idir,RIM_id_epsm1_reference
 use D_lattice,     ONLY:alat,a
 !
 implicit none
 integer  :: iq,N_out
 real(SP) :: qr(3,RIM_n_rand_pts),em1_anis(3)
 !
 ! Work Space
 !
 integer  :: i1,i1min,i2,i2max,i3
 real(DP) :: func,vslab(RIM_n_rand_pts,RIM_W_ng)
 real(DP) :: r1,rfac,RIM_acc,RIM_acc_anis
 real(SP) :: slab_vz1,slab_vplane1,lcut,pre_factor,anis_fact,vslab2
 !
 rfac=8._SP*k_grid_uc_vol/real(N_out)/(2._SP*pi)**4
 !
 if (cut_is_slab) then
  lcut=alat(idir(1))/2._SP
 else
  call error('RIM-W without cutoff slab has not been implemented')
 end if
 !
 !Evaluate vslab
 !
 i1min=1
 if (iq==1) i1min=2
 do i1=i1min,RIM_W_ng
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
 do i1=i1min,RIM_W_ng
   i2max=RIM_W_ng
   if (RIM_W_is_diagonal) i2max=i1
   do i2=i1,i2max
     !
     RIM_acc=0._DP
     !
     do i3=1,RIM_n_rand_pts
       !
       func = f_coeff(1,i1,i2,iq,1)+qr(1,i3)*(f_coeff(2,i1,i2,iq,1)+qr(1,i3)*f_coeff(4,i1,i2,iq,1))&
&                                  +qr(2,i3)*(f_coeff(3,i1,i2,iq,1)+qr(2,i3)*f_coeff(6,i1,i2,iq,1)&
&                                  +2_SP*qr(1,i3)*f_coeff(5,i1,i2,iq,1))
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
 RIM_acc_anis=0._DP
 !
 !If the system is a metal, the head is obtained
 !with the q\=0 formula
 if (RIM_W_for_graph) then
   !
   do i1=1,RIM_n_rand_pts
     slab_vplane1=sqrt((qr(idir(2),i1)*2.*pi/alat(idir(2)))**2+&
&                      (qr(idir(3),i1)*2.*pi/alat(idir(3)))**2)
     !kxy
     r1=iku_v_norm(qr(:,i1))
     if (slab_vplane1 < 1.e-5) then
       vslab(i1,1) = sqrt(4._DP*pi*(1.-exp(-q0_def_norm*lcut))/q0_def_norm**2)
       RIM_acc = RIM_acc + rfac*f_coeff(1,1,1,1,1)
       cycle
     end if
     !
     !Evaluate v_slab
     vslab2 = 4._DP*pi*(1.-exp(-slab_vplane1*lcut))/r1**2
     !
     !Evaluate func
     !
     func = f_coeff(1,1,1,1,1)+qr(1,i1)*(f_coeff(2,1,1,1,1)+qr(1,i1)*f_coeff(4,1,1,1,1))&
&                             +qr(2,i1)*(f_coeff(3,1,1,1,1)+qr(2,i1)*f_coeff(6,1,1,1,1)&
&                             +2_SP*qr(1,i1)*f_coeff(5,1,1,1,1))
     !
     RIM_acc = RIM_acc + rfac*vslab2*func*vslab2/(1._DP-vslab2*func)
     !
     !Store the square root of vslab
     vslab(i1,1) = sqrt(vslab2)
     !
   end do
 else
   !
   do i1=1,RIM_n_rand_pts
     !
     slab_vplane1=sqrt((qr(idir(2),i1)*2.*pi/alat(idir(2)))**2+&
&                      (qr(idir(3),i1)*2.*pi/alat(idir(3)))**2)
     !kxy
     r1=iku_v_norm(qr(:,i1))
     !Regularization
     if (slab_vplane1 < 1.e-5) then
       vslab(i1,1) = sqrt(4._DP*pi*(1.-exp(-q0_def_norm*lcut))/q0_def_norm**2)
       RIM_acc = RIM_acc + rfac*f_coeff(1,1,1,1,1)*(4*pi*lcut)**2
       RIM_acc_anis = RIM_acc_anis + rfac*f_coeff(1,1,1,1,1)*(4*pi*lcut)**2 &
&                     *0.5_SP*(em1_anis(idir(2))+em1_anis(idir(3)))
       cycle
     end if
     !
     !Evaluate v_slab
     vslab2=4._DP*pi*(1.-exp(-slab_vplane1*lcut))
     !
     !Evaluate interpolation function
     func = f_coeff(1,1,1,1,1)*exp(-sqrt((f_coeff(2,1,1,1,1)*(qr(2,i1)+a(2,1)/a(1,1)*qr(1,i1)))**2+&
&                                        (f_coeff(3,1,1,1,1)*(qr(1,i1)+a(2,1)/a(1,1)*qr(2,i1)))**2))
     !
     !Evaluate W
     RIM_acc = RIM_acc + rfac*vslab2*func*vslab2/(r1**2*(1-vslab2*func))
     !Anisotropy contribution
     anis_fact=dot_product(em1_anis,(2._SP*pi*qr(:,i1)/(alat(:)*r1))**2)
     func = func*anis_fact
     RIM_acc_anis = RIM_acc_anis + rfac*vslab2*func*vslab2/(r1**2*(1-vslab2*func))
     !
     !Store the square root of vslab
     vslab(i1,1) = sqrt(vslab2)/r1
     !
   enddo
 end if
 !
 if (RIM_id_epsm1_reference == 0) then
   RIM_W(1,1,1)=RIM_acc
 else
   RIM_W(1,1,1)=RIM_acc_anis
 end if
 !
 ! wings q == 0
 !
 if (RIM_W_is_diagonal) return
 do i2=2,RIM_W_ng
   RIM_acc=0._DP
   !
   do i3=1,RIM_n_rand_pts
     !
     !Evaluate func
     !
     func = f_coeff(1,1,i2,iq,1)+qr(1,i3)*(f_coeff(2,1,i2,iq,1)+qr(1,i3)*f_coeff(4,1,i2,iq,1))&
&                               +qr(2,i3)*(f_coeff(3,1,i2,iq,1)+qr(2,i3)*f_coeff(6,1,i2,iq,1)&
&                               +2_SP*qr(1,i3)*f_coeff(5,1,i2,iq,1))
     !
     RIM_acc = RIM_acc + rfac*vslab(i3,1)**2*func*vslab(i3,i2)**2/(1._DP-vslab(i3,1)*func*vslab(i3,i2))
   enddo
   RIM_W(iq,1,i2)=RIM_acc
   RIM_W(iq,i2,1)=RIM_W(iq,1,i2)
 enddo
 !
end subroutine
