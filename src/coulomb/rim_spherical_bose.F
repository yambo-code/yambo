!
! Copyright (C) 2000-2018 A. Marini and the YAMBO team 
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
subroutine rim_spherical_bose(Np,points,p_weight,radius,power,il,MULTIPLY_BY_Q)
 !
 ! Here I want to approx the integral
 !
 ! 1/vol \int_vol F(q) f_bose(q) d3q = 1/vol \int_vol d3q f_bose(q)/q^power [q^power F(q)] =
 !                                   = 1/vol \sum_{Qi} \int_Si d3q f_bose(q)/q^power [q^power F(q)] ~
 !                                   ~ 1/vol \sum_{Qi} [Qi^power f_bose(Qi) F(Qi)] \int_Si d3q/q^power*f_bose(q)/f_bose(Qi)
 !
 ! with Si a sphere of radius "radius" and volume vol/Np. F(q) is divergent at gamma
 ! and it goes like 1/q^2. Therefore [q^2 F(q)] is regular.
 !
 ! Finally I will rewrite 
 !
 ! 1/vol \int_vol F(q) d3q ~ \sum_{Qi} Wi F(Qi)
 !
 ! with
 !
 ! MULTIPLY_BY_Q=.TRUE. 
 !
 !   with Wi = Qi^power/vol*\int_Si d3q/q^power
 !
 ! MULTIPLY_BY_Q=.FALSE. 
 !
 !   with Wi = 1/vol*\int_Si d3q/q^power
 !
 use pars,          ONLY:SP,pi,DP
 use ELPH,          ONLY:elph_acustic_velocity,ph_freqs_sq
 use electrons,     ONLY:spin_occ
 use vec_operate,   ONLY:v_norm
 use D_lattice,     ONLY:alat
 use functions,     ONLY:bose_f
 use R_lattice,     ONLY:RIM_n_rand_pts
 !
 implicit none
 !
 integer,  intent(in)    :: Np,power,il
 real(SP), intent(in)    :: points(Np,3),radius
 real(SP), intent(inout) :: p_weight(Np)
 logical , intent(in)    :: MULTIPLY_BY_Q
 !
 ! Work Space
 !
 integer   :: ip,ir,N_out,N_in,ic
 real(SP)  :: v_rand(3),qr(RIM_n_rand_pts,3),sphere_vol,box_vol,pt_cc(3),f_bose,ph_freq
 character(12)      :: ch(3)
 integer            :: iseed(8)
 real(DP), external :: dlaran
 !
 if (RIM_n_rand_pts==0) then
   p_weight=1./float(Np)
   return
 endif
 !
 ! Filling a larger Sphere with a random grid
 !===========================================
 !
 ! Random generator seed
 !
 call date_and_time(ch(1),ch(2),ch(3),iseed)
 iseed=iabs(iseed)
 !
 ! iseed(4) must be odd
 !
 iseed(4)=2*(iseed(4)/2)+1
 !
 ! Loop setup
 !
 N_in=1
 N_out=0
 !
 loop: do while(.not.N_in==RIM_n_rand_pts+1)
   !
   do ic=1,3
     v_rand(ic)=( 2.*dlaran(iseed(4:))-1. )*radius*1.2
   enddo
   N_out=N_out+1
   !
   if (v_norm(v_rand)>radius) cycle loop
   qr(N_in,:)=v_rand
   N_in=N_in+1
   !
 enddo loop
 !  
 !Integrated Sphere VOLUME 
 !
 box_vol   =(2.*radius*1.2)**3.
 sphere_vol=4./3.*pi*radius**3.
 !
 do ip=1,Np
   p_weight(ip)=0.
   pt_cc=points(ip,:)*2.*pi/alat(:)
   do ir=1,RIM_n_rand_pts
     ph_freq=sqrt( dot_product( elph_acustic_velocity(il,:),qr(ir,:)+pt_cc(:) ) )
     f_bose=bose_f(ph_freq)/spin_occ
     p_weight(ip)=p_weight(ip)+f_bose/spin_occ/v_norm(qr(ir,:)+pt_cc(:))**power*box_vol/real(N_out)
   enddo
   p_weight(ip)=p_weight(ip)/sphere_vol/real(Np)
   if(ph_freqs_sq(ip,il)>0._SP) p_weight(ip)=p_weight(ip)/bose_f(sqrt(ph_freqs_sq(ip,il)))*spin_occ
   !
   if (MULTIPLY_BY_Q) p_weight(ip)=p_weight(ip)*v_norm(pt_cc)**power
   !
 enddo
 !
end subroutine rim_spherical_bose
