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
subroutine rim_spherical(Np,points,p_weight,radius,power,MULTIPLY_BY_Q)
 !
 ! Here I want to approx the integral
 !
 ! 1/vol \int_vol F(q) d3q = 1/vol \int_vol d3q/q^power [q^power F(q)] =
 !                         = 1/vol \sum_{Qi} \int_Si d3q/q^power [q^power F(q)] ~
 !                         ~ 1/vol \sum_{Qi} [Qi^power F(Qi)] \int_Si d3q/q^power
 !
 ! with Si a sphere of radius "radius" and volume vol/Np. 
 !
 ! a) MULTIPLY_BY_Q=.TRUE. 
 !
 ! If F(q) is divergent at gamma and it goes like 1/q^power. Therefore [q^power F(q)] is regular.
 ! I use this property to regularize the integral and rewrite 
 ! 
 !  1/vol \int_vol F(q) d3q = 1/vol \int_vol q^power/q^power F(q) d3q 
 ! 
 ! with
 ! 
 !  1/vol \int_vol F(q) d3q ~ \sum_{Qi} Wi F(Qi)
 ! 
 ! and
 !  
 !  Wi = 1/vol Qi^power \int_Si d3q/q^power
 ! 
 ! and Wi stored in p_weight
 ! 
 !  a) MULTIPLY_BY_Q=.FALSE. 
 ! 
 ! In this case I want to integrate the 1/q^power straight away, i.e. F(q)=1/q^power
 ! 
 !  1/vol \int_vol F(q) d3q = 1/vol \int_vol 1./q^power d3q  ~ \sum_{Qi} Wi
 ! 
 ! with
 ! 
 !  Wi = 1/vol \int_Si d3q/q^power
 ! 
 ! and Wi stored in p_weight
 ! 
 use pars,          ONLY:SP,pi,DP
 use drivers,       ONLY:l_real_time
 use com,           ONLY:msg
 use stderr,        ONLY:intc
 use LIVE_t,        ONLY:live_timing
 use vec_operate,   ONLY:v_norm
 use D_lattice,     ONLY:alat
 use R_lattice,     ONLY:RIM_n_rand_pts
 implicit none
 !
 integer,  intent(in)    :: Np,power
 real(SP), intent(in)    :: points(Np,3),radius
 real(SP), intent(inout) :: p_weight(Np)
 logical , intent(in)    :: MULTIPLY_BY_Q
 !
 ! Work Space
 !
 integer   :: ip,ir,N_out,N_in,ic
 real(SP)  :: v_rand(3),qr(RIM_n_rand_pts,3),sphere_vol,box_vol,pt_cc(3)
 character(12)      :: ch(3)
 integer            :: iseed(8)
 real(DP), external :: dlaran
 !
 if (RIM_n_rand_pts==0)then
   do ip=1,Np
     p_weight(ip)=0.
     pt_cc=points(ip,:)*2.*pi/alat(:)
     if(     MULTIPLY_BY_Q) p_weight(ip)=1./float(Np)
     if(.not.MULTIPLY_BY_Q.and.v_norm(pt_cc)>0._SP) p_weight(ip)=1./float(Np)/v_norm(pt_cc)**power
   enddo
   return
 endif
 !
 ! Filling a larger Sphere with a random grid
 !===========================================
 !
 if (l_real_time     ) call section('=','Spherical RIM with a N='//trim(intc(power))//' power')
 if (.not.l_real_time) call section('p','Spherical RIM with a N='//trim(intc(power))//' power')
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
 call live_timing('Random points',RIM_n_rand_pts)
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
   call live_timing(steps=1)
   !
 enddo loop
 call live_timing()
 call msg('r','Points outside the Sphere  ',N_out)
 !  
 !Integrated Sphere VOLUME 
 !
 box_vol   =(2.*radius*1.2)**3.
 sphere_vol=4./3.*pi*radius**3.
 !
 call msg('r', 'Sphere volume       [au]',sphere_vol)
 call msg('rn','Integrated volume   [au]',box_vol*real(RIM_n_rand_pts)/real(N_out))
 !
 call live_timing('Integrals',Np)
 !
 do ip=1,Np
   p_weight(ip)=0.
   pt_cc=points(ip,:)*2.*pi/alat(:)
   do ir=1,RIM_n_rand_pts
     p_weight(ip)=p_weight(ip)+1./v_norm(qr(ir,:)+pt_cc(:))**power*box_vol/real(N_out)
   enddo
   p_weight(ip)=p_weight(ip)/sphere_vol/real(Np)
   !
   if (MULTIPLY_BY_Q) p_weight(ip)=p_weight(ip)*v_norm(pt_cc)**power
   !
   call live_timing(steps=1)
 enddo
 !
 call live_timing()
 !
end subroutine
