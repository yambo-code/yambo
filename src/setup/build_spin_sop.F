!
!        Copyright (C) 2000-2019 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DS
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
subroutine build_spin_sop()
 !
 use pars,           ONLY:SP,pi,cI,cONE
 use matrix_operate, ONLY:m3det
 use D_lattice,      ONLY:dl_sop,spin_sop,nsym,i_time_rev,&
&                         sigma_0,sigma_x,sigma_y,sigma_z
 use electrons,      ONLY:n_spinor
 !
#include<memory.h>
 !
 complex(SP)    ::t_rev(2,2)
 complex(SP)    ::spin_RX_delta(2,2),spin_RY_beta(2,2),spin_RZ_alpha(2,2)
 real(SP)       ::alpha,beta,delta,tmp_sop(3,3)
 integer        ::is,i1,i2
 !DEBUG <
 !real(SP)       ::angles(3,nsym)
 !DEBUG >
 !
 YAMBO_FREE(spin_sop)
 !
 YAMBO_ALLOC(spin_sop,(n_spinor,n_spinor,nsym))
 !
 if(n_spinor==1) then
   spin_sop(1,1,:)=cONE
   return
 endif
 !
 ! T_rev=(-i sigma_y K0 ) with K0 the complex conjugation.
 t_rev=-cI*sigma_y
 !
 do is=1,nsym
   !
   tmp_sop=dl_sop(:,:,is)
   !
   ! Trev symm must be excluded to compute the rotation angles
   if(is>nsym/(1+i_time_rev)) tmp_sop=-tmp_sop
   !
   ! Spin is invariant under spatial inversion. I need only the rotations.
   tmp_sop= tmp_sop*m3det(tmp_sop)
   !
   !           "Tait-Bryan angles"
   if (abs(tmp_sop(3,1)+1._SP)<1.E-5_SP) then
     alpha=0._SP
     beta=-pi/2._SP
     delta=atan2(tmp_sop(1,2),tmp_sop(1,3))
   elseif (abs(tmp_sop(3,1)-1._SP)<1.E-5_SP) then
     alpha=0._SP
     beta= pi/2._SP
     delta=atan2(-tmp_sop(1,2),-tmp_sop(1,3))
   else
     beta= asin(tmp_sop(3,1))
     alpha=atan2(tmp_sop(2,1)/cos(beta),tmp_sop(1,1)/cos(beta))
     delta=atan2(tmp_sop(3,2)/cos(beta),tmp_sop(3,3)/cos(beta))
   endif
   !
   alpha=alpha/2._SP
   beta= beta /2._SP
   delta=delta/2._SP
   !
   ! DEBUG <
   !angles(:,is)=(/alpha,beta,delta/)/pi*180._SP
   ! DEBUG >
   !
   ! Sakurai p.166, eq. 3.2.44 + pag 159 eq. 3.2.3
   spin_RX_delta=sigma_0*cos(delta)-cI*sigma_x*sin(delta)
   spin_RY_beta =sigma_0*cos(beta) -cI*sigma_y*sin(beta)
   spin_RZ_alpha=sigma_0*cos(alpha)-cI*sigma_z*sin(alpha)
   spin_sop(:,:,is)=matmul(spin_RZ_alpha,matmul(spin_RY_beta,spin_RX_delta))
   !
   if(is>nsym/(1+i_time_rev)) spin_sop(:,:,is)=matmul(t_rev,spin_sop(:,:,is))
   !
   do i1=1,2
     do i2=1,2
       if(abs(aimag(spin_sop(i1,i2,is)))<1.E-5_SP) spin_sop(i1,i2,is)=cmplx( real(spin_sop(i1,i2,is)),0._SP)
       if(abs(real(spin_sop(i1,i2,is))) <1.E-5_SP) spin_sop(i1,i2,is)=cmplx(0._SP,aimag(spin_sop(i1,i2,is)))
     enddo
   enddo
   !
 enddo
 !
 ! DEBUG <
 !do is=1,nsym
 !  write(*,*) is, angles(:,is)
 !  do i1=1,2
 !    write(*,*) i1,spin_sop(:,i1,is)
 !  enddo
 !enddo
 ! DEBUG >
 !
end subroutine
