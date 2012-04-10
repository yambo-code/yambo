!
! Copyright (C) 2000-2012 A. Marini and the YAMBO team 
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

subroutine construct_spin_sop(spin_sop)
 !
 use pars,           ONLY:SP,pi
 use matrix_operate, ONLY:m3det
 use D_lattice,      ONLY:dl_sop,nsym,i_time_rev
 !
 implicit none
 !
 complex(SP)    ::spin_sop(2,2,nsym)
 real(SP)       ::alpha,beta,delta,tmp_sop(3,3)
 integer        ::is,i1,i2
 !
 do is=1,nsym
   ! Spin is invariant under spatial inversion
   tmp_sop=dl_sop(:,:,is)*m3det(dl_sop(:,:,is))
   if (tmp_sop(3,1)==-1.) then
     alpha=0.
     beta= pi/2.
     delta=atan2(tmp_sop(1,2),tmp_sop(1,3))
   elseif (tmp_sop(3,1)==+1.) then
     alpha=0.
     beta=-pi/2.
     delta=atan2(-tmp_sop(1,2),-tmp_sop(1,3))
   else
     beta=-asin(tmp_sop(3,1))
     alpha=atan2(tmp_sop(2,1)/cos(beta),tmp_sop(1,1)/cos(beta))
     delta=atan2(tmp_sop(3,2)/cos(beta),tmp_sop(3,3)/cos(beta))
   endif
   ! Landau, Vol.3 pagg 215-217 (U )
   spin_sop(1,1,is)= cos(beta/2)*cmplx(cos((alpha+delta)/2), sin((alpha+delta)/2))
   spin_sop(1,2,is)= sin(beta/2)*cmplx(cos((alpha-delta)/2),-sin((alpha-delta)/2))
   spin_sop(2,1,is)=-sin(beta/2)*cmplx(cos((alpha-delta)/2), sin((alpha-delta)/2))
   spin_sop(2,2,is)= cos(beta/2)*cmplx(cos((alpha+delta)/2),-sin((alpha+delta)/2))
   !
   ! T-rev including syms transforms as U^\dag=spin_metric*U*T_rev
   ! I still do not understand why spin_metric is there
   if(is>nsym/(1+i_time_rev)) spin_sop(:,:,is)=conjg(transpose(spin_sop(:,:,is)))
 enddo
 !
end subroutine

