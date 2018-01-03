!
!        Copyright (C) 2000-2018 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): MG
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
subroutine scatter_ModScr(isc,mu)
 !
 ! multiply scatterGamp by exp((-|q+G|^2)/(4\mu^2))
 !
 use pars,          ONLY:SP,pi
 use collision_el,  ONLY:elemental_collision
 use R_lattice,     ONLY:bare_qpg
 !
 implicit none
 !
 type(elemental_collision)::isc
 real(SP)                 :: mu
 !
 !Working Space
 !
 integer :: ng(2),ig1,ig2,iq,i1,i2
 !
 iq=isc%qs(2)
 isc%iqref=iq
 !
 ng=shape(isc%gamp)
 !
 do i1=1,ng(1)
   do i2=1,ng(2)
     ig1=i1
     ig2=i2
     if (ng(1)==1) ig1=ig2
     if (ng(2)==1) ig2=ig1
     isc%gamp(i1,i2)=isc%gamp(i1,i2)*&
          &(1._SP-real(exp(-(bare_qpg(iq,ig1)**2)/(4._SP*mu**2)),SP)) 
   enddo
 end do
 !
end subroutine
