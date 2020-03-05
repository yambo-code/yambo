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
subroutine cutoff_sphere()
 !
 use pars,          ONLY:SP,pi
 use wave_func,     ONLY:wf_ng
 use R_lattice,     ONLY:cyl_ph_radius,bare_qpg,nqibz
 !
 implicit none
 !
 ! Work Space
 !
 integer ::iq,ig
 real(SP)::radius,d_cos,pre_factor
 !
 do iq=1,nqibz
   do ig=1,wf_ng
     !
     radius=real(bare_qpg(iq,ig),SP)*cyl_ph_radius
     d_cos=cos(mod(radius,2*pi))
     !
     pre_factor=sqrt(1._SP -d_cos)
     if (abs(pre_factor)<1.E-5_SP) pre_factor=1.E-5_SP
     !
     bare_qpg(iq,ig)=bare_qpg(iq,ig)/pre_factor
     !
   enddo
 enddo
 bare_qpg(1,1)=sqrt(2.0_SP)/cyl_ph_radius
 !
 !
end subroutine
