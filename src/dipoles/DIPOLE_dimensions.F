!
!        Copyright (C) 2000-2016 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM, DS
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
subroutine DIPOLE_dimensions(Xen,Dip,bands,q0)
 !
 use pars,           ONLY:SP
 use electrons,      ONLY:levels
 use DIPOLES,        ONLY:DIPOLE_t
 use X_m,            ONLY:l_X_terminator
 use wave_func,      ONLY:wf_ng
use com,             ONLY:warning
#if defined _SC
 use drivers,        ONLY:l_real_time,l_sc_run
 use DIPOLES,        ONLY:P_square
#endif
#if defined _QED
 use drivers,        ONLY:l_elphoton_corr
#endif
 !
 implicit none
 !
 integer,       intent(in)    :: bands(2)
 type(levels),  intent(in)    :: Xen
 type(DIPOLE_t),intent(inout) :: Dip
 real(SP),      intent(in)    :: q0(3)
 !
 !
 ! NOTE: 1 - that in case we will enter here during a SC run to update the
 !           screened interaction all transitions must be considered in order to rotate <P>.
 !       2 - when using internal SC potentials oscilators must be rotated
 !           in the new basis. So all transitions are needed.
 !       3 - In real-time simulations P and P^2 are ALWAYS calculated => no band ordering
 !
 Dip%ng=wf_ng
 Dip%q0=q0
 Dip%ib=bands
 !
 Dip%bands_ordered=.true.
 !
#if defined _SC 
 if ( l_sc_run.or.l_real_time ) Dip%bands_ordered=.false.
#endif
#if defined _QED 
 if (l_elphoton_corr)           Dip%bands_ordered=.false.
#endif
 !
 ! Set up band limits
 !
 if (Dip%bands_ordered.or.Dip%Energy_treshold<0._SP) then
   Dip%ib_lim(1)=Xen%nbm
   Dip%ib_lim(2)=Xen%nbf+1
   if (l_X_terminator) Dip%ib_lim(2)=Dip%ib(1)
 else
   Dip%ib_lim(1)=Dip%ib(2)
   Dip%ib_lim(2)=Dip%ib(1)
 endif
 !
end subroutine DIPOLE_dimensions
