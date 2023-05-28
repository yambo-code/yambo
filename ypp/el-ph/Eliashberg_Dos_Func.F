!
!        Copyright (C) 2000-2023 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): CA
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
real(SP) function Eliashberg_Dos_Func(iq,im,ph_en, ph_broad, PH_E, q) 
 !
 use pars,                ONLY:SP
 use electrons,           ONLY:levels
 use R_lattice,           ONLY:bz_samp
 use functions,           ONLY:Fermi_fnc_derivative
 use ELPH,                ONLY:PH_freqs_sq,use_PH_DbGd
 !
#include<memory.h>
 !
 integer,      intent(in) :: iq,im
 real(SP),     intent(in) :: ph_en,ph_broad
 type(levels), intent(in) :: PH_E
 type(bz_samp),intent(in) :: q
 !
 ! work space
 !
 real(SP) :: omega,weight
 integer  :: iq_fg,iE_fg,iqbz
 !     
 omega = sqrt( MAX( PH_freqs_sq(iq,im),0. ) )
 !
 Eliashberg_Dos_Func=Fermi_fnc_derivative(ph_en-omega,ph_broad)
 !
 if(.not.use_PH_DbGd) return
 !
 iqbz=q%k_table(iq,1)
 !
 weight=1.0/(q%FGbz%k_range(iqbz,2)-q%FGbz%k_range(iqbz,1)+1._SP)
 !
 do iq_fg=q%FGbz%k_range(iqbz,1)+1,q%FGbz%k_range(iqbz,2),1
   iE_fg =q%FGbz%E_map(iq_fg)
   omega=PH_E%FG%E(im,iE_fg,1)
   Eliashberg_Dos_Func=Eliashberg_Dos_Func+Fermi_fnc_derivative(ph_en-omega,ph_broad)
 enddo
 !
 Eliashberg_Dos_Func=Eliashberg_Dos_Func*weight
 !
end function
