!
!        Copyright (C) 2000-2021 the YAMBO team
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
subroutine En_k_plus_q_interpolate(k,q_fine,E,GKKP)
 !
 use pars,                ONLY:SP,pi
 use electrons,           ONLY:levels
 use R_lattice,           ONLY:bz_samp,bz_samp_reset
 use interpolate,         ONLY:INTERPOLATION_BZ,INTERP_obj,INTERPOLATION_coefficients
 use ELPH,                ONLY:GKKP_me
 !
#include<memory.h>
 !
 type(bz_samp), intent(in)  :: k,q_fine
 type(levels) , intent(in)  :: E
 type(GKKP_me), intent(out) :: GKKP
 !
 ! Work space
 !
 integer               :: iq,ik
 real(SP), allocatable :: E_1D_array(:,:)
 integer  :: ID_E_kpq=1
 type(bz_samp) :: k_plus_q
 !
 ! Setup interpolation
 ! ========================= 
 call INTERPOLATION_BZ_setup(k)
 INTERP_obj(ID_E_kpq)%what="E(k+q)"
 call INTERPOLATION_coefficients(E=E,k=k,ID=ID_E_kpq,ID_obj=ID_E_kpq)
 !
 ! Generate k'=k+q 
 ! =========================
 call bz_samp_reset(k_plus_q)
 k_plus_q%nibz=k%nibz*q_fine%nbz
 YAMBO_ALLOC(k_plus_q%pt,(q_fine%nibz,3))
 !
 do ik=1,k%nibz
   do iq=1,q_fine%nbz
     k_plus_q%pt(iq+(ik-1)*q_fine%nbz,:)=k%pt(ik,:)+q_fine%ptbz(iq,:) 
   enddo
 enddo
 !
 ! Interpolate Energies
 YAMBO_ALLOC(E_1D_array,(E%nb,k_plus_q%nibz))
 call INTERPOLATION_BZ(K=k_plus_q,NK=k_plus_q%nibz,R1D=E_1D_array,ID=ID_E_kpq)
 !
 do ik=1,k%nibz
   do iq=1,q_fine%nbz
!     E_k_plus_q(ik, iq,:)=E_1D_array(:,iq+(ik-1)*q_fine%nbz)
   enddo
 enddo
 !
 ! Free memory
 ! ============
 YAMBO_FREE(E_1D_array)
 call bz_samp_reset(k_plus_q)
 !
end subroutine
