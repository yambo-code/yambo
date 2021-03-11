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
subroutine En_k_plus_q_interpolate(k,q,q_fine,E,GKKP)
 !
 use pars,                ONLY:SP,pi
 use electrons,           ONLY:levels,FineGd_E_components_alloc
 use R_lattice,           ONLY:bz_samp,bz_samp_reset
 use interpolate,         ONLY:INTERPOLATION_BZ,INTERP_obj,INTERPOLATION_coefficients
 use ELPH,                ONLY:GKKP_me,elph_nb
 !
#include<memory.h>
 !
 type(bz_samp), intent(in)  :: k,q,q_fine
 type(levels) , intent(in)  :: E
 type(GKKP_me), intent(out) :: GKKP
 !
 ! Work space
 !
 integer               :: iq,iq_bz,ik,nq_around,n_k_plus_q,ic
 real(SP), allocatable :: E_1D_array(:,:)
 type(bz_samp) :: k_plus_q
 integer  :: ID_E_kpq=1
 !
 ! Setup interpolation
 ! ========================= 
 call INTERPOLATION_BZ_setup(k)
 INTERP_obj(ID_E_kpq)%what="E(k+q)"
 call INTERPOLATION_coefficients(E=E,k=k,ID=ID_E_kpq,ID_obj=ID_E_kpq)
 !
 ! Allocation of the Double Grids for each k+q points
 ! =====================================================
 n_k_plus_q=0
 !
 do iq=1,q%nibz
   iq_bz    =q%k_table(iq,q%star(iq,1))
   nq_around=q%FGbz%k_range(iq_bz+1,1)-q%FGbz%k_range(iq_bz,1)  ! number of q_fine points around a give iq point
   GKKP%E_kpq(iq)%FG%nb=elph_nb
   GKKP%E_kpq(iq)%FG%nk=nq_around*k%nibz                         
   call FineGd_E_components_alloc(GKKP%E_kpq(iq))
   n_k_plus_q=n_k_plus_q+nq_around*k%nibz
 enddo
 !
 ! Generate k'=k+q  vectors
 ! =========================
 call bz_samp_reset(k_plus_q)
 k_plus_q%nibz=n_k_plus_q
 YAMBO_ALLOC(k_plus_q%pt,(k_plus_q%nibz,3))
 !
 do iq=1,q%nibz
   iq_bz    =q%k_table(iq,q%star(iq,1))
   do iq_fine=q%FGbz%k_range(iq_bz,1),q%FGbz%k_range(iq_bz,1)
     do ik=1,k%nibz
       k_plus_q%ptbz(,:)=k%pt(ik,:)+q_fine%ptbz(iq_fine,:) 
     enddo
  enddo
 enddo
 !
 ! Interpolate Energies
 ! ==========================================
 YAMBO_ALLOC(E_1D_array,(E%nb,k_plus_q%nibz))
 call INTERPOLATION_BZ(K=k_plus_q,NK=k_plus_q%nibz,R1D=E_1D_array,ID=ID_E_kpq)
 !
! do iq=1,q%nibz
!   do ikbz=1,k%nbz
!     GKKP%E_kpq(iq)%
!   enddo
! enddo
 !
 ! Free memory
 ! ============
 YAMBO_FREE(E_1D_array)
 call bz_samp_reset(k_plus_q)
 !
end subroutine
