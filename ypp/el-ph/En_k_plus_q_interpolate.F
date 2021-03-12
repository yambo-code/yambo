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
 use electrons,           ONLY:levels,FineGd_E_components_alloc,n_sp_pol,E_reset
 use R_lattice,           ONLY:bz_samp,bz_samp_reset
 use interpolate,         ONLY:INTERPOLATION_BZ,INTERP_obj,INTERPOLATION_coefficients,INTERPOLATE_is_serial
 use ELPH,                ONLY:GKKP_me,elph_nb
 !
#include<memory.h>
 !
 type(bz_samp), intent(in)   :: k,q,q_fine
 type(levels) , intent(in)   :: E
 type(GKKP_me), intent(out)  :: GKKP
 !
 ! Work space
 !
 integer               :: iq,ik,n_k_plus_q,ic,ik_idx,iq_fine
 integer               :: nq_around
 type(bz_samp) :: k_plus_q
 type(levels)  :: E_kpq
 integer  :: ID_E_kpq=1
 !
 ! Setup interpolation
 ! ========================= 
 INTERPOLATE_is_serial=.TRUE.
 call INTERPOLATION_BZ_setup(k)
 INTERP_obj(ID_E_kpq)%what="E(k+q)"
 call INTERPOLATION_coefficients(E=E,k=k,ID=ID_E_kpq,ID_obj=ID_E_kpq)
 !
 ! Allocation of the Double Grids for each k+q points
 ! =====================================================
 n_k_plus_q=0
 !
 ! number of q_fine points in the BZ around a give iq point in the IBZ
 !
 if(.not.allocated(GKKP%E_kpq)) allocate(GKKP%E_kpq(q%nibz))
 !
 do iq=1,q%nibz
   nq_around=q%FGibz%k_range(iq,2)-q%FGibz%k_range(iq,1)+1
   GKKP%E_kpq(iq)%FG%nb=elph_nb
   GKKP%E_kpq(iq)%FG%nk=nq_around*k%nibz                         
   call FineGd_E_components_alloc(GKKP%E_kpq(iq))
 enddo
 !
 n_k_plus_q=k%nibz*(sum(q%FGibz%k_range(:,2)+1)-sum(q%FGibz%k_range(:,1)))
 !
 ! Generate k'=k+q  vectors
 ! ============================================
 call bz_samp_reset(k_plus_q)
 k_plus_q%nibz=n_k_plus_q
 YAMBO_ALLOC(k_plus_q%pt,(k_plus_q%nibz,3))
 !
 ic=1
 do ik=1,k%nibz
   do iq=1,q%nibz
     do iq_fine=q%FGibz%k_range(iq,1),q%FGbz%k_range(iq,2)
       k_plus_q%pt(ic,:)=k%pt(ik,:)+q_fine%ptbz(iq_fine,:) 
       ic=ic+1
     enddo
   enddo
 enddo
 !
 ! Interpolate Energies
 ! ==========================================
 call E_reset(E_kpq)
 E_kpq%nb=E%nb
 E_kpq%nk=n_k_plus_q
 YAMBO_ALLOC(E_kpq%E,(E%nb,n_k_plus_q, n_sp_pol))
 call INTERPOLATION_BZ(K=k_plus_q,NK=n_k_plus_q,ID=ID_E_kpq,E=E_kpq)
 !
 ic=1
 do ik=1,k%nibz
   do iq=1,q%nibz
     nq_around=q%FGibz%k_range(iq,2)-q%FGbz%k_range(iq,1)+1
     ik_idx=(ik-1)*nq_around
     do iq_fine=1,nq_around
       GKKP%E_kpq(iq)%FG%E(:elph_nb,ik_idx+iq_fine,:)=E_kpq%E(:elph_nb,ic,:)
       ic=ic+1
     enddo
   enddo
 enddo
 !
 ! Free memory
 ! ============
 call bz_samp_reset(k_plus_q)
 call E_reset(E_kpq)
 !
end subroutine
