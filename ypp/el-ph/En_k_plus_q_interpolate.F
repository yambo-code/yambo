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
 use pars,                ONLY:SP,pi,rZERO
 use electrons,           ONLY:levels,FineGd_E_components_alloc,n_sp_pol,E_reset,spin_occ
 use R_lattice,           ONLY:bz_samp,bz_samp_reset
 use interpolate,         ONLY:INTERPOLATION_BZ,INTERP_obj,INTERPOLATION_coefficients,INTERPOLATE_is_quiet, &
         &                     INTERPOLATE_is_serial
 use ELPH,                ONLY:GKKP_me,elph_nb
 use interfaces,          ONLY:ELPH_alloc
 use LIVE_t,              ONLY:live_timing
 !
#include<memory.h>
 !
 type(bz_samp), intent(in)   :: k,q,q_fine
 type(levels) , intent(in)   :: E
 type(GKKP_me), intent(out)  :: GKKP
 !
 ! Work space
 !
 integer               :: iq,ik,n_k_plus_q,ic,iq_fine
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
 call ELPH_alloc("DGRID",GKKP)
 !
 call live_timing('E(k+q) energy interpolation ',q%nibz)
 !
 INTERPOLATE_is_quiet=.TRUE.
 !
 if(E%nbm/=E%nbf) call error("This code for the moment works only for semiconductors/insulators")
 !
 do iq=1,q%nbz
   !
   ! Number of q-fine points around each q-point
   ! ============================================
   !
   nq_around=q%FGbz%k_range(iq,2)-q%FGbz%k_range(iq,1)+1
   n_k_plus_q=nq_around*k%nibz
   !
   ! Generate k'=k+q  vectors
   ! ============================================
   call bz_samp_reset(k_plus_q)
   !
   if(allocated(k_plus_q%pt)) then
     YAMBO_FREE(k_plus_q%pt)
   endif
   !
   k_plus_q%nibz=n_k_plus_q
   YAMBO_ALLOC(k_plus_q%pt,(k_plus_q%nibz,3))
   !
   ic=1
   do iq_fine=q%FGbz%k_range(iq,1),q%FGbz%k_range(iq,1)
     do ik=1,k%nibz
       k_plus_q%pt(ic,:)=k%pt(ik,:)+q_fine%ptbz(iq_fine,:) 
       ic=ic+1
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
   ! Calculate the new occupations (only for semiconductors) 
   ! ==============================
   ! This part has to be generilized to metals, taking into account 
   ! the fact the the Fermi energy can change due to the presence of E(k+q) states
   ! For the moment I set all valence occupations to 1(or 2 without spin)
   !
   E_kpq%f(1:E%nbf,:,1)=spin_occ
   E_kpq%f(E%nbf+1,:,1)=rZERO
   !
   ! Store E(k+q) energies in the GKKP
   ! ==========================================
   GKKP%E_dg_kpq(iq)%FG%nb=elph_nb
   GKKP%E_dg_kpq(iq)%FG%nk=n_k_plus_q                         
   call FineGd_E_components_alloc(GKKP%E_dg_kpq(iq))
   !
   GKKP%E_dg_kpq(iq)%FG%E(:elph_nb,:,:)=E_kpq%E(:elph_nb,:,:)
   GKKP%E_dg_kpq(iq)%FG%f(:elph_nb,:,:)=E_kpq%f(:elph_nb,:,:)
   !
   call live_timing(steps =1)
   !
 enddo
 !
 call live_timing( )
 !
 ! Free memory
 ! ============
 call bz_samp_reset(k_plus_q)
 call E_reset(E_kpq)
 !
end subroutine
