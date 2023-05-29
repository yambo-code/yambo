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
subroutine En_k_plus_q_interpolate(k,q,E,iqbz)
 !
 use pars,                ONLY:pi,rZERO
 use electrons,           ONLY:levels,n_sp_pol,E_reset,spin_occ
 use R_lattice,           ONLY:bz_samp,bz_samp_reset
 use ELPH,                ONLY:elph_nb,ID_E_kpq,GKKP,FineGd_E_kpq_alloc,Efermi_DbGd
 use interpolate,         ONLY:INTERPOLATION_BZ
 !
#include<memory.h>
 !
 type(bz_samp), intent(in)   :: k,q
 type(levels) , intent(in)   :: E
 !
 ! Work space
 !
 integer       :: iqbz,ik,n_k_plus_q,ic,iq_fine,iq_idx
 integer       :: nq_around
 type(bz_samp) :: k_plus_q
 type(levels)  :: E_kpq
 !
 if (n_sp_pol==2) call error("En_k_plus_q_interpolate to be checked for n_sp_pol==2")
 !
 ! Allocation of the Double Grids for each k+q points
 ! =====================================================
 n_k_plus_q=0
 !
 ! Number of q-fine points around each q-point
 ! ============================================
 !
 nq_around=q%FGbz%k_range(iqbz,2)-q%FGbz%k_range(iqbz,1)
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
 do ik=1,k%nibz
  do iq_fine=q%FGbz%k_range(iqbz,1)+1,q%FGbz%k_range(iqbz,2),1
     k_plus_q%pt(ic,:)=k%pt(ik,:)+q%FGbz%pt(iq_fine,:)
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
 ! Calculate the new occupations 
 ! ==============================
 ! For semiconductors I do not recalculate occupations
 ! I supposed fine and course grid have the same metallicity
 ! and I set all valence occupations to 1(or 2 without spin)
 !
 YAMBO_ALLOC(E_kpq%f,(E%nb,n_k_plus_q, n_sp_pol))
 if(all(E%nbm==E%nbf)) then
   E_kpq%f(1:E%nbf(1),:,1) =spin_occ
   E_kpq%f(E%nbf(1)+1:,:,1)=rZERO
 else 
   !
   ! For metals occuations are recalculated
   ! using the Fine-grid fermi energy
   !
   call f_build_ext(E_kpq,Efermi_DbGd) ! Interpolate levels are already aligned
 endif
 !
 ! Store E(k+q) energies in the GKKP
 ! ==========================================
 GKKP%E_dg_kpq(iqbz)%nk       =k%nibz
 GKKP%E_dg_kpq(iqbz)%nq_around=nq_around
 call FineGd_E_kpq_alloc(GKKP%E_dg_kpq(iqbz))
 !
 ic=1
 do ik=1,k%nibz
  do iq_fine=q%FGbz%k_range(iqbz,1)+1,q%FGbz%k_range(iqbz,2),1
    iq_idx=iq_fine-q%FGbz%k_range(iqbz,1)
    GKKP%E_dg_kpq(iqbz)%E_kpq(:elph_nb,ik,:,iq_idx)=E_kpq%E(:elph_nb,ic,:)
    GKKP%E_dg_kpq(iqbz)%f_kpq(:elph_nb,ik,:,iq_idx)=E_kpq%f(:elph_nb,ic,:)
    ic=ic+1
   enddo
 enddo
 !
 ! Free memory
 ! ============
 call bz_samp_reset(k_plus_q)
 call E_reset(E_kpq)
 !
end subroutine
