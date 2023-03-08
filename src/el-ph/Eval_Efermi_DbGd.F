!
!        Copyright (C) 2000-2021 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AC
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
function Eval_Efermi_DbGd(k,q,en)
 !
 use pars,          ONLY:SP
 use R_lattice,     ONLY:bz_samp,bz_samp_reset
 use electrons,     ONLY:n_sp_pol,levels,E_reset
 use ELPH,          ONLY:elph_nb,ID_E_kpq
 use units,         ONLY:HA2EV
 use interpolate,   ONLY:INTERPOLATION_BZ
 use interfaces,    ONLY:OCCUPATIONS_Fermi
 use com,           ONLY:msg
 !
#include<memory.h>
 !
 type(bz_samp), intent(in)  :: q,k
 type(levels) :: en
 !
 ! Work space
 !
 real(SP) :: Eval_Efermi_DbGd
 integer  :: ikbz,iq_fine,N_k_plus_q,ic,ik
 type(bz_samp) :: all_k_plus_q
 type(levels)  :: all_E_kpq
 !
 if(any(elph_nb<en%nbm)) &
&      call error('Number of bands in EL-PH should be => number of metallic bands')
 !
 call bz_samp_reset(all_k_plus_q)
 if(allocated(all_k_plus_q%pt)) then
   YAMBO_FREE(all_k_plus_q%pt)
 endif
 !
 ! In principle should be N_k_plus_q=k%nbz*q%FGbz%N
 ! see comment below
 !
 N_k_plus_q=q%FGbz%N
 all_k_plus_q%nibz=N_k_plus_q
 YAMBO_ALLOC(all_k_plus_q%pt,(all_k_plus_q%nibz,3))
 YAMBO_ALLOC(all_k_plus_q%weights,(N_k_plus_q))
 !
 all_k_plus_q%weights=1._SP/real(N_k_plus_q,SP)
 !
 call warning('Fermi Energy is calculated on the Double-Grid E(q_fine) ')
 !
 ! In principle one should include all the point
 ! E(k_i + q_j^fine) where i=1,k%nbz and j=1,q%FGbz%N
 ! but this grid is too large, I calculate the Fermi
 ! energy on the grid E(q_fine) that is supposed to be
 ! much larger than E(k)
 !
 !=============================================
 !
 all_k_plus_q%pt(:,:)=q%FGbz%pt(:,:)
 !
 ! Interpolate All Energies
 ! ==========================================
 call E_reset(all_E_kpq)
 all_E_kpq%nb=elph_nb
 all_E_kpq%nk=N_k_plus_q
 YAMBO_ALLOC(all_E_kpq%E,(elph_nb,N_k_plus_q, n_sp_pol))
 call INTERPOLATION_BZ(K=all_k_plus_q,NK=N_k_plus_q,ID=ID_E_kpq,E=all_E_kpq)
 !
 call OCCUPATIONS_Fermi(all_E_kpq,all_k_plus_q,'DbGrid',n_bands=elph_nb,mode='FERMI')
 Eval_Efermi_DbGd=all_E_kpq%E_Fermi
 call msg('rs','Fermi Energy shift due to the Double-Grid : ',Eval_Efermi_DbGd*HA2EV,'eV')
 !
end function Eval_Efermi_DbGd

