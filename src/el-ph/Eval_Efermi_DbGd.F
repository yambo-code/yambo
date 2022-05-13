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
function Eval_Efermi_DbGd(k,q)
 !
 use pars,          ONLY:SP
 use R_lattice,     ONLY:bz_samp
 use electrons,     ONLY:n_sp_pol,levels
 use ELPH,          ONLY:elph_nb,ID_E_kpq
 !
#include<memory.h>
 !
 type(bz_samp), intent(in)  :: q,k
 type(levels),  intent(in)  :: E
 !
 ! Work space
 !
 real(SP) :: Eval_Efermi_DbGd
 integer  :: ikbz,iq_fine,N_k_plus_q,ic
 type(bz_samp) :: all_k_plus_q
 type(levels)  :: all_E_kpq
 !
 call bz_samp_reset(all_k_plus_q)
 if(allocated(all_k_plus_q%pt)) YAMBO_FREE(all_k_plus_q%pt)
 !
 N_k_plus_q=k%nbz*q%FGbz%N
 all_k_plus_q%nibz=N_k_plus_q
 YAMBO_ALLOC(all_k_plus_q%pt,(all_k_plus_q%nibz,3))
 !
 ic=1
 do ik=1,k%nbz
   do iq_fine=q%FGbz%N
     all_k_plus_q%pt(ic,:)=k%ptbz(ic,:)+q%FGbz%pt(iq_fine,:)
     ic=ic+1
   enddo
 enddo
 !
 ! Interpolate All Energies
 ! ==========================================
 call E_reset(all_E_kpq)
 all_E_kpq%nb=elph_nb
 all_E_kpq%nk=N_k_plus_q
 YAMBO_ALLOC(all_E_kpq%E,(elph_nb,n_k_plus_q, n_sp_pol))
 call INTERPOLATION_BZ(K=k_plus_q,NK=N_k_plus_q,ID=ID_E_kpq,E=all_E_kpq)
 !
 call OCCUPATIONS_Fermi(all_E_kpt,all_k_plus_q)
 !
 Eval_Efermi_DbGd=0._SP
 !
end function Eval_Efermi_DbGd

