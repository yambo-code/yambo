!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): FP
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
subroutine RT_PHEL_Scattering_step(iq,deltaT)
 ![NOTE] RT_PH_occupations are to be set somewhere after the time step calculation
 !
 ! The propagation of the scattering part is done in terms of phonons occupations (D lesser, N) + electron-hole pair occupations.
 ! The lifetimes used (computed by RT_relaxation in the PHEL case) are all constructed in terms of N, f_e, f_h.
 ! The time dependence of f_e, f_h comes from the coupled equation for the Glesser (see RT_Scattering_step).
 !
 ! Thus I propagate the following equation:
 !    \partial_t N = -GAMMA_eh*N -GAMMA_res -GAMMA_ares
 !
 ! NOTICE:
 ! - The equation is always solved with the EULER step
 ! - The reference occupations are updated during the dynamics and so the delta occupations,
 !   unless the relaxation time approximation is used
 !
 ! Only after the D lesser has been propagated, the occupations are updated
 ! Here remember that the occupations stored in the module are always equal to dG_inp
 !
 use pars,           ONLY:SP
 use ELPH,           ONLY:elph_branches
 !
 implicit none
 !
 integer,     intent(in)    :: iq
 real(SP),    intent(in)    :: deltaT
 !
 integer     :: il
 !
 !if( .not. RT_PHEL_is_dynamically_dephased) return
 !
 do il=elph_branches(1),elph_branches(2)
   !
   !N_lq  = RT_PH_occupations(il,iq)
   !
   !N0_lq = RT_PH_occupations_reference(il,iq)
   !dN_lq = RT_PH_delta_occupations(il,iq)
   !
   ![NOTE] Is this right??
   !
   !GAMMA0(1)=EQ_P_E_eh_lifetime(il,iq)
   !GAMMA0(2)=EQ_P_E_sp_res_lifetime(il,iq)+EQ_P_E_sp_ares_lifetime(il,iq)
   !
   !dGAMMA(1)=0._SP
   !dGAMMA(2)=0._SP
   !dGAMMA(1)=dGAMMA(1)+ P_E_eh_lifetime(il,iq)
   !dGAMMA(2)=dGAMMA(2)+ P_E_sp_res_lifetime(il,iq)+ P_E_sp_ares_lifetime(il,iq)
   !
   !if (RT_PHEL_is_statically_dephased) GAMMA0(1)=GAMMA0(1)+PHEL_Deph_Matrix(il,iq)
   !
   !GAMMA_(1)=GAMMA0(1)+dGAMMA(1)
   !GAMMA_(2)=GAMMA0(2)+dGAMMA(2)
   !
   !dN_lq_out = -deltaT*(GAMMA_(1)*N_lq+GAMMA_(2))
   !
   ! UPDATE OCCUPATIONS HERE
   !---------------------------------------------
   !RT_PH_occupations(il,iq)      = dN_lq_out + RT_PH_occupations_reference(il,iq)
   !RT_PH_delta_occupations(il,iq)= dN_lq_out
   !
 enddo
 !
end subroutine RT_PHEL_Scattering_step
