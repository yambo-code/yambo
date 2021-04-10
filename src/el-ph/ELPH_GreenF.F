!
!        Copyright (C) 2000-2020 the YAMBO team
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
!> @brief Calculate real and imaginary part of the electron-phonon self-energy
!!
!! @param[in]       i_qp                quasi-particle index
!! @param[in]       iq_bz               q-index in the BZ
!! @param[in]       iq_mem              q-index in distributed array
!! @param[in]       il                  phonon-branch index
!! @param[in]       en                  energy levels
!! @param[in]       Sc_W                energy where self-energy is calculated
!! @param[in]       elph_gkkp_sq        electron-phonon matrix elements squares <ib,k | \partial V /\partial u_il | ob, k-q>
!!
!
subroutine ELPH_GreenF(i_qp,ob,ok,iq_bz,iq_ibz,iq_mem,il,elph_gkkp_sq,en,k,Sc_W,q_weight)
 !
 use pars,          ONLY:SP,pi
 use electrons,     ONLY:levels,spin_occ
 use frequency,     ONLY:w_samp
 use ELPH,          ONLY:gsqF_fan,gsqF_dw,gsqF_ca_corr,gsqF_life_bose,gsqF_life_f,GKKP,&
&                        gsqF_energy_steps,gsqF_dw,eval_G_using_KK,elph_use_q_grid,&
&                        FAN_deltaE_treshold,DW_deltaE_treshold,PH_freqs_sq,use_PH_DbGd
 use QP_m,          ONLY:QP_table,QP_time_order_sign
 use R_lattice,     ONLY:bz_samp
 use functions,     ONLY:Fermi_fnc_derivative
 !
#include<memory.h>
 !
 integer,      intent(in) :: i_qp
 integer,      intent(in) :: ob
 integer,      intent(in) :: ok
 integer,      intent(in) :: iq_bz,iq_ibz,iq_mem
 integer,      intent(in) :: il
 real(SP),     intent(in) :: elph_gkkp_sq,q_weight
 type(levels), intent(in) :: en
 type(bz_samp),intent(in) :: k
 type(w_samp), intent(in) :: Sc_W
 !
 ! Work Space
 !
 integer  :: iw
 complex(SP) :: delta_E
 real(SP) :: delta_E_at_gamma,f_occ,E_kpq
 real(SP) :: one_minus_2f_occ,gsqF_damping,ph_E
 integer  :: ik,ik_bz,ib
 !
 ! Indexes
 !
 ib   =QP_table(i_qp,1)
 ik   =QP_table(i_qp,3)
 ik_bz=sum(k%nstar(:ik-1))+1
 !
 ! Phonon frequency
 !
 ph_E=sqrt(abs(PH_freqs_sq(iq_ibz,il)))
 !
 ! Skip modes @ Gamma (1st point is always gamma, either with random
 ! or uniform grids, as it is needed to evaluate the DW factor) 
 !
 if (abs(ph_E)<FAN_deltaE_treshold) return
 !
 ! Define outgoing energies and occupations
 !
 if (elph_use_q_grid) then
   E_kpq=en%E(ob,ok,1)
   f_occ=en%f(ob,ok,1)/spin_occ
 else
   E_kpq=GKKP%E_kpq(iq_bz)%E(ob,ik_bz,1)
   f_occ=GKKP%E_kpq(iq_bz)%f(ob,ik_bz,1)/spin_occ
 endif
 !
 ! Frequency Loop
 !
 do iw=1,gsqF_energy_steps
    !
    ! Define gsqF denominators
    !
    delta_E=Sc_W%p(iw)-E_kpq
    !
    one_minus_2f_occ=(1._SP-2._SP*f_occ)
    gsqF_damping=aimag(Sc_W%p(iw))
    !
    ! Lifetimes
    !-----------
    !
    ! "Bose" Term
    !
    gsqF_life_bose(i_qp,iq_mem,il,iw)= &
&       gsqF_life_bose(i_qp,iq_mem,il,iw)+2.*pi*elph_gkkp_sq*q_weight*&
&       ( Fermi_fnc_derivative(real(delta_E)+ph_E,gsqF_damping) + &
&       QP_time_order_sign* Fermi_fnc_derivative(real(delta_E)-ph_E,gsqF_damping) )
    !
    ! "f" Term
    !
    gsqF_life_f(i_qp,iq_mem,il,iw)= &
&       gsqF_life_f(i_qp,iq_mem,il,iw)+pi*elph_gkkp_sq*q_weight*&
&       ( ( Fermi_fnc_derivative(real(delta_E)+ph_E,gsqF_damping) - &
&       QP_time_order_sign* Fermi_fnc_derivative(real(delta_E)-ph_E,gsqF_damping) )*f_occ-&
&       Fermi_fnc_derivative(real(delta_E)+ph_E,gsqF_damping) )
    !
    if (eval_G_using_KK) cycle
    !
    ! QP's energies
    !--------------
    !
    ! Correction to the Fan-DW term (not in Cardona-Allen paper, possibly
    ! important for metals).
    !
    gsqF_ca_corr(i_qp,iq_mem,il,iw)=gsqF_ca_corr(i_qp,iq_mem,il,iw)+elph_gkkp_sq* &
&                                ph_E*one_minus_2f_occ/(delta_E**2-ph_E**2)*q_weight
    !
    ! Cardona-Allen formulation
    !
    ! (a) Fan Term                                          this 2 is to be consistent with AC definition
    !                                                       |
    !                                                      \/
    gsqF_fan(i_qp,iq_mem,il,iw)=gsqF_fan(i_qp,iq_mem,il,iw)+2._SP*elph_gkkp_sq*&
&                                      delta_E/(delta_E**2-ph_E**2)*q_weight
    !
 enddo
 !
 ! The DW term comes from a perturbation theory expansion
 ! which does not allow zero-energy transitions between the
 ! perturbed stated and itself
 !
 if (elph_use_q_grid) then
   delta_E_at_gamma=en%E(ib,ik,1)  -en%E(ob,ik,1)
 else
   delta_E_at_gamma=en%E(ib,ik,1)  -GKKP%E_kpq(1)%E(ob,ik_bz,1)
 endif
 !
 if (abs(delta_E_at_gamma)<DW_deltaE_treshold) return
 !
 ! (b) Debye Waller Term
 !
 gsqF_dw(i_qp,iq_mem,il)=gsqF_dw(i_qp,iq_mem,il)-1._SP/2._SP*&
&                         2._SP*GKKP%dVr(il,ib,ob,ik_bz,1)/delta_E_at_gamma*q_weight/2._SP/ph_E
 !                          /\
 !                           |
 !                           this 2 is to be consistent with AC definition (see Eq. 5, PRB 31, 2163)
end subroutine
