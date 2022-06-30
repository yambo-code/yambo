!
!        Copyright (C) 2000-2022 the YAMBO team
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
!! @param[out]      gsqF_fan            Fan self-energy, real-part
!! @param[out]      gsqF_dw             Debye-Waller sefl-energy
!! @param[out]      gsqF_ca_corr        Additional term in Fan self-energy for metals [A. Marini]
!! @param[out]      gsqF_life_bose      Imaginary part Fan self-energy Bose part
!! @param[out]      gsqF_life_f         Imaginary part Fan self-energy Fermi part
!
subroutine ELPH_GreenF(i_qp,ob,ok,iq_bz,iq_ibz,iq_mem,il,elph_gkkp_sq,en,k,q,Sc_W,q_weight)
 !
 use pars,          ONLY:SP,pi,cZERO
 use electrons,     ONLY:levels,spin_occ
 use frequency,     ONLY:w_samp
 use ELPH,          ONLY:gsqF_fan,gsqF_dw,gsqF_ca_corr,gsqF_life_bose,gsqF_life_f,GKKP,&
&                        gsqF_energy_steps,gsqF_dw,eval_G_using_KK,elph_use_q_grid,PH_freqs,&
&                        FAN_deltaE_treshold,DW_deltaE_treshold,PH_freqs_sq,use_PH_DbGd, &
&                        elph_grids_are_expanded
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
 type(bz_samp),intent(in) :: k,q
 type(w_samp), intent(in) :: Sc_W
 !
 ! Work Space
 !
 integer  :: iw
 integer  :: ik,ik_bz,ib,iE_fg,iq_fg,ic,nq_around,iq_idx
 complex(SP) :: delta_E
 real(SP) :: delta_E_at_gamma,f_occ,E_kpq
 real(SP) :: one_minus_2f_occ,gsqF_damping,ph_E
 real(SP) :: weight_dbg,DW_weight
 !
 logical  :: eval_DW
 !
 ! Residues
 !
 real(SP) ::gsqF_RES_life_B,gsqF_RES_life_f,gsqF_RES_ca_corr,gsqF_RES_fan,gsqF_RES_DW 
 !
 ! Denominators
 !
 real(SP) :: gsqF_DEN_life_B(gsqF_energy_steps)
 real(SP) :: gsqF_DEN_life_f(gsqF_energy_steps)
 real(SP) :: gsqF_DEN_ca_corr(gsqF_energy_steps)
 real(SP) :: gsqF_DEN_fan(gsqF_energy_steps)
 real(SP) :: gsqF_DEN_DW 
 !
 ! Indexes
 !
 ib   =QP_table(i_qp,1)
 ik   =QP_table(i_qp,3)
 !
 ik_bz=ik
 if(ik>1) ik_bz=sum(k%nstar(:ik-1))+1
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
 if (elph_use_q_grid.and..not.elph_grids_are_expanded) then
   E_kpq=en%E(ob,ok,1)
   f_occ=en%f(ob,ok,1)/spin_occ
 else
   E_kpq=GKKP%E_kpq(iq_bz)%E(ob,ik_bz,1)
   f_occ=GKKP%E_kpq(iq_bz)%f(ob,ik_bz,1)/spin_occ
 endif
 !
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
    gsqF_RES_life_B    =2._SP*pi*elph_gkkp_sq*q_weight
    gsqF_DEN_life_B(iw)=            (Fermi_fnc_derivative(real(delta_E)+ph_E,gsqF_damping)&
 &               +QP_time_order_sign*Fermi_fnc_derivative(real(delta_E)-ph_E,gsqF_damping))
    !
    ! "f" Term
    !
    gsqF_RES_life_f    =pi*elph_gkkp_sq*q_weight
    gsqF_DEN_life_f(iw)=  ((Fermi_fnc_derivative(real(delta_E)+ph_E,gsqF_damping) - &
&       QP_time_order_sign* Fermi_fnc_derivative(real(delta_E)-ph_E,gsqF_damping))*f_occ-&
&                           Fermi_fnc_derivative(real(delta_E)+ph_E,gsqF_damping))
    !
    if (.not.eval_G_using_KK) then 
      !
      ! QP's energies
      !--------------
      !
      ! Correction to the Fan-DW term (not in Cardona-Allen paper, possibly
      ! important for metals).
      !
      gsqF_RES_ca_corr    =elph_gkkp_sq*q_weight
      gsqF_DEN_ca_corr(iw)=ph_E*one_minus_2f_occ/(delta_E**2-ph_E**2)
      !
      ! Cardona-Allen formulation
      !
      ! (a) Fan Term                                          this 2 is to be consistent with AC definition
      !                                                       |
      gsqF_RES_fan    =2._SP*elph_gkkp_sq*q_weight
      gsqF_DEN_fan(iw)=delta_E/(delta_E**2-ph_E**2)
      !
    endif
    !
 enddo
 !
 ! The DW term comes from a perturbation theory expansion
 ! which does not allow zero-energy transitions between the
 ! perturbed stated and itself
 !
 if (elph_use_q_grid.and..not.elph_grids_are_expanded) then
   delta_E_at_gamma=en%E(ib,ik,1)               -en%E(ob,ik,1)
 else
   delta_E_at_gamma=GKKP%E_kpq(1)%E(ib,ik_bz,1) -GKKP%E_kpq(1)%E(ob,ik_bz,1)
 endif
 !
 eval_DW=(.not.abs(delta_E_at_gamma)<DW_deltaE_treshold)
 !
 gsqF_RES_DW=cZERO
 !
 if (eval_DW) then 
   !
   ! (b) Debye Waller Term
   !
   gsqF_RES_DW=-1._SP/2._SP*2._SP*GKKP%dVr(il,ib,ob,ik_bz,1)*q_weight/2._SP
   !                       /\
   !                        |
   !                        this 2 is to be consistent with AC definition (see Eq. 5, PRB 31, 2163)
   gsqF_DEN_DW=1._SP/delta_E_at_gamma/ph_E
   !
 endif
 !
 ! IF double-grid is present aveages all denominators on the q-points 
 ! of the fine grid around iq_bz
 !
 DW_weight=1._SP
 !
 if(use_PH_DbGd) then
   !      
   nq_around =q%FGbz%k_range(iq_bz,2)-q%FGbz%k_range(iq_bz,1)   ! Number of fine q-points around iq_bz
   weight_dbg=1._SP/(real(nq_around,SP)+1._SP)                             ! Total weight of the Db-grid points including the iq_bz
   do iq_fg=q%FGbz%k_range(iq_bz,1)+1,q%FGbz%k_range(iq_bz,2),1
     !
     iq_idx=iq_fg-q%FGbz%k_range(iq_bz,1)
     iE_fg =q%FGbz%E_map(iq_fg)                             ! Map w(q) from BZ to IBZ
     ph_E  =PH_freqs%FG%E(il,iE_fg,1)                       ! w(q_db) phonon energy in the D-Grid
     !
     E_kpq       =GKKP%E_dg_kpq(iq_bz)%E_kpq(ob,ik,1,iq_idx)    ! E(k+q_dg)  q_db in the D-Grid
     f_occ       =GKKP%E_dg_kpq(iq_bz)%f_kpq(ob,ik,1,iq_idx)/spin_occ
     !
     do iw=1,gsqF_energy_steps
       ! 
       delta_E     =Sc_W%p(iw)-E_kpq                                ! E(k) - E(k+q_dg)
       !
       gsqF_DEN_life_B(iw)=gsqF_DEN_life_B(iw)+     (Fermi_fnc_derivative(real(delta_E)+ph_E,gsqF_damping)&
 &                               +QP_time_order_sign*Fermi_fnc_derivative(real(delta_E)-ph_E,gsqF_damping))
       gsqF_DEN_life_f(iw)=gsqF_DEN_life_f(iw)+  ((Fermi_fnc_derivative(real(delta_E)+ph_E,gsqF_damping) - &
&                              QP_time_order_sign* Fermi_fnc_derivative(real(delta_E)-ph_E,gsqF_damping))*f_occ-&
&                                                  Fermi_fnc_derivative(real(delta_E)+ph_E,gsqF_damping))
       !
       one_minus_2f_occ=(1._SP-2._SP*f_occ)
       !
       if (.not.eval_G_using_KK) then 
         gsqF_DEN_ca_corr(iw)=gsqF_DEN_ca_corr(iw)+ph_E*one_minus_2f_occ/(delta_E**2-ph_E**2)
         gsqF_DEN_fan(iw)    =gsqF_DEN_fan(iw)+delta_E/(delta_E**2-ph_E**2)
       endif
       !
     enddo
     !
   enddo
   !
   if (eval_DW) then  
     !      
     ph_E=sqrt(abs(PH_freqs_sq(iq_ibz,il)))
     !
     do iq_idx=1,GKKP%E_dg_kpq(1)%nq_around  ! Numbere of q-points around Gamma
       !
       delta_E_at_gamma=en%E(ib,ik,1) - GKKP%E_dg_kpq(1)%E_kpq(ob,ik,1,iq_idx)
       !
       if(abs(delta_E_at_gamma)>DW_deltaE_treshold) then
         gsqF_DEN_DW=gsqF_DEN_DW+1._SP/delta_E_at_gamma/ph_E
         DW_weight=DW_weight+1._SP
       endif
       !
     enddo
     !
   endif
   !
 else
   weight_dbg=1._SP
 endif
 !
 ! Store results
 !
 gsqF_life_bose(i_qp,iq_mem,il,:)=gsqF_life_bose(i_qp,iq_mem,il,:)+gsqF_RES_life_B*gsqF_DEN_life_B(:)*weight_dbg
 gsqF_life_f(i_qp,iq_mem,il,:)   =gsqF_life_f(i_qp,iq_mem,il,:)   +gsqF_RES_life_f*gsqF_DEN_life_f(:)*weight_dbg
 !
 if (.not.eval_G_using_KK) then
    gsqF_fan(i_qp,iq_mem,il,:)    =gsqF_fan(i_qp,iq_mem,il,:) +gsqF_RES_fan*gsqF_DEN_fan(:)*weight_dbg
    gsqF_ca_corr(i_qp,iq_mem,il,:)=gsqF_ca_corr(i_qp,iq_mem,il,:) +gsqF_RES_ca_corr*gsqF_DEN_ca_corr(:)*weight_dbg
 endif
 !
 if (eval_DW)  gsqF_dw(i_qp,iq_mem,il)=gsqF_dw(i_qp,iq_mem,il)    +gsqF_RES_DW *gsqF_DEN_DW/DW_weight
 !
end subroutine
