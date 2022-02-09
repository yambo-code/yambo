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
!> @brief Calculate X(w) including ph-assisted transitions
!!
subroutine EXCPH_X_phass(wv,Epsilon_ii,PL_ii,q)
 !
 use frequency,     ONLY:w_samp
 use pars,          ONLY:SP,cZERO
 use units,         ONLY:HA2EV
 use functions,     ONLY:Lorentzian_func
 use IO_m,          ONLY:NONE,OP_APP_CL,OP_RD_CL,DUMP,OP_RD,RD_CL_IF_END,REP,VERIFY
 use IO_int,        ONLY:io_control
 use EXCPH,         ONLY:EXCPH_states,EXCPH_sum,BS_Sat_E,BS_Sat_WEIGHT,EXCPH_Renorm,BS_all_E,&
&                        BS_Sat_E_PH_abs,BS_Sat_WEIGHT_PH_abs
 use ELPH,          ONLY:elph_branches,PH_freqs_sq,FAN_deltaE_treshold
 use com,           ONLY:msg
 use R_lattice,     ONLY:bz_samp
 use functions,     ONLY:boltzman_f,bose_f
 !
#include<memory.h>
 !
 type(w_samp),  intent(in)  :: wv
 type(bz_samp), intent(in)  :: q
 complex(SP),   intent(out) :: Epsilon_ii(wv%n_freqs,1),PL_ii(wv%n_freqs,2)
 !
 ! Work space
 !
 complex(SP) :: pole,W,Satells_alpha,Satells_alpha_PL
 complex(SP), external :: Lorentzian_FT
 real(SP) :: min_E,exc_E
 integer :: i_alpha,i_beta,il,iq,iw,io_err,ID_exc_gkkp,ID_gkkp
 integer, external :: io_EXCPH_gkkp,io_ELPH
 real(SP) :: bose_factor,PH_E
 !
 min_E=minval(BS_all_E(1,:))
 call msg('rs','Minimum excitonic energy : ',min_E*HA2EV,' eV ')
 !
 ! Load the Exciton-phonon elements and phonon energies
 !
 call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1/),MODE=DUMP,ID=ID_exc_gkkp)
 io_err=io_EXCPH_gkkp(ID_exc_gkkp,'satellites')
 !
 call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1/),MODE=DUMP,ID=ID_gkkp)
 io_err=io_ELPH(ID_gkkp,'gkkp_expanded no_matrix_elements')
 !
 Epsilon_ii=cZERO
 PL_ii     =cZERO
 !
 ! The term without satellites
 !
 do iw=1,wv%n_freqs
   !
   W=wv%p(iw)
   !
   do i_alpha=EXCPH_states(1),EXCPH_states(2)
     !
     exc_E=BS_all_E(i_alpha,1)-min_E
     pole =BS_all_E(i_alpha,1)
     Epsilon_ii(iw,1)=Epsilon_ii(iw,1) + (1._SP-EXCPH_Renorm(i_alpha))*Lorentzian_FT(W,pole,'r')
     PL_ii(iw,1)     =PL_ii(iw,1)      + (1._SP-EXCPH_Renorm(i_alpha))*Lorentzian_FT(W,pole,'r')*boltzman_f(exc_E)
     !
   enddo
   !
 enddo
 !
 ! The satellites
 !
 do iq=1,q%nibz
   !
   call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1+iq/),MODE=DUMP,ID=ID_exc_gkkp)
   io_err=io_EXCPH_gkkp(ID_exc_gkkp,'satellites')
   !
   call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1+iq/),MODE=DUMP,ID=ID_gkkp)
   io_err=io_ELPH(ID_gkkp,'gkkp_expanded no_matrix_elements')
   !
   do iw=1,wv%n_freqs
     !
     W=wv%p(iw)
     !
     do i_alpha=EXCPH_states(1),EXCPH_states(2)
       !
       ! Satellites for the alpha exciton
       !
       Satells_alpha   =cZERO
       Satells_alpha_PL=cZERO
       !
       do il=elph_branches(1),elph_branches(2)
         !
         ph_E=sqrt(PH_freqs_sq(iq,il))
         !
         if(ph_E<FAN_deltaE_treshold) cycle
         !
         ! Bose function for the phonons
         !
         bose_factor=bose_f(PH_E)
         !
         do i_beta=EXCPH_sum(1),EXCPH_sum(2)
           !
           exc_E=BS_all_E(i_beta,1)-min_E
           !
           ! Phonon emission
           !
           pole=BS_Sat_E(il,i_beta,i_alpha)
           Satells_alpha   =Satells_alpha+BS_Sat_WEIGHT(il,i_beta,i_alpha)*Lorentzian_FT(W,pole,'r')*(1._SP+bose_factor)
           Satells_alpha_PL=Satells_alpha+BS_Sat_WEIGHT(il,i_beta,i_alpha) &
&                   *Lorentzian_FT(W,pole,'r')*bose_factor*boltzman_f(exc_E)
           !
           ! Phonon absorption
           !
           pole=BS_Sat_E_PH_abs(il,i_beta,i_alpha)
           Satells_alpha   =Satells_alpha   +BS_Sat_WEIGHT_PH_abs(il,i_beta,i_alpha)*Lorentzian_FT(W,pole,'r')*bose_factor 
           Satells_alpha_PL=Satells_alpha_PL+BS_Sat_WEIGHT_PH_abs(il,i_beta,i_alpha) &
&                   *Lorentzian_FT(W,pole,'r')*(1._SP+bose_factor)*boltzman_f(exc_E) 
           !
         enddo
         !
       enddo
       !
       ! Add the satellites to the dielectric constant and luminescence
       !
       Epsilon_ii(iw,1)=Epsilon_ii(iw,1)+Satells_alpha
       PL_ii(iw,1)=PL_ii(iw,1)          +Satells_alpha_PL
       !
     enddo   
     !
   enddo   
   !
 enddo   
 !
end subroutine EXCPH_X_phass
