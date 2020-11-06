!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM
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
subroutine ELPH_acoustic_phonon_properties(q)
 !
 ! The phonon branches are splitted in two: acoustic and optical
 !
 ! Acoustic branch is defined in such a way that 
 !
 ! w_ph(i) -> \alpha(i)*|q| for |q| -> 0
 ! 
 use units,          ONLY:HA2EV
 use pars,           ONLY:schlen,rZERO,SP
 use vec_operate,    ONLY:v_norm
 use R_lattice,      ONLY:bz_samp
 use ELPH,           ONLY:PH_freqs_sq,ph_modes,PH_acoustic_branch,PH_acoustic_speed,PH_Q_modulus
 use stderr,         ONLY:intc
 use com,            ONLY:msg
 !
 implicit none
 !
 type(bz_samp),intent(in) :: q
 !
 ! Work Space
 !
 real(SP)            ::GAMMA_E_ph(ph_modes),delta_E
 integer             ::iq,il
 character(schlen)   ::aco_string
 !
 ! Acoustic Branches
 !-------------------
 aco_string=" "
 GAMMA_E_ph=abs( sqrt(PH_freqs_sq(1,:)) )
 do il=1,ph_modes
   delta_E=GAMMA_E_ph(il)-minval(GAMMA_E_ph)
   if (delta_E<0.01/HA2EV) then
     if (.not.any(PH_acoustic_branch)) aco_string=trim(intc(il))
     if (     any(PH_acoustic_branch)) aco_string=trim(aco_string)//" - "//trim(intc(il))
     PH_acoustic_branch(il)=.TRUE.
     !DEBUG_AM>
     !PH_freqs_sq(1,il)=rZERO
     !DEBUG_AM<
   endif
 enddo
 !
 !DEBUG_AM>
 PH_acoustic_branch     =.FALSE.
 PH_acoustic_branch(1:3)=.TRUE.
 !DEBUG_AM<
 !
 ! PH speeds
 !-----------
 call k_ibz2bz(q,'c',.TRUE.)  
 !
 PH_acoustic_speed=rZERO
 do iq=2,q%nbz
   do il=1,ph_modes
     PH_Q_modulus(iq)=v_norm(q%ptbz(iq,:))
     if (.not.PH_acoustic_branch(il)) cycle
     PH_acoustic_speed(iq,il)=sqrt(abs(PH_freqs_sq(iq,il)))/PH_Q_modulus(iq)
   enddo
 enddo 
 !
 call msg("r","Phonon acoustic branches",trim(aco_string))
 !
 call k_ibz2bz(q,'i',.TRUE.)  
 !
end subroutine ELPH_acoustic_phonon_properties
