!
! License-Identifier: GPL
!
! Copyright (C) 2020 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine ELPH_acoustic_phonon_properties(q)
 !
 ! The phonon branches are splitted in two: acoustic and optical
 !
 ! Acoustic branch is defined in such a way that 
 !
 ! w_ph(i) -> \alpha(i)*|q| for |q| -> 0
 ! 
 use units,          ONLY:HA2EV,HA2THZ
 use pars,           ONLY:schlen,rZERO,SP
 use vec_operate,    ONLY:v_norm
 use R_lattice,      ONLY:bz_samp
 use ELPH,           ONLY:PH_freqs_sq,ph_modes,PH_acoustic_branch,PH_acoustic_speed,PH_Q_modulus,&
&                         elph_nQ,l_GKKP_expanded_DB_exists,l_GKKP_DB_exists,PH_W_debye
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
   endif
 enddo
 !
 ! PH speeds
 !-----------
 call k_ibz2bz(q,'c',.TRUE.)  
 !
 PH_acoustic_speed=rZERO
 do iq=2,elph_nQ
   do il=1,ph_modes
     if (l_GKKP_expanded_DB_exists) PH_Q_modulus(iq)=v_norm(q%ptbz(iq,:))
     if (l_GKKP_DB_exists         ) PH_Q_modulus(iq)=v_norm(q%ptbz(iq,:))
     if (.not.PH_acoustic_branch(il)) cycle
     PH_acoustic_speed(iq,il)=sqrt(abs(PH_freqs_sq(iq,il)))/PH_Q_modulus(iq)
   enddo
 enddo 
 !
 call msg("nr","[Ph]Phonon acoustic branches",trim(aco_string))
 call msg('rn','[Ph] Debye energy',(/PH_W_debye*HA2EV,PH_W_debye*HA2THZ/),"[ev/ThZ]")
 !
 call k_ibz2bz(q,'i',.TRUE.)  
 !
end subroutine ELPH_acoustic_phonon_properties
