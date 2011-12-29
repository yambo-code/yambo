!
! Copyright (C) 2000-2012 A. Marini and the YAMBO team 
!              http://www.yambo-code.org
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
subroutine RT_ELPH_initialize(q)
 ! 
 use pars,           ONLY:SP,pi
 use drivers,        ONLY:l_ph_corr 
 use units,          ONLY:HA2EV
 use com,            ONLY:depth
 use ELPH,           ONLY:ph_modes,elph_nb,elph_nDBs,elph_nDBs_used,nq_memory,elph_branches,&
&                         elph_global_alloc,ph_freqs_sq
 use SC,             ONLY:SC_bands
 use real_time,      ONLY:q_weight
 use R_lattice,      ONLY:nqbz,q_pt,nqibz,RL_vol,q_pt,RIM_n_rand_pts,bz_samp
 use vec_operate,    ONLY:iku_v_norm
 !
 implicit none
 !
 type(bz_samp),intent(in) :: q
 !
 ! Work Space
 !
 integer             :: iq,il
 real(SP), parameter :: Q_critical=0.4,Q_border=0.9
 real(SP)            :: FIT_coefficients(6,12) ! 1-3 around Gamma, 4-6 near the BZ border
 real(SP)            :: Q_mod,VOL_sphere,RADIUS_sphere,BARE_integral
 !
 if (.not.l_ph_corr) return
 !
 FIT_coefficients(:,1) =(/1.71265,42.5313,-15.6902,39.6942,3.83583,-90.6768/)
 FIT_coefficients(:,2) =(/5.13637,75.8111,-112.057,40.0783,7.61683,-76.7238/)
 FIT_coefficients(:,3) =(/1.19975,109.794,37.974,65.1302,-73.2227,-199.2/)
 FIT_coefficients(:,4) =(/6.76183,75.0173,94.1604,65.6299,-81.8142,-220.957/)
 FIT_coefficients(:,5) =(/4.1194,153.752,98.1517,107.457,186.22,289.338/)
 FIT_coefficients(:,6) =(/10.5744,107.279,181.17,106.791,178.163,294.323/)
 FIT_coefficients(:,7) =(/89.8843,-8.55514,-5.74619,131.951,-39.605,-280.053/)
 FIT_coefficients(:,8) =(/94.4763,4.18746,-51.6884,132.234,-39.4876,-280.003/)
 FIT_coefficients(:,9) =(/166.756,0.0396949,-37.6847,146.437,-55.9147,-64.0451/)
 FIT_coefficients(:,10)=(/167.087,-0.560888,-37.9211,147.055,-54.6055,-63.6376/)
 FIT_coefficients(:,11)=(/163.521,68.3978,-62.9593,153.591,-29.9826,58.2015/)
 FIT_coefficients(:,12)=(/179.849,51.0177,-102.749,153.415,-34.5894,66.9633/)
 !
 ph_modes=1
 elph_nb=SC_bands(2)
 elph_nDBs=nqbz
 elph_nDBs_used=nqbz
 nq_memory=nqbz
 elph_branches=(/1,1/)
 !
 call elph_global_alloc(' ')
 !
! do iq=1,nqibz
!   !
!   Q_mod=iku_v_norm(q_pt(iq,:))
!   !
!   do il=1,ph_modes
!     if (Q_mod<Q_critical) then
!       ph_freqs_sq(iq,il)=FIT_coefficients(1,il)+FIT_coefficients(2,il)*Q_mod+FIT_coefficients(3,il)*Q_mod**2.
!     else
!       ph_freqs_sq(iq,il)=FIT_coefficients(4,il)+FIT_coefficients(5,il)*(Q_mod-Q_border)+&
!&                         FIT_coefficients(6,il)*(Q_mod-Q_border)**2.
!     endif
!     !
!     ph_freqs_sq(iq,il)=(ph_freqs_sq(iq,il)/HA2EV)**2.
!     !
!   enddo
!   !
!   !write (100,'(20f10.5)') Q_mod,sqrt(ph_freqs_sq(iq,:))*HA2EV
!   !
! enddo
 !
 ph_freqs_sq=(36/1000./HA2EV)**2.
 !
 ! Reset the sections depth in order to not pop-out the messages in rim_spherical in the wrong
 ! section level
 !
 depth=0
 !
 ! RIM
 !
 allocate(q_weight(nqbz))
 !
 call k_ibz2bz(q,'i',.TRUE.)  
 !
 RIM_n_rand_pts=100000
 !
 RADIUS_sphere=(3.*RL_vol/nqbz/4./pi)**(1./3.)
 VOL_sphere=4./3.*pi*RADIUS_sphere**3.
 !
 call rim_spherical(nqbz,q%ptbz,q_weight,RADIUS_sphere,.FALSE.)
 !
! do iq=1,nqbz
!   BARE_integral=1./float(nqbz)/iku_v_norm(q%ptbz(iq,:))**2.
!   write (*,*)  iku_v_norm(q%ptbz(iq,:)),q_weight(iq),BARE_integral
! enddo
 !
end subroutine RT_ELPH_initialize
