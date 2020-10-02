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
subroutine ELPH_Q_weight_times_Bose(q)
 ! 
 use pars,           ONLY:SP,pi
 use electrons,      ONLY:spin_occ
 use vec_operate,    ONLY:v_norm
 use D_lattice,      ONLY:Bose_Temp
 use R_lattice,      ONLY:RL_vol,bz_samp
 use ELPH,           ONLY:ph_freqs_sq,elph_branches
 use functions,      ONLY:bose_f
 use real_time,      ONLY:q_weight_RT
 use R_lattice,      ONLY:RIM_n_rand_pts
 !
#include<memory.h>
 !
 type(bz_samp),intent(in) :: q
 !
 ! Work Space
 !
 integer             ::iq,il,i1
 real(SP)            ::RADIUS_sphere,q_weight_tmp(q%nbz,2),ph_mean_speed(3),ph_E,q_mod(q%nbz),q0_mod
 !
 ! q_weight_RT are written in section 2, thus here are not known
 ! since only section 1 of the REF DB is written
 !
 if (allocated(q_weight_RT)) return
 !
 ! RIM support
 !
 ! Here It would be possible to have one different weight for each elph_mode.
 ! However in practice I assume there are two possibilities:
 ! 1 - acustic phonons w_ph(i) ~ \alpha(i)*|q|
 ! 2 - optical phonons w_ph(i) ~ const(i)
 !
 YAMBO_ALLOC(q_weight_RT,(q%nbz,elph_branches(1):elph_branches(2),2))
 q0_mod=(3._SP*RL_vol/4._SP/4._SP/pi)**(1._SP/3._SP)
 i1=0
 ph_mean_speed=0._SP
 q_mod=0._SP
 q_weight_RT=0._SP
 !
 call k_ibz2bz(q,'c',.TRUE.)  
 !
 if (RIM_n_rand_pts>0) then
   !
   do iq=1,q%nbz
     !
     q_mod(iq)=v_norm(q%ptbz(iq,:))
     !
     if (q_mod(iq)>q0_mod.or.iq==1) cycle
     !
     i1=i1+1
     !
     do il=1,3
       ph_mean_speed(il)=ph_mean_speed(il)+sqrt(ph_freqs_sq(iq,il))/q_mod(iq)
     enddo
     !
   enddo
   !
   ph_mean_speed=ph_mean_speed/float(i1)
   !
 endif
 !
 call k_ibz2bz(q,'i',.TRUE.)  
 !
 RADIUS_sphere=(3._SP*RL_vol/q%nbz/4._SP/pi)**(1._SP/3._SP)
 !
 call rim_spherical(q%nbz,q%ptbz,q_weight_tmp(:,1),RADIUS_sphere,1,.FALSE.)
 call rim_spherical(q%nbz,q%ptbz,q_weight_tmp(:,2),RADIUS_sphere,2,.FALSE.)
 !
 do iq=1,q%nbz
   !
   do il=elph_branches(1),elph_branches(2)
     !
     ph_E=sqrt(abs(ph_freqs_sq(iq,il)))
     !
     if (ph_E>0.) then
       q_weight_RT(iq,il,1)=1._SP/ph_E/float(q%nbz)
       q_weight_RT(iq,il,2)=bose_f(ph_E)/ph_E/float(q%nbz)/spin_occ
     endif
     !
     if (RIM_n_rand_pts>0.and.q_mod(iq)<=q0_mod .and. il<=3) then
       q_weight_RT(iq,il,1)=q_weight_tmp(iq,1)/ph_mean_speed(il)
       q_weight_RT(iq,il,2)=Bose_Temp*q_weight_tmp(iq,2)/ph_mean_speed(il)**2/spin_occ
     endif
     !
   enddo
   !
 enddo
 !
end subroutine ELPH_Q_weight_times_Bose
