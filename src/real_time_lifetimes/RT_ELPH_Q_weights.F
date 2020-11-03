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
subroutine RT_ELPH_Q_weights(q)
 ! 
 use pars,           ONLY:SP,pi,rZERO
 use R_lattice,      ONLY:RL_vol,bz_samp
 use ELPH,           ONLY:PH_freqs_sq,elph_branches,PH_acoustic_speed,PH_acoustic_branch,PH_Q_modulus
 use RT_lifetimes,   ONLY:q_weight_RT,l_RT_skip_ph_abs_lifetimes
 use R_lattice,      ONLY:RIM_n_rand_pts
 !
#include<memory.h>
 !
 type(bz_samp),intent(in) :: q
 !
 ! Work Space
 !
 integer             ::iq,il
 real(SP)            ::RADIUS_sphere,RIM(q%nbz,2),ph_E
 !
 ! q_weight_RT are written in section 2, thus here are not known
 ! since only section 1 of the REF DB is written
 !
 if (allocated(q_weight_RT)) return
 !
 YAMBO_ALLOC(q_weight_RT,(q%nbz,elph_branches(1):elph_branches(2),2))
 !
 if (RIM_n_rand_pts>0) then
   RADIUS_sphere=(3._SP*RL_vol/q%nbz/4._SP/pi)**(1._SP/3._SP)
   call rim_spherical(q%nbz,q%ptbz,RIM(:,1),RADIUS_sphere,1,.FALSE.)
   call rim_spherical(q%nbz,q%ptbz,RIM(:,2),RADIUS_sphere,2,.FALSE.)
 endif
 !
 do iq=1,q%nbz
   !
   do il=elph_branches(1),elph_branches(2)
     !
     q_weight_RT(iq,il,:)=rZERO
     !
     ph_E=sqrt(abs(PH_freqs_sq(iq,il)))
     !
     if (PH_acoustic_branch(il).and.iq>1) then
       q_weight_RT(iq,il,1)=1._SP/ph_E/float(q%nbz)
       q_weight_RT(iq,il,2)=1._SP/ph_E/float(q%nbz)/PH_Q_modulus(iq)
     endif
     if (.not.PH_acoustic_branch(il)) then
       q_weight_RT(iq,il,:)=1._SP/ph_E/float(q%nbz)
     endif
     if (RIM_n_rand_pts>0.and.PH_acoustic_branch(il)) then
       q_weight_RT(iq,il,:)=RIM(iq,:)/PH_acoustic_speed(iq,il)
     endif
     !
   enddo
   !
 enddo
 !
 if (l_RT_skip_ph_abs_lifetimes) q_weight_RT(:,:,2)=rZERO
 !
end subroutine RT_ELPH_Q_weights
