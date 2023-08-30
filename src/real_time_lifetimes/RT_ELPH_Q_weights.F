!
! License-Identifier: GPL
!
! Copyright (C) 2020 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
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
     if (ph_E>0._SP) q_weight_RT(iq,il,:)=1._SP/ph_E/float(q%nbz)
     !
     if (PH_acoustic_branch(il).and.iq>1) q_weight_RT(iq,il,2)=1._SP/ph_E/float(q%nbz)/PH_Q_modulus(iq)
     !
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
