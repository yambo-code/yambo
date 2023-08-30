!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine cutoff_sphere()
 !
 use pars,          ONLY:SP,pi
 use wave_func,     ONLY:wf_ng
 use R_lattice,     ONLY:cyl_ph_radius,bare_qpg,nqibz
 !
 implicit none
 !
 ! Work Space
 !
 integer ::iq,ig
 real(SP)::radius,d_cos,pre_factor
 !
 do iq=1,nqibz
   do ig=1,wf_ng
     !
     radius=real(bare_qpg(iq,ig),SP)*cyl_ph_radius
     d_cos=cos(mod(radius,2*pi))
     !
     pre_factor=sqrt(1._SP -d_cos)
     if (abs(pre_factor)<1.E-5_SP) pre_factor=1.E-5_SP
     !
     bare_qpg(iq,ig)=bare_qpg(iq,ig)/pre_factor
     !
   enddo
 enddo
 bare_qpg(1,1)=sqrt(2.0_SP)/cyl_ph_radius
 !
 !
end subroutine
