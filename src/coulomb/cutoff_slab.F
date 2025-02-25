!
! License-Identifier: GPL
!
! Copyright (C) 2016 The Yambo Team
!
! Authors (see AUTHORS file for details): AMDV
!
 subroutine cutoff_slab(q)
 !
 use com,           ONLY:of_open_close
 use pars,          ONLY:SP,pi,schlen
 use wave_func,     ONLY:wf_ng
 use D_lattice,     ONLY:alat
 use R_lattice,     ONLY:bz_samp,g_vec,bare_qpg,&
&                        d3q_factor,RIM_qpg,q0_def_norm,idir
 implicit none
 !
 ! Work Space
 ! 
 type(bz_samp) :: q
 integer       :: iq,ig,n_idx,n_max
 real(SP)      :: slab_vz,slab_vplane,lcut,pre_factor,R_circle,Sup,factorial
 
 lcut=alat(idir(1))/2.0_SP
 !
 do ig=1,wf_ng
   do iq=1,q%nibz
     !
     ! kz
     !
     slab_vz=(q%pt(iq,idir(1))+g_vec(ig,idir(1)) )*2.*pi/alat(idir(1))
     !
     !kxy
     !
     slab_vplane=sqrt(((q%pt(iq,idir(2))+g_vec(ig,idir(2)))*2.*pi/alat(idir(2)))**2+&
&                     ((q%pt(iq,idir(3))+g_vec(ig,idir(3)))*2.*pi/alat(idir(3)))**2)
     !
     !Potential regularization
     !
     if(slab_vplane < q0_def_norm) slab_vplane = q0_def_norm
     !
     pre_factor=sqrt(1._SP-exp(-slab_vplane*lcut)*cos(slab_vz*lcut))
     !
     bare_qpg(iq,ig)=bare_qpg(iq,ig)/real(pre_factor,KIND=SP)
   enddo
 enddo
 !
 if (.not.allocated(RIM_qpg)) then
   !
   n_max = 15
   factorial = 1._SP
   pre_factor = 0._SP
   Sup = d3q_factor/((2._SP*pi)/alat(idir(1)))
   R_circle = sqrt(Sup/pi)
   do n_idx=1,n_max
     factorial = factorial*n_idx
     pre_factor = pre_factor+(-1._SP)**(n_idx+1)/n_idx/factorial*(R_circle*lcut)**n_idx
   enddo
   pre_factor = 2._SP*pi/Sup*pre_factor
   bare_qpg(1,1) = real(1._SP/sqrt(pre_factor),KIND=SP)
   !
 endif
 !
end subroutine
