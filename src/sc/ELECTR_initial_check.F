!
! License-Identifier: GPL
!
! Copyright (C) 2016 The Yambo Team
!
! Authors (see AUTHORS file for details): AC MG
!
subroutine ELECTR_initial_check(k,Dip)
 !
 use pars,        ONLY:SP
 use units,       ONLY:AU2VMm1
 use com,         ONLY:msg
 use DIPOLES,     ONLY:DIPOLE_t
 use vec_operate, ONLY:normalize_v
 use SC,          ONLY:SC_potential
 use fields,      ONLY:Efield,Efield_strength
 use R_lattice,   ONLY:k_map,bz_samp
 !
 implicit none
 !
 type(bz_samp) ::k
 type(DIPOLE_t)::Dip
 !
 ! Workspace
 !
 character(1)  :: sectioning
 !
 if(Efield(1)%intensity==0._SP) then
   call warning("Zero electric field. Turned off")
   Efield(1)%ef_name="NONE"
   return
 endif
 !
 if (     trim(SC_potential)=='default') sectioning='*'
 if (.not.trim(SC_potential)=='default') sectioning='+'
 !
 call section(sectioning,'Electric field Hamiltonian') 
 !
 ! Set up ELECTRIC_hamiltonian_type
 !============================
 !
 Efield(1)%ef_name="STATIC"
 !
 if(all(abs(Efield(1)%versor(:))<epsilon(1.))) &
&    call error(" External static field has undefined direction.")
 !
 Efield(1)%versor        =normalize_v(Efield(1)%versor)
 Efield(1)%amplitude     =Efield_strength(Efield(1)%intensity,'AU')
 Efield(1)%t_initial_indx=1
 Efield(1)%t_initial     =0._SP
 Efield(1)%ef_pol        ="linear"
 !
 call Check_symmetries(Efield(1)%versor,"electric")
 !
 Dip%approach='Covariant'
 !
 call k_map_nearest_by_dir(k,k_map) 
 !
 ! LOG
 ! ===
 !
 call msg( 'sr', 'Static Electric Field ')
 call msg( 'sr', 'Efield                     '//trim(Efield(1)%ef_name))
 call msg( 'sr', 'Efield Amplitude           ',Efield(1)%amplitude*AU2VMm1/100._SP/1000._SP,"[kV/cm]")
 !
end subroutine
