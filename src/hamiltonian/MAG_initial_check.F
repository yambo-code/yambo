!
! License-Identifier: GPL
!
! Copyright (C) 2015 The Yambo Team
!
! Authors (see AUTHORS file for details): DS
!
subroutine MAG_initial_check()
 !
 use pars,        ONLY:SP,pi
 use electrons,   ONLY:n_sp_pol,n_spin
 use D_lattice,   ONLY:a
 use SC,          ONLY:SC_potential
 use parser_m,    ONLY:parser
 use vec_operate, ONLY:v_norm
 use magnetic,    ONLY:MAG_radius,MAG_hamiltonian_type,MAG_landau,MAG_pauli, &
&                         MAG_gauge,MAG_theta,MAG_psi,phase_trick
 !
 implicit none
 !
 character(1)  :: sectioning
 !
 if (     trim(SC_potential)=='default') sectioning='*'
 if (.not.trim(SC_potential)=='default') sectioning='+'
 !
 call section(sectioning,'Magnetic field ('//trim(MAG_hamiltonian_type)//') Hamiltonian') 
 !
 ! Set up MAG_hamiltonian_type
 !============================
 MAG_landau=trim(MAG_hamiltonian_type)=='landau'.or.trim(MAG_hamiltonian_type)=='all'
 MAG_pauli =trim(MAG_hamiltonian_type)=='pauli' .or.trim(MAG_hamiltonian_type)=='all'
 !
 ! Checks related to Pauli magnetism
 !
 if(n_spin==1) then
   if(MAG_pauli) then
     MAG_pauli=.false.
     call warning('Pauli magnetism switched off (spin unpolarized system).')
   endif
   if(MAG_landau) MAG_hamiltonian_type='landau'
 endif
 if(MAG_pauli .and. MAG_theta/=0. .and. n_sp_pol==2) &
&    call warning('The B field is not along z => the spins are assumed to be aligned to B. Please check.')
 !
 ! Checks related to Landau Magnetism with finite radius:
 !   impose the symmetric gauge
 !   impose flux to follow the a(3,:) direction 
 !
 if(MAG_radius/=0) then
   if(.not.MAG_gauge=='SYMM') then
     MAG_gauge='SYMM'
     call warning('Finite radius B field in the asymm. gauge has not been implemented. Symm. gauge imposed')
   endif
   if(MAG_landau) then
     if(any(a(3,:2)>epsilon(1.))) then
       MAG_theta=atan(v_norm((/a(3,1),a(3,2),0._SP/))/a(3,3)) 
       if(a(3,1)/=0._SP)                    MAG_psi= atan( a(3,2) / a(3,1) )
       if(a(3,1)< 0._SP)                    MAG_psi= MAG_psi+pi
       if(a(3,1)==0._SP .and. a(3,2)>0._SP) MAG_psi= pi/2._SP
       if(a(3,1)==0._SP .and. a(3,2)<0._SP) MAG_psi=-pi/2._SP
     else
       MAG_theta=0._SP
       MAG_psi  =0._SP
     endif
   endif
 endif
 !
 call parser('PhaseTrick',phase_trick)
 !
 return
 !
end subroutine
