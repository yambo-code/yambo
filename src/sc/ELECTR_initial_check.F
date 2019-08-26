!
!        Copyright (C) 2000-2019 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AC MG
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
subroutine ELECTR_initial_check(k)
 !
 use pars,        ONLY:SP
 use units,       ONLY:AU2VMm1
 use com,         ONLY:error,msg,warning
 use X_m,         ONLY:Dipole_approach
 use vec_operate, ONLY:normalize_v
 use SC,          ONLY:SC_potential
 use fields,      ONLY:Efield,Efield_strength
 use R_lattice,   ONLY:k_map,bz_samp
 !
 implicit none
 !
 type(bz_samp) ::k
 !
 ! Workspace
 !
 character(1)  :: sectioning
 !
 if(Efield(1)%intensity==0._SP) then
   call warning("Zero electric field. Turned off")
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
 call Check_symmetries(Efield(1)%versor)
 !
 Dipole_approach='Covariant'
 !
 call k_map_nearest_by_dir(k,k_map) 
 !
 ! LOG
 ! ===
 !
 call msg( 'sr', 'Static Electric Field ')
 call msg( 'sr', 'Efield                     :'//trim(Efield(1)%ef_name))
 call msg( 'sr', 'Efield Amplitude    [kV/cm]:',Efield(1)%amplitude*AU2VMm1/100._SP/1000._SP)
 !
end subroutine
