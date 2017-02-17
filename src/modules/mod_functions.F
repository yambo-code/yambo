!
! Copyright (C) 2000-2010 A. Marini and the YAMBO team 
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
module functions
 !
 use pars, ONLY: SP
 implicit none
 !
 real(SP)     :: bose_E_cut
 !
 contains
   !
   ! Fermi functions
   !-----------------
   !
   function Fermi_fnc(E,T)
     !
     use electrons, ONLY:filled_tresh
     real(SP)::E,T,Fermi_fnc
     Fermi_fnc=1.
     if (T==0..and.E>0.) Fermi_fnc=0.
     !
     if (T/=0.) Fermi_fnc=1./(1.+exp(E/T))
     !
     if (Fermi_fnc<filled_tresh)  Fermi_fnc=0.
     !
   end function
   !
   function Fermi_fnc_derivative(E,T)
     !
     ! This is the energy derivative of the Fermi-Dirac function.
     ! It can be considered and approximation to the delta function.
     !
     ! Note that the function defined here is integrated to 1.
     !
     real(SP) :: E,T,Fermi_fnc_derivative
     real(SP),parameter :: tresh=0.
     Fermi_fnc_derivative=0. 
     if (T/=0.) Fermi_fnc_derivative=1./(2.+exp(E/T)+exp(-E/T))/T
     if (Fermi_fnc_derivative<tresh)  Fermi_fnc_derivative=0.
   end function
   !
   ! Bose functions
   !----------------
   !
   pure function bose_f(Eb)
     !
     use drivers,   ONLY:Finite_Tel
     use D_lattice, ONLY:Bose_Temp
     real(SP), intent(in):: Eb
     real(SP)            :: bose_f
     bose_f=0.
     if (Eb<0.) bose_f=-2.
     if (.not.Finite_Tel) return
     !
     !
   end function
   !
   pure function bose_decay(E)
     !
     use drivers,   ONLY:Finite_Tel
     !
     use D_lattice, ONLY:Tel
     real(SP), intent(in):: E
     real(SP)            :: bose_decay
     bose_decay=1.
     !
     !
   end function
   !
   !
   logical function K_scatter(E,Ep,Er,Dr)
     !
     real(SP)::E,Ep,Er(2),Dr(2),D
     K_scatter=.false.
     D=Dr(1)+(E-Er(1))/(Er(2)-Er(1))*(Dr(2)-Dr(1))
     D=max(D,0.)
     K_scatter=abs(E-Ep)<=D
   end function
   !
   integer function BZ_index(ik,k)
     !
     ! Returns the BZ index of a given ik in the IBZ
     !
     use R_lattice,      ONLY:bz_samp
     type(bz_samp) :: k
     integer       :: ik
     if (ik==1) BZ_index=1
     if (ik>1 ) BZ_index=1+sum(k%nstar(:ik-1))
   end function
   !
end module
