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
module functions
 !
 use pars, ONLY: SP
 implicit none
 !
 real(SP)     :: bose_E_cut
 !
 interface
   !
   integer function Fermi_fnc_fit(RT_occ_in,nstates,T_fit,Ef_fit,err_fit,RT_occ_fit_out,RT_E_fit_out)
     !
     use pars, ONLY: SP
     !
     ! This function makes a fit of RT_occ_in to the Fermi-Dirac function using two
     ! temperature and two fermi level, one for the valence and one for the conduction
     !
     integer,  intent(in)  :: nstates
     real(SP), intent(in)  :: RT_occ_in(nstates)
     !
     real(SP), intent(out) :: T_fit(2)
     real(SP), intent(out) :: Ef_fit(2)
     real(SP), intent(out) :: err_fit(4)
     real(SP), intent(out), optional :: RT_occ_fit_out(1000,2)
     real(SP), intent(out), optional :: RT_E_fit_out(1000,2)
     !
   end function Fermi_fnc_fit
   !
 end interface
 !
 contains
   !
   logical function infinity(E)
     use pars, ONLY: infinity_dfl
     real(SP)     :: E
     infinity= E > infinity_dfl
   end function infinity
   !
   logical function NAN(E)
#if defined(_XLF) || defined(_PGI)
     use ieee_arithmetic
     real(SP) :: E
     NAN= ieee_is_nan(E)
#else
     real(SP) :: E
     NAN= isnan(E)
#endif
   end function NAN
   !
   real(SP) function theta_function(T,step,order)
     !
     ! This is an easy numerical implementation of the
     ! theta function and its derivatives (the delta and the sign).
     !
     integer :: order
     real(SP):: T,step
     character(4) :: step_case
     !
     theta_function=0._SP
     if (abs(T/step)>1.05_SP) then
       if(order==0.and.T>0._SP) theta_function= 1._SP
       return
     endif
     !
     if( abs(T/step+1.0_SP)<0.1 ) step_case="-1.0"
     if( abs(T/step+0.5_SP)<0.1 ) step_case="-0.5"
     if( abs(T/step+0.0_SP)<0.1 ) step_case=" 0.0"
     if( abs(T/step-0.5_SP)<0.1 ) step_case="+0.5"
     if( abs(T/step-1.0_SP)<0.1 ) step_case="+1.0"
     !
     select case(step_case)
     case("-1.0")
       if (order==0) theta_function= 0.00_SP
       if (order==1) theta_function= 0.00_SP
       if (order==2) theta_function= 1.00_SP/step**2
     case("-0.5")
       if (order==0) theta_function= 0.25_SP
       if (order==1) theta_function= 0.50_SP/step
       if (order==2) theta_function= 1.00_SP/step**2
     case(" 0.0")
       if (order==0) theta_function= 0.5_SP
       if (order==1) theta_function= 1.0_SP/step
       if (order==2) theta_function= 0.0_SP
     case("+0.5")
       if (order==0) theta_function= 0.75_SP
       if (order==1) theta_function= 0.50_SP/step
       if (order==2) theta_function=-1.00_SP/step**2
     case("+1.0")
       if (order==0) theta_function= 1.00_SP
       if (order==1) theta_function= 0.00_SP
       if (order==2) theta_function=-1.00_SP/step**2
     end select
     !
   end function
   !
   ! Fermi functions
   !-----------------
   !
   function Fermi_fnc(E,T)
     !
     use electrons, ONLY:filled_tresh
     real(SP)::E,T,Fermi_fnc
     Fermi_fnc=1._SP
     if (T==0._SP.and.E>0._SP) Fermi_fnc=0._SP
     !
     if (T/=0._SP) Fermi_fnc=1._SP/(1._SP+exp(E/T))
     !
     if (Fermi_fnc<filled_tresh)  Fermi_fnc=0._SP
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
     real(SP),parameter :: tresh=0._SP
     Fermi_fnc_derivative=0._SP
     if (T/=0._SP) Fermi_fnc_derivative=1._SP/(2._SP+exp(E/T)+exp(-E/T))/T
     if (Fermi_fnc_derivative<tresh)  Fermi_fnc_derivative=0._SP
   end function
   !
   function Fermi_fnc_T_derivative(E,T)
     !
     ! This is the Temperature derivative of the Fermi-Dirac function.
     !
     real(SP) :: E,T,Fermi_fnc_T_derivative
     real(SP),parameter :: tresh=0._SP
     Fermi_fnc_T_derivative=0._SP
     if (T/=0._SP) Fermi_fnc_T_derivative=E/(2._SP+exp(E/T)+exp(-E/T))/T**2
   end function
   !
   ! Bose functions
   !----------------
   !
   pure function bose_f(Eb)
     !
     use drivers,   ONLY:Finite_Tel
     use D_lattice, ONLY:Bose_Temp
     use electrons, ONLY:spin_occ
     real(SP), intent(in):: Eb
     real(SP)            :: bose_f
     bose_f=0.
     if (Eb<0.) bose_f=-spin_occ
     if (.not.Finite_Tel) return
     !
     if (abs(Eb)>epsilon(1.)) then
       if (abs(Eb)<=bose_E_cut*Bose_Temp) bose_f=spin_occ*Bose_Temp/Eb
       if (abs(Eb)> bose_E_cut*Bose_Temp) bose_f=spin_occ/(exp(Eb/Bose_Temp)-1.)
     else
       bose_f=spin_occ*Bose_Temp/epsilon(1.)
     endif
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
     if (.not.Finite_Tel) return
     if (abs(E)<=bose_E_cut*Tel) bose_decay=E**2./(Tel*bose_E_cut)**2.
     !
   end function
   !
   logical function K_scatter(E,Ep,Er,Dr)
     !
     real(SP)::E,Ep,Er(2),Dr(2),D
     K_scatter=.false.
     D=Dr(1)+(E-Er(1))/(Er(2)-Er(1))*(Dr(2)-Dr(1))
     D=max(D,0._SP)
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
