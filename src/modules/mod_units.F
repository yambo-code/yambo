!
! Copyright (C) 2000-2009 A. Marini and the YAMBO team 
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
module UNITS
 !
 use pars,       ONLY:SP,pi
 !
 implicit none
 !
 ! UNITs CONVERSION
 !
 real(SP), parameter :: BOHR=0.52917721_SP            ! Armstrongs
 real(SP), parameter :: radian=pi/180.0_SP  
 !
 ! Energies
 !
 real(SP), parameter :: HARTREE=27.2113834_SP         ! EV2HA, 1 HA = HA2EV eV
 real(SP), parameter :: Kelvin=11604.5077_SP*HARTREE  ! EV2K,  1 K = KE2EV eV
 !
 ! Format :  U2 = U1_2_U2 * U1
 ! 
 ! remember: G=10^9 T=10^12
 !
 ! Frequencies
 !
 !
 real(SP), parameter :: HA2GHZ=HARTREE*241798.474_SP
 real(SP), parameter :: HA2THZ=HA2GHZ/1000._SP
 !
 ! wavelength
 !
 real(SP), parameter :: HA2CM1=HARTREE*8065.5_SP
 !
 ! Time
 !
 real(SP), parameter :: HA2PSm1=HA2THZ
 real(SP), parameter :: HA2FSm1=HA2THZ/1000._SP     ! Note that 1 THz = 10^-3 / fs
 real(SP), parameter :: AUT2SEC=2.418884326505_SP*10._SP**(-17)
 real(SP), parameter :: SEC2AUT=1._SP/AUT2SEC
 real(SP), parameter :: FS2AUT =10.**2/2.418884326505_SP
 real(SP), parameter :: PS2AUT =10.**5/2.418884326505_SP
 !
 ! Magnetic fields
 !
 real(SP), parameter :: AU2Tesla=2.350517*10.**5
 real(SP), parameter :: Tesla2AU=1./AU2Tesla
 !
 ! From Octopus ( http://www.tddft.org/programs/octopus )
 !
 ! It is very common to describe the strength of a laser field by it intensity, 
 ! rather than using the electric field amplitude. In atomic units, the relationship 
 ! between instantaneous electric field and intensity is:
 !
 ! I(t) = \frac{c}{8\pi} E^2(t)
 !
 ! It is common to read intensities in kW cm^-2. The dimensions of intensities are [W]/(L^2T), 
 ! where [W] are the dimensions of energy. The relevant conversion factors are:
 !
 ! Hartree / (a_0^2 atomic_time) = 6.4364086e+09 kW cm^-2
 !
 ! If, in atomic units, we set the electric field amplitude to E_0, then the intensity is:
 !
 ! I_0 = \frac{c}{8 \pi}  1./W2Au kW cm^-2 (E_0^2)
 !
 real(SP), parameter :: SPEED_OF_LIGHT = 137.03599911
 !
 real(SP), parameter :: W2Au = 1./6.4364086E+09 
 !
end module 
