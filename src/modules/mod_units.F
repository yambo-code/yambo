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
module UNITS
 !
 use pars,       ONLY:SP,pi
 !
 implicit none
 !
 ! [25/8/2011]
 !
 ! To organize better units and convertions the
 ! formato of the variables given here is
 !
 ! Format :  U = U2P * P
 ! 
 ! Note that complex units, like, W/cm^(-2) must be written as WATTCMm2 
 ! 
 ! For example:
 !  
 !   1 Hartree = 27.21 eV => AU2EV = 27.21 
 !   1 Bohr = 0.53 A      => AU2ARM= 0.53
 ! 
 ! PREFIXES
 !==========
 real(SP), parameter :: ATTO  =1E-18_SP
 real(SP), parameter :: FEMTO =1E-15_SP
 real(SP), parameter :: PICO  =1E-12_SP
 real(SP), parameter :: NANO  =1E-9_SP
 real(SP), parameter :: MICRO =1E-6_SP
 real(SP), parameter :: MILLI =1E-3_SP
 real(SP), parameter :: KILO  =1E3_SP
 real(SP), parameter :: MEGA  =1E6_SP
 real(SP), parameter :: GIGA  =1E9_SP
 real(SP), parameter :: TERA  =1E12_SP
 real(SP), parameter :: PETA  =1E12_SP
 !
 ! LENGTHS
 !=========
 !
 real(SP), parameter :: BO2ARM    =0.52917721_SP        ! BOHR = AU2ARM Armstrongs
 real(SP), parameter :: DEG2RAD   =pi/180.0_SP          ! DEGREE = DEG2RAD radiants
 !
 ! Energies
 !==========
 !
 real(SP), parameter :: HA2EV     =27.2113834_SP        ! HA  = HA2EV eV
 real(SP), parameter :: HA2KEL    =11604.5077_SP*HA2EV  ! HA  = HA2KEL K
 !
 ! Frequencies
 !=============
 !
 real(SP), parameter :: CORE_CNST =2.418884326505_SP
 real(SP), parameter :: HA2GHZ    =HA2EV*CORE_CNST*10._SP**5  ! HA = HA2GHZ GHz
 real(SP), parameter :: HA2THZ    =HA2GHZ/1000._SP            ! HA = HA2THZ THz
 !
 ! wavelength
 !============
 !
 real(SP), parameter :: HA2CMm1   =HA2EV*8065.5_SP            ! HA = HA2CMm1 cm^(-1)
 !
 ! Time
 !======
 !
 real(SP), parameter :: HA2PSm1   =HA2THZ                     ! HA = HA2PSm1 pico-SEC^(-1)
 real(SP), parameter :: HA2FSm1   =HA2THZ/1000._SP            ! HA = HA2FSm1 femto-SEC^(-1)
 !                                                              Note that 1 THz = 10^-3 / fs
 real(SP), parameter :: FS2AUT    =10.**2/CORE_CNST           ! femto-SEC = FS2AUT unit of Time in A.U. (2.4189 10^(-17) sec)
 real(SP), parameter :: PS2AUT    =10.**5/CORE_CNST           ! pico-SEC  = PS2AUT unit of Time in A.U.
 !
 ! LifeTimes
 !===========
 !
 ! To express the mean lifetime \tau (fs) in terms of its decay width \gamma (in eV) 
 ! one uses
 !
 !   \gamma=\hbar/\tau
 !
 ! and equivalently
 !
 !   \tau=\hbar/\gamma
 !
 real(SP), parameter :: HBAR_eVfs = 6.58211899_SP*10._SP**(-1._SP) ! eV*fs
 !
 ! Magnetic fields
 !=================
 !
 real(SP), parameter :: AU2Tesla      =1.7152551_SP*10._SP**3   ! (A.U. + C.G.S.)
 real(SP), parameter :: Tesla2AU      =1./AU2Tesla
 real(SP), parameter :: Bohr_magneton =9.24700915*10._SP**(-24) ! Joule * SEC^{-1} (S.I.). The 
 !                                                                Bohr magneton is also the A.U. unit for the 
 !                                                                electron magnetic dipole moment.
 !
 ! Electric fields and current
 !=============================
 !
 ! NOTE that in the S.I. the unit for the electric field is V/mm. statV/cm is the unit
 ! of E fields in C.G.S (Gaussian units)
 !
 real(SP), parameter :: SVCMm12VMm1=29.98E3_SP                  !  from [statV/cm] to [V/mm]
 real(SP), parameter :: VMm12SVCMm1 =1./SVCMm12VMm1
 real(SP), parameter :: AU2nAmpere =6.62361782E6_SP             !  [nano-Ampere]
 ! 
 real(SP), parameter  :: WMm22kWCMm2      =1E-7 ! W/m^2 = WMm22kWCMm2 kW/cm^2
 real(SP), parameter  :: WMm22ERGCMm2SECm1=1E-3 ! W/m^2 = WMm22ERGCMm2SECm1 erg/cm^2/sec
 !
 real(SP), parameter  :: AU2J   =4.3597482E-18  ! Ha = AU2J Joule
 real(SP), parameter  :: J2AU   =1./AU2J        ! J  = J2AU Ha
 !
 real(SP), parameter  :: AU2M  =5.2917720859E-11 ! Bohr = AU2M m
 real(SP), parameter  :: M2AU  =1./AU2M          ! m    = M2AU Bohr
 !
 real(SP), parameter  :: AU2SEC =2.418884326505E-17  ! Tau = AU2SEC sec
 real(SP), parameter  :: SEC2AU =1./AU2SEC           ! sec = SEC2AU Tau
 !
 real(SP), parameter  :: AU2VMm1=5.14220632E11_SP    ! V/m = AU2VMm1 AU
 real(SP), parameter  :: VMm12AU=1./AU2VMm1          ! Au  = VMm12AU AU
 !
 real(SP), parameter  :: SPEED_OF_LIGHT_SI = 2.99792458E8 ! m/sec
 real(SP), parameter  :: SPEED_OF_LIGHT    = 137.03599911 ! a.u.
 !
 real(SP), parameter  :: FREE_SPACE_PERM   = 1E7/4./pi/SPEED_OF_LIGHT_SI**2 ! Farad=Coulomb/Volt 
 !
 real(SP), parameter  :: kWCMm22AU=1E7*J2AU/(M2AU**2 * SEC2AU)  ! kW/cm^2 = kWCMm22AU * AU
 real(SP), parameter  :: AU2KWCMm2=1./kWCMm22AU                 ! AU      = AU2KWCMm2 kW/cm^2
 !
end module 
