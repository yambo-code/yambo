!
! Copyright (C) 2000-2005 A. Marini and the SELF team 
!         http://www.fisica.uniroma2.it/~self
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
!================
   use pars
   real(SP) :: bose_E_cut
 contains

 pure function bose_f(Eb)
!========================
 use drivers,   ONLY:Finite_Tel
 use D_lattice, ONLY:Tel
 implicit none
 real(SP), intent(in):: Eb
 real(SP)            :: bose_f
 bose_f=0.
 if (Eb<0.) bose_f=-2.
 if (.not.Finite_Tel) return

! if (abs(Eb/Tel)<bose_E_cut)  bose_f=2./(exp(bose_E_cut)-1.)

 if (abs(Eb)>epsilon(1.)) then
   if (abs(Eb)<=bose_E_cut*Tel) bose_f=2.*Tel/Eb
   if (abs(Eb)> bose_E_cut*Tel) bose_f=2./(exp(Eb/Tel)-1.)
 else
   bose_f=2.*Tel/epsilon(1.)
 endif
 end function

 pure function bose_decay(E)
!===========================
 use drivers,   ONLY:Finite_Tel
 use D_lattice, ONLY:Tel
 implicit none
 real(SP), intent(in):: E
 real(SP)            :: bose_decay
 bose_decay=1.
 if (.not.Finite_Tel) return
 if (abs(E)<=bose_E_cut*Tel) bose_decay=E**2./(Tel*bose_E_cut)**2.
 end function
!
!=================================================================
!
! Lifetime functions e2et/h2ht
!
! Gamma_n = 2 i \sum_m  { -/+ i Im[e^-1(e_n -e_m) (2-f+bose_f) <- e2et
!                           + i Im[e^-1(e_m -e_n) (f+bose_f)   <- h2ht }
!
! where - for T-ordered theory, + for causal (finite Tel)
!
 function e2et(is,os,E,F)
!========================
 use electrons,   ONLY:levels
 use drivers,     ONLY:Finite_Tel
 implicit none
 integer      ::is(2),os(2),e2et
 type(levels) ::E
 real(SP) :: F
 real(SP) :: dE !ws
 e2et=0
!
!"Electron 2 Electron" decay
!
 dE=E%E(is(1),is(2),1)-E%E(os(1),os(2),1)
!
 F=               -(2.-E%f(os(1),os(2),1)+bose_f(dE))
 if (Finite_Tel) F=(2.-E%f(os(1),os(2),1)+bose_f(dE))
!
 if (dE>0..and.abs(F)>epsilon(1.)) e2et=1
 if (e2et==0) F=0.
 end function

 function h2ht(is,os,E,F)
!========================
 use electrons, ONLY:levels
 use drivers,   ONLY:Finite_Tel
 implicit none
 integer      ::is(2),os(2),h2ht
 type(levels) ::E
 real(SP) :: F
!ws
 real(SP) :: dE 
 h2ht=0
!
!"Hole 2 Hole" decay
!
 dE=E%E(os(1),os(2),1)-E%E(is(1),is(2),1)
!
 F=E%f(os(1),os(2),1)+bose_f(dE)
!
 if (dE>0..and.abs(F)>epsilon(1.)) h2ht=1
 if (h2ht==0) F=0.
 end function

 logical function K_scatter(E,Ep,Er,Dr)
!======================================
 use pars
 implicit none
 real(SP)::E,Ep,Er(2),Dr(2),D
 K_scatter=.false.
 D=Dr(1)+(E-Er(1))/(Er(2)-Er(1))*(Dr(2)-Dr(1))
 D=max(D,0.)
 K_scatter=abs(E-Ep)<=D
 end function


 end module
