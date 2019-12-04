!
!        Copyright (C) 2000-2019 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DS
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
subroutine SC_Energy(E_total,E_Hartree,E_xc_,E,k,rho)
 !
 ! Total energy defined in terms of the KS orbitals
 !
 use pars,           ONLY:SP
 use wrapper,        ONLY:V_dot_V
 use drivers,        ONLY:l_sc_is_libDFT,l_sc_hartree
 use R_lattice,      ONLY:bz_samp
 use electrons,      ONLY:levels,n_sp_pol,n_spin,nel
 use xc_functionals, ONLY:V_xc,E_xc,XC_potential_driver
 use FFT_m,          ONLY:fft_size
 use hamiltonian,    ONLY:l_sc_V_is_local,V_hartree_sc,E_reference
 use SC,             ONLY:SC_kind,SC_xc_functional
 !
 implicit none
 !
 type(levels) ::E
 type(bz_samp)::k
 real(SP)     ::E_total,E_Hartree,E_xc_
 real(SP)     ::rho(fft_size)
 !
 ! Work Space
 !
 integer      :: ib,ik,i_spin
 !
 E_total=  0._SP
 E_Hartree=0._SP
 E_xc_=    0._SP
 !
 if (.not.l_sc_is_libDFT .and. .not.l_sc_hartree .and. .not.l_sc_V_is_local) return
 !
 call V_Hartree(rho,V_hartree_sc)                           ! V_Hartree
 call XC_potential_driver(E,k,SC_kind,SC_xc_functional,1)   ! V_xc
 call XC_potential_driver(E,k,SC_kind,SC_xc_functional,0)   ! E_xc
 !
 ! Hartree & XC
 !
 ! E_Hartree= 1/2 \int rho(r) V_hartree(r)= 1/2 \int\int rho(r) v(r-r') rho(r')
 ! E_xc=  \int rho(r) E_xc(r)
 !
 E_Hartree=V_dot_V(fft_size,rho,(0.5_SP*V_hartree_sc(:,1)))
 E_xc_=V_dot_V(fft_size,rho,E_xc(:))
 !
 ! Fermi level contribution to E_total
 !
 E_total=(E%E_Fermi+E_reference%E_Fermi)*nel
 !
 ! Bands energy
 !
 do i_spin=1,n_sp_pol
   do ik=1,k%nibz
     do ib=1,E%nbm
       E_total=E_total+E%E(ib,ik,i_spin)*E%f(ib,ik,i_spin)*k%weights(ik)
     enddo
   enddo
 enddo
 !
 ! DAVIDE (11/06/2015): to be fixed with spin, the term -\int magn(r) B_xc(r) is missing
 do i_spin=1,n_spin
   E_total=E_total-V_dot_V(fft_size,rho,V_xc(:,i_spin))
 enddo
 !
 ! E_total =  \sum_v E_v -E_Hartree +E_xc -\int rho(r) V_xc(r) -\int magn(r) B_xc(r)
 !
 E_total=E_total-E_Hartree+E_xc_
 !
end subroutine SC_Energy
