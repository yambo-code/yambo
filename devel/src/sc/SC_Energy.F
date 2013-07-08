!
! Copyright (C) 2000-2013 D. Sangalli and the YAMBO team
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
subroutine SC_Energy(E_total,E_Hartree,E_xc_,E,k,rho)
 !
 ! Total energy defined in terms of the KS orbitals
 !
 use pars,           ONLY:SP
 use wrapper,        ONLY:real_V_dot_V
 use drivers,        ONLY:l_sc_contains_lda
 use R_lattice,      ONLY:bz_samp
 use electrons,      ONLY:levels,n_sp_pol,n_spin,nel
 use xc_functionals, ONLY:V_xc,E_xc
 use FFT_m,          ONLY:fft_size
 use SC,             ONLY:local_V,V_hartree_sc,SC_kind,SC_xc_functional,E_bare_Ef
 use IO_m,           ONLY:IO_and_Messaging_switch
 !
 implicit none
 !
 type(levels) ::E
 type(bz_samp)::k
 real(SP)     ::E_total,E_Hartree,E_xc_
 real(SP)     ::rho(fft_size,n_spin)
 !
 ! Work Space
 !
 integer      :: ib,ik,i_spin
 !
 E_total=0.
 E_Hartree=0.
 E_xc_=0.
 !
 if (.not.l_sc_contains_lda.and..not. local_V) return
 !
 call V_Hartree(rho,V_hartree_sc)                           ! V_Hartree
 call IO_and_Messaging_switch("-report -log -io_in -io_out")
 call XC_potential_driver(E,k,SC_kind,SC_xc_functional,1)  ! V_xc
 call XC_potential_driver(E,k,SC_kind,SC_xc_functional,0)  ! E_xc
 call IO_and_Messaging_switch("+report +log +io_in +io_out")
 !
 ! Hartree & XC
 !
 ! E_Hartree= 1/2 \int rho(r) V_hartree(r)= 1/2 \int\int rho(r) v(r-r') rho(r')
 ! E_xc=  \int rho(r) E_xc(r)
 !
 E_Hartree=real_V_dot_V(fft_size,rho,(0.5_SP*V_hartree_sc(:,1)))
 E_xc_=real_V_dot_V(fft_size,rho,E_xc(:))
 !
 ! Fermi level contribution to E_total
 !
 E_total=(E%Efermi(1)+E_bare_Ef)*nel
 !
 do i_spin=1,n_sp_pol
   do ik=1,k%nibz
     do ib=1,E%nbm
       E_total=E_total+E%E(ib,ik,i_spin)*E%f(ib,ik,i_spin)*k%weights(ik)
     enddo
   enddo
   !
   E_total=E_total-real_V_dot_V(fft_size,rho,V_xc(:,i_spin))
   !
 enddo
 !
 ! E_total =  \sum_v E_v -E_Hartree +E_xc -\int rho(r) V_xc(r)
 !
 E_total=E_total-E_Hartree+E_xc_
 !
end subroutine SC_Energy
