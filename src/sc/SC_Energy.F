!
! Copyright (C) 2000-2010 D. Sangalli and the YAMBO team
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
subroutine SC_Energy(E_total,E,k,rho)
 !
 ! Total energy defined in terms of the KS orbitals
 !
 use pars,           ONLY:SP
 use wrapper,        ONLY:real_V_dot_V
 use drivers,        ONLY:l_sc_contains_lda
 use R_lattice,      ONLY:bz_samp
 use electrons,      ONLY:levels,n_sp_pol,n_spin
 use xc_functionals, ONLY:V_xc,E_xc
 use FFT_m,          ONLY:fft_size
 use SC,             ONLY:local_V,V_hartree_sc,SC_kind,SC_xc_functional
 !
 implicit none
 !
 type(levels) ::E
 type(bz_samp)::k
 real(SP)     ::E_total
 real(SP)     ::rho(fft_size,n_spin)
 !
 ! Work Space
 !
 integer      :: ib,ik,i_spin
 !
 if (.not.l_sc_contains_lda.and..not. local_V) return
 if (n_sp_pol ==2 ) return
 !
 E_total=0
 do ik=1,k%nibz
   do i_spin=1,n_sp_pol
     do ib=1,E%nbm
       E_total=E_total+E%E(ib,ik,i_spin)*E%f(ib,ik,i_spin)*k%weights(ik)
     enddo
   enddo
 enddo
 !
 call V_Hartree(rho,V_hartree_sc)                           ! V_Hartree
 call IO_and_Messaging_switch("-report -log -io_in -io_out")
 call XC_potential_driver(E,k,SC_kind,SC_xc_functional,1)  ! V_xc
 call XC_potential_driver(E,k,SC_kind,SC_xc_functional,0)  ! E_xc
 call IO_and_Messaging_switch("+report +log +io_in +io_out")
 !
 E_total=E_total+real_V_dot_V(fft_size,rho,(-0.5_SP*V_hartree_sc(:,1)-V_xc(:,1)+E_xc(:)))
 !
end subroutine SC_Energy
