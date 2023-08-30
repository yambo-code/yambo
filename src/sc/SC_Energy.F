!
! License-Identifier: GPL
!
! Copyright (C) 2010 The Yambo Team
!
! Authors (see AUTHORS file for details): DS
!
subroutine SC_Energy(E_total,E_eigen,E_zero,E_Hartree,E_xc_,E_Fermi,E,k,rho)
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
 use hamiltonian,    ONLY:l_sc_V_is_local,V_hartree_sc,E_reference,&
&                         Hzero,H_rotation,H_ref_bands,H_ref_nbands
 use SC,             ONLY:SC_kind,SC_xc_functional
 !
 implicit none
 !
 type(levels) ::E
 type(bz_samp)::k
 real(SP)     ::E_eigen,E_zero,E_total,E_Hartree,E_xc_,E_Fermi
 real(SP)     ::rho(fft_size)
 !
 ! Work Space
 !
 integer      :: ib,ik,i_sp_pol,i_spin
 real(SP)     :: E_Vxc_rho
 complex(SP)  :: H_tmp(H_ref_bands(1):H_ref_bands(2),H_ref_bands(1):H_ref_bands(2))
 !
 E_Vxc_rho=0._SP
 E_eigen  =0._SP
 E_zero   =0._SP
 E_total  =0._SP
 E_Hartree=0._SP
 E_xc_    =0._SP
 E_total  =0._SP
 !
 if (.not.l_sc_is_libDFT .and. .not.l_sc_hartree .and. .not.l_sc_V_is_local) return
 !
 call V_Hartree(rho,V_hartree_sc)                           ! V_Hartree
 call XC_potential_driver(E,k,SC_kind,SC_xc_functional,1)   ! V_xc
 call XC_potential_driver(E,k,SC_kind,SC_xc_functional,0)   ! E_xc
 !
 ! Bands energy
 !
 do i_sp_pol=1,n_sp_pol
   do ik=1,k%nibz
     do ib=H_ref_bands(1),H_ref_bands(2)
       E_eigen=E_eigen+E%E(ib,ik,i_sp_pol)*E%f(ib,ik,i_sp_pol)*k%weights(ik)
     enddo
   enddo
 enddo
 !
 ! E_zero:
 ! E_zero = sum_i ho_ii
 !
 do i_sp_pol=1,n_sp_pol
   do ik=1,k%nibz
     H_tmp=Hzero(:,:,ik,i_sp_pol)
     call OBS_rotate(H_rotation(:,:,ik,i_sp_pol),H_tmp,H_ref_nbands,i_sp_pol)
     do ib=H_ref_bands(1),H_ref_bands(2)
       E_zero=E_zero+H_tmp(ib,ib)*E%f(ib,ik,i_sp_pol)*k%weights(ik)
     enddo
   enddo
 enddo
 !
 !
 ! Hartree:
 ! E_Hartree= 1/2 \int rho(r) V_hartree(r)= 1/2 \int\int rho(r) v(r-r') rho(r')
 !
 E_Hartree=V_dot_V(fft_size,rho,(0.5_SP*V_hartree_sc(:,1)))
 !
 ! DFT Exc:
 ! E_xc=  \int rho(r) E_xc(r)
 !
 E_xc_=V_dot_V(fft_size,rho,E_xc(:))
 !
 ! DAVIDE (11/06/2015): to be fixed with spin, the term -\int magn(r) B_xc(r) is missing
 do i_spin=1,n_spin
   E_Vxc_rho=E_Vxc_rho-V_dot_V(fft_size,rho,V_xc(:,i_spin))
 enddo
 !
 ! Fermi level contribution to E_total
 !
 E_Fermi=(E%E_Fermi+E_reference%E_Fermi)*nel
 !
 ! E_total =  \sum_v E_v -E_Hartree +(E_xc -\int rho(r) V_xc(r) -\int magn(r) B_xc(r))+E_Fermi
 !
 E_total=E_eigen-E_Hartree+(E_xc_-E_Vxc_rho)+E_Fermi
 !
end subroutine SC_Energy
