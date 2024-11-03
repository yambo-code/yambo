!
!        Copyright (C) 2000-2022 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM, DS
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
!> @brief Calculate QP_Vxc = E_bare - h0 - VHartree (i.e., for GW+U calculations)
!
! @param[in]       E               energy levels
! @param[in]       Xk              k-grid X
! @param[in]       k               k-grid
! @param[in]       Dip             Dipoles
!
! @param[out]      QP_Vxc          DFT xc potential +U term
!
subroutine XCo_from_H_minus_Hbare(E,Xk,k,q,Dip)
 !
 ! Local V_xc (+ Hubbard term if present): 
 ! 
 ! <nk| V_xc +U |nk> = E^KS - <nk| h^0 +V^H |nk>
 !
 ! We need the KS eigenvalues, the noninteracting Hamiltonian and the Hartree 
 ! potential written in the QP basis -> we get QP_Vxc to be subtracted in QP equation
 !
 use com,           ONLY:msg 
 use pars,          ONLY:SP,cZERO
 use electrons,     ONLY:levels,spin,n_sp_pol
 use QP_m,          ONLY:QP_Vxc,QP_n_states,QP_table,QP_nk,QP_ng_SH,QP_ng_Vxc
 use R_lattice,     ONLY:bz_samp,ng_vec,qindx_S
 use DIPOLES,       ONLY:DIPOLE_t
 use wave_func,     ONLY:WF
 use hamiltonian,   ONLY:Hzero,V_hartree_sc,H_alloc,H_free,E_reference,H_ref_bands,&
&                        WF_G_max!,WF_Go_indx
 use H_interfaces,  ONLY:V_real_space_to_H
 use parallel_int,  ONLY:PP_redux_wait
 use parallel_m,    ONLY:PAR_IND_QP
 use IO_int,        ONLY:IO_and_Messaging_switch
 use timing_m,      ONLY:timing
 use FFT_m,         ONLY:fft_size
 use parser_m,      ONLY:parser
 !
#include<y_memory.h>
 !
 type(levels) ::E
 type(bz_samp)::Xk,k,q
 type(DIPOLE_t)::Dip
 !
 ! Work Space
 !
 integer     ::i1,ib,ibp,ik,i_sp_pol
 !
 ! Debug
 !
 complex(SP), allocatable :: Vh_test(:,:,:,:)
 logical ::l_write_Vxc
 !
 call parser('WriteVxc',l_write_Vxc) ! only for testing purposes
 !
 if (l_write_Vxc) then
  open(3, file='Hbare_TEST.dat')
  open(4, file='Hplus_TEST.dat')
  open(5, file='Eref_TEST.dat')
  open(6, file='QPVxc_TEST.dat')
  open(7, file='VHartree_TEST.dat')
 endif
 !
 call timing('XCo_from_H_minus_Hbare',OPR="start")
 !
 ! Allocation 
 !
 H_ref_bands(1)=minval(QP_table(:,1))
 H_ref_bands(2)=maxval(QP_table(:,1))
 !
 ! Max G-vectors for FFT-vloc, WF for Hbare and VHartree
 WF_G_max=ng_vec
 !
 ! Allocate only quantities not in real space, because
 ! fft_size might change inside Bare_Hamiltonian_from_Scratch
 ! when calling WF_load with new WF_G_max
 call H_alloc(E,.false.,.false.)
 ! 
 ! The P2 dipoles (needed for Hzero) are already switched on by 
 ! the Hbare logical in DIPOLE_driver
 !
 ! Here we get Hzero in H space and V_Hartree_sc in real space
 !
 call Bare_Hamiltonian_from_Scratch(E,Xk,k,q,Dip)
 !
 ! PARALLEL EFFICIENCY FOR THE FOLLOWING LOOPS NEED TO BE CHECKED!
 ! (may include call to V_real_space_to_H in QP loop but seems wasteful)
 !
 ! First we get Hzero => Hzero+Vhartree in H space
 !
 ![DEBUG>]
 if (l_write_Vxc) then ! Debug: get V_Hartree alone
   !
   allocate(Vh_test(H_ref_bands(1):H_ref_bands(2),H_ref_bands(1):H_ref_bands(2),E%nk,n_sp_pol))
   Vh_test=cZERO
   !
   do i_sp_pol=1,n_sp_pol
    do ik=1,QP_nk
    !
    call V_real_space_to_H(ik,i_sp_pol,Vh_test(:,:,ik,i_sp_pol),WF,'def',V=V_hartree_sc)
    !
    write(7,*) "# Vh    (ik,i_sp_pol) ",ik,i_sp_pol
    do ib=H_ref_bands(1),H_ref_bands(2)
      write(7,*) Vh_test(ib,:,ik,i_sp_pol)
    enddo
    !
    enddo
   enddo
   close(7)
   !
 endif
 ![DEBUG<]
 !
 do i_sp_pol=1,n_sp_pol
  do ik=1,QP_nk
  !
  ![DEBUG>]
  if (l_write_Vxc) then
   write(3,*) "# Ho    (ik,i_sp_pol) ",ik,i_sp_pol
   write(4,*) "# Ho+ VH (ik,i_sp_pol) ",ik,i_sp_pol
   write(5,*) "# E_reference (ik,i_sp_pol) ",ik,i_sp_pol
   do ib=H_ref_bands(1),H_ref_bands(2)
    write(3,*) Hzero(ib,:,ik,i_sp_pol)
    write(5,*) E_reference%E(ib,ik,i_sp_pol)
   enddo
  endif
  ![DEBUG<]
  !
  call V_real_space_to_H(ik,i_sp_pol,Hzero(:,:,ik,i_sp_pol),WF,'def',V=V_hartree_sc)
  !
  ![DEBUG>]
  if (l_write_Vxc) then
   do ib=H_ref_bands(1),H_ref_bands(2)
    write(4,*) Hzero(ib,:,ik,i_sp_pol)
   enddo
  endif
  ![DEBUG<]
  !
  enddo
 enddo
 !  
 ! Then we get E_dft-Hzero-Vhartree in QP space
 !
 do i1=1,QP_n_states
  !
  !if (.not.PAR_IND_QP%element_1D(i1)) cycle
  !
  ib =QP_table(i1,1)
  ibp=QP_table(i1,2)
  ik =QP_table(i1,3)
  i_sp_pol=spin(QP_table(i1,:))
  !
  QP_Vxc(i1)=E_reference%E(ib,ik,i_sp_pol)-Hzero(ib,ibp,ik,i_sp_pol)
  !
  ![DEBUG>]
  if (l_write_Vxc) then
   write(6,*) "#QP_Vxc (ik,i_sp_pol,ib,ibp) ",ik,i_sp_pol,ib,ibp
   write(6,*) QP_Vxc(i1)
  endif
  ![DEBUG<]
  !
 enddo
 !
 !call PP_redux_wait(QP_Vxc)
 !
 ![DEBUG>]
 if (l_write_Vxc) then
  close(3)
  close(4)
  close(5)
  close(6)
  deallocate(Vh_test)
 endif
 ![DEBUG<]
 !
 call H_free()
 !
 call msg('rsn','[XC] H_BARE reconstruction used to calculate xc functional')
 !
 call timing('XCo_from_H_minus_Hbare',OPR="stop")
 !
 return
 !
end subroutine
