!
! License-Identifier: GPL
!
! Copyright (C) 2023 The Yambo Team
!
! Authors (see AUTHORS file for details): FP DS
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
  open(133, file='Hbare_TEST.dat')
  open(134, file='Hplus_TEST.dat')
  open(135, file='Eref_TEST.dat')
  open(136, file='QPVxc_TEST.dat')
  open(137, file='VHartree_TEST.dat')
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
    write(137,*) "# Vh    (ik,i_sp_pol) ",ik,i_sp_pol
    do ib=H_ref_bands(1),H_ref_bands(2)
      write(137,*) Vh_test(ib,:,ik,i_sp_pol)
    enddo
    !
    enddo
   enddo
   close(137)
   !
 endif
 ![DEBUG<]
 !
 do i_sp_pol=1,n_sp_pol
  do ik=1,QP_nk
  !
  ![DEBUG>]
  if (l_write_Vxc) then
   write(133,*) "# Ho    (ik,i_sp_pol) ",ik,i_sp_pol
   write(134,*) "# Ho+ VH (ik,i_sp_pol) ",ik,i_sp_pol
   write(135,*) "# E_reference (ik,i_sp_pol) ",ik,i_sp_pol
   do ib=H_ref_bands(1),H_ref_bands(2)
    write(133,*) Hzero(ib,:,ik,i_sp_pol)
    write(135,*) E_reference%E(ib,ik,i_sp_pol)
   enddo
  endif
  ![DEBUG<]
  !
  call V_real_space_to_H(ik,i_sp_pol,Hzero(:,:,ik,i_sp_pol),WF,'def',V=V_hartree_sc)
  !
  ![DEBUG>]
  if (l_write_Vxc) then
   do ib=H_ref_bands(1),H_ref_bands(2)
    write(134,*) Hzero(ib,:,ik,i_sp_pol)
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
   write(136,*) "#QP_Vxc (ik,i_sp_pol,ib,ibp) ",ik,i_sp_pol,ib,ibp
   write(136,*) QP_Vxc(i1)
  endif
  ![DEBUG<]
  !
 enddo
 !
 !call PP_redux_wait(QP_Vxc)
 !
 ![DEBUG>]
 if (l_write_Vxc) then
  close(133)
  close(134)
  close(135)
  close(136)
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
