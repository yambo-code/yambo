!
!        Copyright (C) 2000-2021 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): FP, DS
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
subroutine Bare_Hamiltonian_from_Scratch(E,Xk,k,q,Dip)
 !
 use pars,           ONLY:DP,SP,cZERO,schlen,lchlen
 use electrons,      ONLY:levels,n_sp_pol,n_spin,l_spin_orbit,spin_occ,n_spinor
 use dipoles,        ONLY:DIPOLE_t,compute_P2_dipoles,P_square
 use QP_m,           ONLY:QP_nk,QP_ng_Vxc
 use interfaces,     ONLY:el_density_and_current,el_magnetization,WF_load
 use hamiltonian,    ONLY:Hzero,H_ref_bands,WF_G_max,WF_Go_indx,Hext_nl,Hext_loc,&
&                         V_hartree_sc,rho_n,magn_n,H_alloc_real_space
 use parallel_int,  ONLY:PP_redux_wait,PARALLEL_global_indexes,PARALLEL_WF_distribute,PARALLEL_WF_index
 use parallel_m,    ONLY:PAR_IND_Xk_ibz,PAR_IND_QP,&
&                        PAR_IND_QP_ID,PAR_IND_Q_ibz,PAR_IND_G_b,&
&                        PAR_IND_Q_ibz_ID,n_WF_bands_to_load
 use wave_func,      ONLY:WF
 use R_lattice,      ONLY:bz_samp,nkibz,qindx_S
 use D_lattice,      ONLY:DL_vol
 use timing_m,       ONLY:timing
 use pseudo,         ONLY:becp,pp_is_uspp,qe_atoms_map
 use uspp,           ONLY:nkb,ikb_i=>indv_ijkb0,deeq,deeq_nc
 use uspp_param,     ONLY:nh
 use com,            ONLY:msg,of_open_close
 use ions_base,      ONLY:nat,ityp,ntyp=>nsp
 use vlocal,         ONLY:vloc_dealloc
 use wrapper,        ONLY:Vstar_dot_VV
 use fft_m,          ONLY:fft_size,fft_g_table
 use IO_int,         ONLY:io_control
 use IO_m,           ONLY:NONE,OP_RD_CL,REP,DUMP
 use gvect,          ONLY:ngm
 use stderr,         ONLY:intc
 use parser_m,       ONLY:parser
#if defined _SC
 use drivers,        ONLY:l_sc_run
#endif
#if defined _RT
 use drivers,        ONLY:l_real_time
#endif
#include<dev_defs.h>
#include<y_memory.h>
 !
 type(levels)             :: E ! This is so far not needed by the subroutine 
 type(bz_samp)            :: Xk,k,q
 type(DIPOLE_t)           :: Dip
 !
 ! Work space
 !
 integer     :: ik,ikb,ib1,ib2,i_sp_pol,WFbands(2),ia,it,i1,i2,ia_qe,ia_per_type
 integer     :: i_wf1,i_wf2,io_DIP_err,ID_DIP,i_qp
 complex(SP), allocatable :: vloc_full_r(:)
 character(schlen)  :: DIP_par_scheme
 character(schlen) :: Hsc_file,headings(6),msg_format
 character(lchlen) :: msg_to_dump
 !
 integer, external :: io_DIPOLES
 !
 ! Debug
 !
 logical :: l_scale_fermi,l_write_Hsc
 !
 !open(1, file='TEST_Hbare_real.dat')
 !open(2, file='TEST_Hbare_imag.dat')
 !
 Hzero   =cZERO
 Hext_nl =cZERO
 Hext_loc=cZERO
 !
 l_scale_fermi=.false.
 call parser('BareHScaleFermi',l_scale_fermi)
 !
 l_write_Hsc=.false.  
 call parser('WriteHsc',l_write_Hsc) ! write Hamiltonian on file only for testing purpose 
 !
 if(l_write_Hsc) then
   Hsc_file='H_sc'
   call of_open_close(Hsc_file,'ot')
   call msg('o '//Hsc_file,'#')
   call msg('o '//Hsc_file,'# H-Bare Hamiltonian from scratch ') 
   call msg('o '//Hsc_file,'#')
   headings=(/'is_pol','kpt   ','bnd1  ','bnd2  ','Re[H] ','Img[H]'/)
   call msg('o '//Hsc_file,'#',headings,INDENT=0,USE_TABS=.TRUE.)
   call msg('o '//Hsc_file,'#')
   msg_format='(4I7,2F16.10)'
 endif
 !
 call timing('Bare_Hamiltonian_from_Scratch',OPR='start')
 !
 if (pp_is_uspp) call error("[PPs] USPP not implemented for bare Hamiltonian reconstruction")
 !
 ! Read dipoles
 !
 ! (NB : In the final version this may be done in a different subroutine)
 ! (NB2: Incompatibility of DIP_par_scheme and DIPOLE_IO call in the present version
 !       is the reason why SE parall. over q is turned off in XCo_driver)
 !=====================================================================
 if (.not.compute_P2_dipoles) call error("[DIP] P_square not activated")
 call DIPOLE_dimensions(E,Dip,H_ref_bands,(/0._SP,0._SP,0._SP/))
 !
 DIP_par_scheme="K"
 if(l_sc_run) DIP_par_scheme="SC"

 call DIPOLE_IO(k,E,Dip,'read',io_DIP_err,DIP_par_scheme)
 if (.not.allocated(P_square)) call error("[DIP] P_square not found")
 !
 ! Load WF, compute Vloc and becp and setup FFT
 !
 ! (NB : this turns pp_is_uspp to .true., later on it is restored to .false.)
 ! (NB2: this calls vloc_alloc(), it is deallocated below) 
 ! (NB3: quantities in real space [vlocal and VHartree] require loading all occupied states)
 !=====================================================================
 WFbands=(/1,max(H_ref_bands(2),maxval(E%nbm))/)
 ! 
 ! WF distributed & load
 !=======================
 !
 if (.not.l_sc_run) then
  call PARALLEL_global_indexes(E,k,q,"Self_Energy")
  ! 
  ! Final states (k-q,np)...
  call PARALLEL_WF_distribute(K_index=PAR_IND_Xk_ibz,B_index=PAR_IND_G_b,CLEAN_UP=.TRUE.)
  ! Initial states (k,n \in QP)...
  call PARALLEL_WF_distribute(QP_index=PAR_IND_QP)
  call PARALLEL_WF_index( )
 endif
 !
 call WF_load(WF,QP_ng_Vxc,maxval(qindx_S(:,:,2)),WFbands,(/1,k%nibz/),title='-vloc')
 !
 !****************************************************
 !
 ! FFT of vloc into real space
 !
 ! (NB: grid dimensions are determined by the FFT_setup previously run by WF_load)
 !=====================================================================
 YAMBO_ALLOC(vloc_full_r,(fft_size))
 vloc_full_r=cZERO
 !
 call FFT_vloc_G_to_R(vloc_full_r)
 !
 ! Calculation of Hzero
 !
 ! Hzero = T + Hext = P**2/2. + V_ext^loc + V_ext^nl
 !
 ! Hzero_n,m = P_square(n,m)/2. + int_r [WF%c(r,n)]* Vloc(r) * WF%c(r,m)/2.  + sum_ij [becp%k(i,n)]* D_ij * becp%k(j,m)/2.
 !
 !=======================================================================================================
 !
 do i_sp_pol=1,n_sp_pol
   !
   do ik=1,QP_nk
     !
     !write (*,*) "H_pseudo (ik,i_sp_pol) ",ik,i_sp_pol
     !write (*,*) "H_zero (ik,i_sp_pol) ",ik,i_sp_pol
     !
     Hext_loc=cZERO
     Hext_nl=cZERO
     !
     do ib1=H_ref_bands(1),H_ref_bands(2)
       do ib2=H_ref_bands(1),H_ref_bands(2)
         !
         !
         ! Calculate the local part of Hext ...
         !
         ! This is a sandwich in R-space of vloc(r) with the wave functions
         ! ==================================================================
         i_wf1 = WF%index(ib1,ik,i_sp_pol)
         i_wf2 = WF%index(ib2,ik,i_sp_pol)
         !
         !
         Hext_loc(ib1,ib2) = Hext_loc(ib1,ib2) + &
                             Vstar_dot_VV(fft_size,WF%c(:,1,i_wf1),vloc_full_r,WF%c(:,1,i_wf2))
         !
         if (n_spinor==2) then
           Hext_loc(ib1,ib2) = Hext_loc(ib1,ib2) + &
                             Vstar_dot_VV(fft_size,WF%c(:,2,i_wf1),vloc_full_r,WF%c(:,2,i_wf2))
         endif
         !
         ! Calculate the nonlocal part of Hext ...
         !
         ! NB: Unlike for (i) the dipoles or (ii) the Pseudo_Hamiltonian cases,
         !     here we have to *explicitly* use the nondiagonal deeq matrix to
         !     compute the pseudo Hamiltonian matrix elements since deeq is *not*
         !     already diagonalised and incorporated into the KB projectors.
         !
         ! The external loops over ntyp, nat are adapted from add_vuspsi_k() 
         ! in PW/src/add_vuspsi.f90
         ! =================================================================
         ikb = 0
         do it=1,ntyp 
           !
           if ( nh(it) == 0 ) cycle
           !
           ia_per_type=0
           do ia=1,nat
             !
             if ( ityp(ia) == it ) then
               ia_per_type=ia_per_type+1
               !
               ia_qe=qe_atoms_map(ia_per_type,it)
               !
               !write(*,*) it,na,ia_qe,ikb_i(ia)
               !
               do i1=1,nh(it) ! number of projectors for atom ia which is of species it
                 do i2=1,nh(it) ! deeq is nondiagonal if more than one projector per atom
                   
                   !write(*,*) i1,i2,deeq(i1,i2,ia_qe,i_sp_pol)
                   !
                   if (l_spin_orbit) then
                     !
                     Hext_nl(ib1,ib2) = Hext_nl(ib1,ib2) + &
                                       conjg(becp(ik,i_sp_pol)%k(ikb_i(ia)+i1,ib1)) * &
                                                      deeq_nc(i1,i2,ia_qe,i_sp_pol) * &
                                              becp(ik,i_sp_pol)%k(ikb_i(ia)+i2,ib2)
                     !
                   else
                     !
                     Hext_nl(ib1,ib2) = Hext_nl(ib1,ib2) + &
                                        conjg(becp(ik,i_sp_pol)%k(ikb_i(ia)+i1,ib1))* &
                                                         deeq(i1,i2,ia_qe,i_sp_pol) * &
                                              becp(ik,i_sp_pol)%k(ikb_i(ia)+i2,ib2)
                     !
                   endif
                   !  
                 enddo
                 ikb = ikb +1  
               enddo
               !
             endif
             !
           enddo
           !
         enddo
         !
         ! ikb and nkb are here just for debugging, may be removed
         if (ikb/=nkb) call error("[Pseudo Ham.] Something wrong in counting KB projectors and deeq")
       enddo
     enddo
     !
     if (l_scale_fermi) then
       !
       do ib1=H_ref_bands(1),H_ref_bands(2)
         Hzero(ib1,ib1,ik,i_sp_pol)=-E%E_fermi
       enddo
       !
     endif
     !
     ![DEBUG>]
     !do ib1=H_ref_bands(1),H_ref_bands(2)
      ! write(*,*) "P2= ", P_square(ib1,:,ik,i_sp_pol)/2._SP
      ! write(*,*) "Hl= ", Hext_loc(ib1,:)/2._SP
      ! write(*,*) "Hn= ", Hext_nl(ib1,:)/2._SP
      ! if (l_scale_fermi) write(*,*) "Ef= ", Hzero(ib1,:,ik,i_sp_pol)
      ! write(*,*) ""
     !enddo
     ![DEBUG<]
     !
     ! Fill Hzero
     !
     ! Note: 0.5 factor in Hloc and Hnl is conversion from Ry to Ha
     ! Note: conjg(P2) is taken because P2 has swapped indices as per
     !       comment in DIPOLE_p_matrix_elements
     ! ==============
     !
     Hzero(:,:,ik,i_sp_pol) = Hzero(:,:,ik,i_sp_pol) & 
                              +( conjg(P_square(:,:,ik,i_sp_pol)) + Hext_loc(:,:) + Hext_nl(:,:) )/2._SP
     !
     !
     !write(1,*) "#Re H_bare (ik,i_sp_pol) ",ik,i_sp_pol
     !write(2,*) "#Im H_bare (ik,i_sp_pol) ",ik,i_sp_pol
     if(l_write_Hsc) then
       do ib1=H_ref_bands(1),H_ref_bands(2)
         do ib2=H_ref_bands(1),H_ref_bands(2)
           write(msg_to_dump,msg_format) i_sp_pol,ik,ib1,ib2,&
&             real(Hzero(ib1,ib2,ik,i_sp_pol)),aimag(Hzero(ib1,ib2,ik,i_sp_pol))
           call msg('o '//Hsc_file,msg_to_dump)
         enddo
       enddo
     endif
     !do ib1=H_ref_bands(1),H_ref_bands(2)
     !  write(1,*) real(Hzero(ib1,:,ik,i_sp_pol))
     !  write(2,*) aimag(Hzero(ib1,:,ik,i_sp_pol))
     !enddo
     !
#if defined _RT
     if(l_real_time) then
       if (.not.PAR_IND_WF_k%element_1D(ik)) cycle
     endif
#endif
     !
   enddo
   !
 enddo
 !
 pp_is_uspp=.false. 
 call vloc_dealloc() ! Dealloc here since we couldn't do it in PP_uspp_init 
 YAMBO_FREE(vloc_full_r)
 !
 if(l_write_Hsc) call of_open_close(Hsc_file)
 !close(1)
 !close(2)
 !write(*,*) "End of Bare hamiltonian from scratch "
 !
 !
 ! Now calculating V_Hartree in real space. First density [including all valence states]...
 !
 if ((.not.allocated(rho_n)).or.(size(rho_n)/=fft_size)) call H_alloc_real_space()
 !
 call el_density_and_current(E,Xk,rho=rho_n,bands=(/1,H_ref_bands(2)/))
 !
#if defined _SC
 if(l_sc_run) then
   if(n_spin>1) call el_magnetization(E,Xk,magn_n,bands=(/1,H_ref_bands(2)/))
 endif
#endif
 !
 ! ... then potential
 !
 call V_Hartree(rho_n,V_hartree_sc)
 !
 call timing('Bare_Hamiltonian_from_Scratch',OPR='stop')
 !
end subroutine Bare_Hamiltonian_from_Scratch
