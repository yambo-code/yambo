!
!        Copyright (C) 2000-2022 the YAMBO team
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
subroutine a2y_gkkp_header(i_sp_pol,ncid,grpid)
 !
 use netcdf
 use netcdf_data
 use pars,                  ONLY:DP,SP,cZERO
 use stderr,                ONLY:intc
 use com,                   ONLY:msg
 use interfaces,            ONLY:ELPH_alloc
 use R_lattice,             ONLY:nkibz,nkbz,nqibz,nqbz,k_pt,k_sstar,q_pt,q_sstar
 use D_lattice,             ONLY:n_atoms
 use ELPH,                  ONLY:ph_modes,elph_nb,elph_bands,l_GKKP_hosts_bare_dV,l_GKKP_hosts_DW,&
&                                elph_nQ,elph_nk_bz,elph_grids_are_expanded,elph_use_q_grid,&
&                                PH_qpt,PH_kpt_bz,GKKP,PH_freqs_sq
 !
#include<memory.h>
 !
 integer,     intent(in)  :: i_sp_pol,ncid
 integer,     intent(out) :: grpid
 !
 ! Work Space
 !
 integer   :: varid,nk_gkkp,nq_gkkp,netcdf_error
 !
 l_GKKP_hosts_bare_dV=.false.
 l_GKKP_hosts_DW     =.false.
 elph_use_q_grid     =.true.
 !
 ! global dimensions
 ! TODO: add checks vs WFK file
 !
 !n_atoms     = netcdf_get_dimension(ncid, "natom")
 ph_modes    = netcdf_get_dimension(ncid, "natom3")
 ! 
 nkibz = netcdf_get_dimension(ncid, "gstore_nkibz")
 nkbz  = netcdf_get_dimension(ncid, "gstore_nkbz" )
 nqibz = netcdf_get_dimension(ncid, "gstore_nqibz")
 nqbz  = netcdf_get_dimension(ncid, "gstore_nqbz" )
 !
 ! dimensions specific of gkk_spin group
 !
 netcdf_error = nf90_inq_grp_ncid(ncid, "gqk_spin"//trim(intc(i_sp_pol)), grpid)
 !
 elph_nb    = netcdf_get_dimension(grpid, "nb")
 elph_nk_bz = netcdf_get_dimension(grpid, "glob_nk")
 elph_nQ    = netcdf_get_dimension(grpid, "glob_nq")
 !
 elph_bands=(/1,elph_nb/)
 !
 call msg('s','ELPH dims',(/elph_bands,ph_modes,elph_nk_bz,elph_nQ/))
 !
 call ELPH_alloc("PHONONS dV LEVELS",GKKP=GKKP)
 !
 ! K-points
 !
 call msg('s','ELPH K-points mesh (BZ) ...')
 call import_kpoints_group()
 call msg('l','done')
 !
 ! Q-points
 !
 call msg('s','ELPH Q-points mesh (IBZ) ...')
 call import_qpoints_group()
 call msg('l','done')
 !
 call import_ph_frequencies()
 !
 contains
  !
  ! This is a duplication, to be fixed
  !
  ! K-points (convert to new units)
  !===========================================================
  subroutine import_kpoints_group()
   use pars,                 only : pi
   use R_lattice,            only : b
   use D_lattice,            only : alat
   implicit none
   integer :: ik
   real(DP), allocatable :: gstore_kbz(:,:)
   real(DP), allocatable :: abi_kibz(:,:)
   integer,  allocatable :: abi_kbz2ibz(:,:)
   !
   ! k-points of gstore, for now always in the BZ
   !==============================================
   allocate(gstore_kbz(3, elph_nk_bz))
   !
   varid = netcdf_inquire_varid(ncid, "gstore_kbz")
   netcdf_error = nf90_get_var(ncid, varid, gstore_kbz)
   call netcdf_check(ncid,netcdf_error,varid)
   !
   do ik = 1,elph_nk_bz
     PH_kpt_bz(ik,:)=matmul(transpose(b),gstore_kbz(:,ik))*alat(:)/2.0_SP/pi
   enddo
   !
   deallocate(gstore_kbz)
   !
   ! k-points in the IBZ
   !=====================
   allocate(abi_kibz(3, nkibz))
   !
   varid = netcdf_inquire_varid(ncid, "reduced_coordinates_of_kpoints")
   netcdf_error = nf90_get_var(ncid, varid, abi_kibz)
   call netcdf_check(ncid,netcdf_error,varid)
   !
   allocate(k_pt(nkibz,3) )
   do ik = 1,nkibz
     k_pt(ik,:)=matmul(transpose(b),abi_kibz(:,ik))*alat(:)/2.0_SP/pi
   enddo
   !
   deallocate(abi_kibz)
   !
   ! symmetries to move from the BZ to IBZ
   !=======================================
   allocate(abi_kbz2ibz(6, nkbz))
   !
   ! Here I will have to load the k-points symmetries, etc ...
   varid = netcdf_inquire_varid(ncid, "gstore_kbz2ibz")
   netcdf_error = nf90_get_var(ncid, varid, abi_kbz2ibz)
   call netcdf_check(ncid,netcdf_error,varid)
   !
   allocate(k_sstar(nkbz,2) )
   do ik = 1,nkbz
     k_sstar(ik,:)=abi_kbz2ibz(1:2,ik)
   enddo
   !
   deallocate(abi_kbz2ibz)
   !
  end subroutine import_kpoints_group
  !
  ! Q-points (convert to new units)
  !=================================
  subroutine import_qpoints_group()
   use pars,                 only : pi
   use R_lattice,            only : b
   use D_lattice,            only : alat
   implicit none
   integer :: iq
   real(DP), allocatable :: gstore_q(:,:)
   real(DP), allocatable :: abi_qibz(:,:)
   integer,  allocatable :: abi_qbz2ibz(:,:)
   !
   allocate(gstore_q(3, elph_nQ))
   !
   if(elph_nQ==nqibz) varid = netcdf_inquire_varid(ncid, "gstore_qibz")
   if(elph_nQ==nqbz ) varid = netcdf_inquire_varid(ncid, "gstore_qbz")
   netcdf_error = nf90_get_var(ncid, varid, gstore_q)
   call netcdf_check(ncid,netcdf_error,varid)
   !
   ! warning: here I et a -q
   call warning(" setting PH_qpt=-qpt_abi")
   do iq = 1,elph_nQ
     PH_qpt(iq,:)=-matmul(transpose(b),gstore_q(:,iq))*alat(:)/2.0_SP/pi
   enddo
   !
   deallocate(gstore_q)
   !
   ! q-points in the IBZ
   !=====================
   allocate(abi_qibz(3, nqibz))
   !
   varid = netcdf_inquire_varid(ncid, "reduced_coordinates_of_kpoints")
   netcdf_error = nf90_get_var(ncid, varid, abi_qibz)
   call netcdf_check(ncid,netcdf_error,varid)
   !
   allocate(q_pt(nqibz,3) )
   do iq = 1,nqibz
     q_pt(iq,:)=matmul(transpose(b),abi_qibz(:,iq))*alat(:)/2.0_SP/pi
   enddo
   !
   deallocate(abi_qibz)
   !
   ! symmetries to move from the BZ to IBZ
   !=======================================
   allocate(abi_qbz2ibz(6, nqbz))
   !
   ! Here I will have to load the k-points symmetries, etc ...
   varid = netcdf_inquire_varid(ncid, "gstore_qbz2ibz")
   netcdf_error = nf90_get_var(ncid, varid, abi_qbz2ibz)
   call netcdf_check(ncid,netcdf_error,varid)
   !
   allocate(q_sstar(nqbz,2) )
   do iq = 1,nqbz
     q_sstar(iq,:)=abi_qbz2ibz(1:2,iq)
   enddo
   !
   deallocate(abi_qbz2ibz)
   !
  end subroutine import_qpoints_group
  !
  ! PH_frequencies
  !=================
  subroutine import_ph_frequencies()
   use pars,                 only : pi
   use R_lattice,            only : b
   use D_lattice,            only : alat
   implicit none
   integer :: iq,iqibz
   real(DP), allocatable :: phfreqs_ibz(:,:)
   !
   allocate(phfreqs_ibz(ph_modes,nqibz))
   !
   varid = netcdf_inquire_varid(ncid, "phfreqs_ibz")
   netcdf_error = nf90_get_var(ncid, varid, phfreqs_ibz)
   call netcdf_check(ncid,netcdf_error,varid)
   !
   if (elph_nQ==nqbz) then
     do iq = 1,nqbz
       iqibz=q_sstar(iq,1)
       PH_freqs_sq(iq,:)=real(phfreqs_ibz(:,iqibz),SP)**2
     enddo
   else
     do iqibz = 1,nqibz
       PH_freqs_sq(iqibz,:)=real(phfreqs_ibz(:,iqibz),SP)**2
     enddo
   endif
   !
   deallocate(phfreqs_ibz)
   !
  end subroutine import_ph_frequencies
  !
end subroutine a2y_gkkp_header
 !
 !  TO FIX
 ! call io_bulk(ID_frag,VAR="E_K_PLUS_Q"//trim(intc(iq)),VAR_SZ=(/elph_nb,nkbz,1/) )
 ! call io_bulk(ID_frag,R3=GKKP%E_kpq(iq)%E)
 !
 !
subroutine a2y_gkkp(iq,i_sp_pol,ncid,grpid)
 !
 ! Reads and returns g_kkp matrix elements for each q point
 !
 use netcdf
 use netcdf_data
 use D_lattice,             ONLY: n_atoms
 use pars,                  ONLY: DP,SP
 use elph,                  ONLY: GKKP,elph_nb,ph_modes,elph_nQ,elph_nk_bz,PH_pol_vector
 !
#include<memory.h>
 !
 integer,     intent(in)  :: iq,i_sp_pol,ncid,grpid
 !
 ! Workspace
 !
 integer    :: varid,netcdf_error,ik,i_modes,ipos(6),isize(6),i_atom,xyz,i_n,i_m
 real(DP), allocatable :: gstore_abinit(:,:,:,:,:,:),pheigvec_cart_ibz(:,:,:,:,:)
 !
 ! (natom3, glob_nk, nb, glob_nq, nb, cplex) 
 !
 allocate(gstore_abinit(2,elph_nb,elph_nb,ph_modes,elph_nk_bz,1))
 !AMBO_ALLOC(gstore_abinit,(2,elph_nb,elph_nb,ph_modes,elph_nQ))
 !
 ipos =(/1,1,1,1,1,iq/)
 isize=(/2,elph_nb,elph_nb,ph_modes,elph_nk_bz,1/)
 !
 varid = netcdf_inquire_varid(grpid, "gvals")
 netcdf_error = nf90_get_var(grpid, varid, gstore_abinit, ipos, isize)
 !
 !call ELPH_alloc("dV")
 !
 ! (/2,ph_modes,elph_nb,elph_nb,nkbz/)
 !
 do ik=1,elph_nk_bz
   do i_modes=1,ph_modes
     ! Here we should invert the band indexes and do the complex conjugate
     ! See eq.(5) of the notes of Fulvio
     ! This operation however is already performed inside the subrtouine
     ! src/el-ph/ELPH_databases_load.F
     !
     ! See comment there, reported also here.
     ! There has been a key error in the definition of the el-ph SE of the BKE (Eq. 30_9_11.18)
     ! where the band indexes have been exchanged. Instead of doing a global change of the notes
     ! and of the code I decided, here, to exchange the indexes:
     !
     do i_n=1,elph_nb
       do i_m=1,elph_nb
         GKKP%dVc(i_modes,i_m,i_n,ik,1)=cmplx(gstore_abinit(1,i_m,i_n,i_modes,ik,1),&
         &                                    gstore_abinit(2,i_m,i_n,i_modes,ik,1),kind=SP)
       enddo
     enddo
   enddo
 enddo
 !
 deallocate(gstore_abinit)
 !
 ! Polarization vectors
 !
 allocate(pheigvec_cart_ibz(2,3,n_atoms,ph_modes,1))
 !
 ipos(1:5) =(/1,1,1,1,iq/)
 isize(1:5)=(/2,3,n_atoms,ph_modes,1/)
 !
 varid = netcdf_inquire_varid(ncid, "pheigvec_cart_ibz")
 !varid = netcdf_inquire_varid(ncid, "phdispl_cart_ibz")
 netcdf_error = nf90_get_var(ncid, varid, pheigvec_cart_ibz, ipos(1:5), isize(1:5))
 !
 do xyz=1,3
   do i_atom=1,n_atoms
     PH_pol_vector(:,i_atom,xyz,iq)=cmplx(pheigvec_cart_ibz(1,xyz,i_atom,:,1),pheigvec_cart_ibz(2,xyz,i_atom,:,1),kind=SP)
   enddo
 enddo
 ! 
 deallocate(pheigvec_cart_ibz)
 !
end subroutine a2y_gkkp
