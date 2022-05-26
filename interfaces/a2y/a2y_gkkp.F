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
 use ELPH,                  ONLY:ph_modes,elph_nb,l_GKKP_hosts_bare_dV,l_GKKP_hosts_DW,&
&                                elph_nQ,elph_nk_bz,elph_grids_are_expanded,elph_use_q_grid,&
&                                PH_qpt,PH_kpt_bz
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
 netcdf_error = nf90_inq_grp_ncid(ncid, "gkk_spin"//trim(intc(i_sp_pol)), grpid)
 !
 netcdf_error = nf90_inq_varid(grpid, "nb", varid)
 netcdf_error = nf90_get_var(grpid, varid, elph_nb)
 !
 netcdf_error = nf90_inq_varid(grpid, "natom3", varid)
 netcdf_error = nf90_get_var(grpid, varid, ph_modes)
 !
 netcdf_error = nf90_inq_varid(grpid, "glob_nk", varid)
 netcdf_error = nf90_get_var(grpid, varid, elph_nk_bz)
 !
 netcdf_error = nf90_inq_varid(grpid, "glob_nq", varid)
 netcdf_error = nf90_get_var(grpid, varid, elph_nQ)
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
 call ELPH_alloc("PHONONS dV")
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
   real(SP), allocatable :: reduced_coordinates_of_kpoints(:,:)
   !
   allocate(reduced_coordinates_of_kpoints(3, elph_nk_bz))
   !
   varid = netcdf_inquire_varid(ncid, "reduced_coordinates_of_kpoints")
   netcdf_error = nf90_get_var(ncid, varid, reduced_coordinates_of_kpoints)
   call netcdf_check(ncid,netcdf_error,varid)
   !
   YAMBO_ALLOC(PH_kpt_bz,(elph_nk_bz,3))
   do ik = 1,elph_nk_bz
     PH_kpt_bz(ik,:)=matmul(transpose(b),reduced_coordinates_of_kpoints(:,ik))*alat(:)/2.0_SP/pi
   enddo
   !
   deallocate(reduced_coordinates_of_kpoints)
   !
   ! Here I will have to load the k-points symmetries, etc ...
   !
  end subroutine import_kpoints_group
  !
  !
  ! Q-points (convert to new units)
  !===========================================================
  subroutine import_qpoints_group()
   use pars,                 only : pi
   use R_lattice,            only : b
   use D_lattice,            only : alat
   implicit none
   integer :: iq
   real(SP), allocatable :: reduced_coordinates_of_qpoints(:,:)
   !
   allocate(reduced_coordinates_of_qpoints(3, elph_nQ))
   !
   !varid = netcdf_inquire_varid(ncid, "reduced_coordinates_of_qpoints")
   !netcdf_error = nf90_get_var(ncid, varid, reduced_coordinates_of_qpoints)
   !call netcdf_check(ncid,netcdf_error,varid)
   !
   YAMBO_ALLOC(PH_qpt,(elph_nQ,3))
   !do iq = 1,elph_nQ
   !  PH_kpt_bz(iq,:)=matmul(transpose(b),reduced_coordinates_of_qpoints(:,iq))*alat(:)/2.0_SP/pi
   !enddo
   PH_kpt_bz=0._SP
   !
   deallocate(reduced_coordinates_of_qpoints)
   !
  end subroutine import_qpoints_group
  !
end subroutine a2y_gkkp_header
!
!
subroutine a2y_gkkp(iq,i_sp_pol,ncid,grpid)
 !
 ! Reads and returns g_kkp matrix elements for each q point
 !
 use netcdf
 use netcdf_data
 use pars,                  ONLY: DP,SP
 use elph,                  ONLY: GKKP,elph_nb,ph_modes,elph_nQ,elph_nk_bz
 !
#include<memory.h>
 !
 integer,     intent(in)  :: iq,i_sp_pol,ncid,grpid
 !
 ! Workspace
 !
 integer    :: varid,netcdf_error,ik,i_modes,ipos(6),isize(6)
 real(DP), allocatable :: gkkp_abinit(:,:,:,:,:,:)
 !
 ! (natom3, glob_nk, nb, glob_nq, nb, cplex) 
 !
 allocate(gkkp_abinit(2,elph_nb,1,elph_nb,elph_nk_bz,ph_modes))
 !AMBO_ALLOC(gkkp_abinit,(2,elph_nb,elph_nb,ph_modes,elph_nQ))
 !
 ipos =(/1,1,iq,1,1,1/)
 isize=(/2,elph_nb,1,elph_nb,elph_nk_bz,ph_modes/)
 !
 netcdf_error = nf90_inq_varid(grpid, "vars", varid)
 netcdf_error = nf90_get_var(grpid, varid, gkkp_abinit, ipos, isize)
 !
 !call ELPH_alloc("dV")
 !
 ! (/2,ph_modes,elph_nb,elph_nb,nkbz/)
 !
 do ik=1,elph_nk_bz
   do i_modes=1,ph_modes
     GKKP%dVc(i_modes,:,:,ik,1)=cmplx(gkkp_abinit(1,:,1,:,i_modes,ik),gkkp_abinit(2,:,1,:,ik,i_modes),kind=SP)
     !GKKP%dVc(i_modes,:,:,ik,1)=cmplx(gkkp_abinit(1,:,1,:,i_modes,ik),gkkp_abinit(2,:,1,:,i_modes,ik),kind=SP)
   enddo
 enddo
 !
 deallocate(gkkp_abinit)
 !
end subroutine a2y_gkkp
